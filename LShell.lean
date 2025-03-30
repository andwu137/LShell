import Std.Data

@[extern "enable_raw_mode"]
opaque enableRawMode : IO Unit

@[extern "disable_raw_mode"]
opaque disableRawMode : IO Unit

structure Stdio where
  stdin : IO.FS.Stream
  stdout : IO.FS.Stream
  stderr : IO.FS.Stream

structure ShellEnv where
  user : Option String
  path : String
  home : String
  prompt_command : Option String
  prompt_default : String
  prompt_continuation : String
  prompt_select : String
  extras : Array (String × Option String)

namespace IO
def
bracket
(acquire : IO a)
(release : a → IO Unit)
(action : a → IO b)
: IO b := do
  let resource ← acquire
  try
    action resource
  finally
    release resource
end IO

namespace Char
def
char8_is_print
(i : UInt8)
: Bool := 32 <= i && i <= 126 -- WARN: could be wrong
end Char

namespace LShell
-- TODO: create a shell language
def
get_args
(input : String)
(aliases : Std.HashMap String String)
: Option (String × List String) := do
  let add_to_front (c : Char) := λ
    | [] => [[c]]
    | x :: xs => (c :: x) :: xs

  let rec go (it : List Char) (quoted : Bool) : Option (List (List Char)) :=
    match it with
    | [] => if quoted then none else some [] -- TODO: prevent the shell from running, but do a multiline parse
    | c :: cs =>
      -- TODO: parse: pipe, background
      match c with
      | '"' =>
        go cs (not quoted)
      | ' ' =>
        if quoted
          then add_to_front c <$> go cs quoted
          else List.cons [] <$> go cs quoted
      | '\\' =>
        match cs with
        | [] => if quoted then none else some []
        | c' :: cs' => add_to_front c' <$> go cs' quoted
      | c => add_to_front c <$> go cs quoted

  let split_spaces (str : String) : Option (List String) :=
    List.map String.mk <$> go str.toList False

  split_spaces input >>= λ -- initial
  | [] => none
  | cmd :: rest => -- success
    match aliases.get? cmd with
    | none => (cmd, rest) -- no alias
    | some als => -- alias
      split_spaces als >>= λ
      | [] => match rest with -- empty alias
        | [] => none -- empty rest
        | new_cmd :: new_rest => some (new_cmd, new_rest)
      | new_cmd :: new_rest => some (new_cmd, new_rest ++ rest)

def
parse_line
(line : String)
(aliases : Std.HashMap String String)
: Option (String × Array String) :=
  get_args line.trimLeft aliases >>= λx => x.map id List.toArray

def
shell_cd
(path : List String)
: IO Unit := IO.Process.setCurrentDir (System.mkFilePath path)

def
path_list
(path : String)
: List String := path.split (. == ':') -- WARN: not handling escaped ':'

-- PERF: inefficient walk over paths
-- we even regenerate path names from the ':' delimited string
-- path are stored in a list
def
bin_exists
(cmd : String)
(path : List String)
: IO Bool := do
  let f (x : String) : IO Bool := do
    let p := System.FilePath.mk x
    let p_exists ← p.pathExists
    if !p_exists || !p.isAbsolute then pure False else

    let is_dir ← p.isDir
    if !is_dir then pure False else

    let dirents ← p.readDir
    pure $ dirents.any (λd => d.fileName == cmd)
  List.anyM f path

partial def
get_line
(io : Stdio)
(envp : ShellEnv)
: IO (Option String) := do
  let write (arr : Array UInt8) : IO Unit := io.stdout.write (ByteArray.mk arr)
  let failed (x : String) : IO Unit :=
    /- io.stderr.putStrLn $ "UNHANDLED " ++ x -/
    pure ()

  let curr_path ← IO.Process.getCurrentDir
  io.stdout.putStrLn $ String.join ["\n", curr_path.toString]
  io.stdout.putStr envp.prompt_default

  let mut eof : Bool := False
  let mut line := ""
  let mut cursor_x := 0
  let mut cursor_y := 0
  repeat
    let read_1 ← io.stdin.read 1
    if h : read_1.size <= 0 then eof := True break else
    have hl : 0 < read_1.size := by omega

    match read_1[0]'hl with
    | 0x4 => do -- EOT
      eof := True
      break

    | 0xA => do -- \n
      write #[0xA]
      break

    | 0x7F => do -- DEL
      if not line.isEmpty && cursor_x != 0 then
        let start := line.take (cursor_x - 1)
        let ending := line.drop cursor_x
        if cursor_x == line.length then
          write #[0x8, 0x20, 0x8]
        else
          write #[0x8]
          io.stdout.putStr ending
          write #[0x20]
          write (List.replicate (ending.length + 1) 0x8).toArray
        line := start ++ ending
        cursor_x := cursor_x - 1

    | 0x1B => do -- ESC -- TODO: finish escapes
      let read_1 ← io.stdin.read 1
      if h : read_1.size <= 0 then eof := True break else
      have hl : 0 < read_1.size := by omega
      match read_1[0]'hl with
      | 0x5B => do -- arrow -- TODO: up/down arrow
          let read_1 ← io.stdin.read 1
          if h : read_1.size <= 0 then eof := True break else
          have hl : 0 < read_1.size := by omega
          match read_1[0]'hl with
          | 0x44 => do -- right
            if cursor_x != 0 then do
            cursor_x := cursor_x - 1
            write #[0x1B, 0x5B, 0x44]
          | 0x43 => do -- left
            if cursor_x != line.length then do
            cursor_x := cursor_x + 1
            write #[0x1B, 0x5B, 0x43]
          | c => failed $ toString c
      | c => failed $ toString c

    | ic => do -- TODO: ascii / vt
      if Char.char8_is_print ic then do
          let c := Char.ofUInt8 ic
          io.stdout.putStr $ c.toString
          line := line.push $ c
          cursor_x := cursor_x + 1
      else -- TODO: unicode?
          failed $ toString ic
  pure (if eof then none else some line)

partial def
shell_loop
(io : Stdio)
(envp : ShellEnv)
: IO Unit := do
  let mut aliases: Std.HashMap String String := Std.HashMap.empty
  aliases := aliases.insert "ls" "ls --color=auto"

  repeat
    let mline ← get_line io envp
    match mline with
    | none => break -- EOF
    | some line => do
      if line.trim == "" then continue

      match parse_line line aliases with
      | none => io.stderr.putStrLn "unable to parse"
      -- Builtins
      | some ("cd", args) =>
        match h : args.size with
        | 0 => shell_cd [envp.home]
        | 1 =>
          have hx : 0 < args.size := by omega
          if args[0]'hx == "~" then shell_cd [envp.home] else
          shell_cd [args[0]'hx]
        | _ => io.stderr.putStrLn "cd: too many arguments"
      | some ("exit", args) => break
      -- Binaries
      | some (cmd, args) => do
        let spawn_args := {
          cmd := cmd,
          args := args,
          env := Array.mk [
            ("USER", envp.user),
            ("PATH", envp.path),
            ("HOME", envp.home),
          ] ++ envp.extras,
        }
        let cmd_exists ← bin_exists cmd (path_list envp.path)
        if cmd_exists then
          let child ← IO.Process.spawn spawn_args
          let exitCode ← tryCatch
            child.wait
            (λe => io.stderr.putStrLn e.toString *> pure 0)
        else
          io.stderr.putStrLn (cmd ++ ": command not found")

def
run
: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let stderr ← IO.getStderr

  let env_user ← IO.getEnv "USER"
  let env_path ← IO.getEnv "PATH"
  let env_home ← IO.getEnv "HOME"
  let env_prompt_default ← IO.getEnv "PROMPT_DEFAULT"
  let env_prompt_cont ← IO.getEnv "PROMPT_CONT"
  let env_prompt_select ← IO.getEnv "PROMPT_SELECT"
  let env_prompt_command ← IO.getEnv "PROMPT_COMMAND"
  let env_extras ← BaseIO.toIO $
    Array.mapM
      (λx => IO.getEnv x >>= λy => pure (x, y))
      #[
        "TERM",
        "LS_COLORS",
        "CLICOLOR",
        "CLICOLOR_FORCE",
        "NOCOLOR",
        "FORCE_COLOR"
      ]
  let envp : ShellEnv := {
    user := env_user,
    path := env_path.getD "/usr/bin:/bin",
    home := env_home.getD "/home",
    prompt_command := env_prompt_command,
    prompt_default := env_prompt_default.getD "$ ",
    prompt_continuation := env_prompt_cont.getD "> ",
    prompt_select := env_prompt_select.getD "#? ",
    extras := env_extras.filter (λ(_, x) => x.isSome),
  }

  -- TODO: setup CTRL_c interrupt callback
  IO.bracket enableRawMode (λ_ => disableRawMode) $ λ_ => do
    shell_loop {stdin, stdout, stderr} envp
end LShell

def
main
: IO Unit := do
  LShell.run

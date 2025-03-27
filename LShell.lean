structure ShellEnv where
  user : Option String
  path : String
  home : String

-- TODO: create a shell language
def
get_args
(input : String)
: Option (List String) :=
  let add_to_front (c : Char) (ls : List (List Char)) :=
    match ls with
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
  List.map (λx => String.mk x) <$> go input.toList False

-- TODO: handle EOF
def
parse_line
(line : String)
: Option (String × Array String) :=
  get_args line.trimLeft >>= λxs =>
    match xs with
    | [] => none
    | cmd :: rest => some (cmd, rest.toArray)

def
shell_cd
(path : List String)
: IO Unit := IO.Process.setCurrentDir (System.mkFilePath path)

partial def
shell_loop
(stdin : IO.FS.Stream)
(stdout : IO.FS.Stream)
(stderr : IO.FS.Stream)
(envp : ShellEnv)
: IO Unit := do
  repeat
    let curr_path ← IO.Process.getCurrentDir
    stdout.putStrLn $ String.join ["\n", curr_path.toString]
    stdout.putStr "$ "
    stdout.flush

    let line ← (String.dropRight . 1) <$> stdin.getLine
    if line.trim ≠ "" then
      match parse_line line with
      | none => stdout.putStrLn "unable to parse"
      -- Builtins
      | some ("cd", args) =>
        match h : args.size with
        | 0 => shell_cd [envp.home]
        | 1 =>
          have hx : 0 < args.size := by omega
          if args[0]'hx == "~" then shell_cd [envp.home] else
          shell_cd [args[0]'hx]
        | _ => stderr.putStrLn "cd: too many arguments"
      -- Binaries
      | some (cmd, args) => do
        let file ← IO.FS.Stream.ofHandle <$> IO.FS.Handle.mk "/home/ufedora/personal/playground/lean/shell.out" IO.FS.Mode.append -- TODO: remove the test for stdout redirection
        let spawn_args := {
          cmd := cmd,
          args := args,
          env := Array.mk [
            ("USER", envp.user),
            ("PATH", envp.path),
            ("HOME", envp.home)
          ],
        }
        let child ← IO.Process.spawn spawn_args -- TODO: dont fork before checking if the cmd exists
        let exitCode ← tryCatch (child.wait) (λe => stdout.putStrLn e.toString *> pure 0)

def
main
: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let stderr ← IO.getStderr

  let env_user ← IO.getEnv "USER"
  let env_path ← IO.getEnv "PATH"
  let env_home ← IO.getEnv "HOME"
  let envp := {
    user := env_user,
    path := env_path.getD "/usr/bin:/bin",
    home := env_home.getD "/home"
  }

  shell_loop stdin stdout stderr envp

import Lake
open System Lake DSL

package LShell where
  srcDir := "."

lean_exe lshell where
  root := `LShell

-- Terminal
target terminal.o pkg : FilePath := do
  let oFile := pkg.buildDir / "terminal.o"
  let srcJob ← inputTextFile <| pkg.dir / "terminal.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO oFile srcJob weakArgs #["-fPIC"]

extern_lib libterminal pkg := do
  let terminalO ← terminal.o.fetch
  buildStaticLib (pkg.nativeLibDir / (nameToStaticLib "terminal")) #[terminalO]

-- Lang Scheme
target lshell_lang_scheme pkg : FilePath := do
  let exePath := pkg.buildDir / "lshell_lang_scheme"
  let srcPath := pkg.dir / "lshell_lang_scheme.rkt"
  proc {
    cmd := "raco",
    args := #["exe", "-o", exePath.toString, srcPath.toString]
  }
  inputFile exePath False

-- default
@[default_target]
target defaultBuild pkg : Unit := do
  let _ ← lshell.fetch
  let _ ← lshell_lang_scheme.fetch
  pure (Job.pure ())

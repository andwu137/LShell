import Lake
open System Lake DSL

package LShell where
  srcDir := "."

@[default_target]
lean_exe lshell where
  root := `LShell

target ffi.o pkg : FilePath := do
  let oFile := pkg.buildDir / "terminal.o"
  let srcJob ← inputTextFile <| pkg.dir / "terminal.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO oFile srcJob weakArgs #["-fPIC"]

extern_lib libleanffi pkg := do
  let terminalO ← ffi.o.fetch
  buildStaticLib (pkg.nativeLibDir / (nameToStaticLib "terminal")) #[terminalO]

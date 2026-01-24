import Lake

open Lake DSL

package chain_interop

lean_lib ChainInterop

@[default_target]
lean_exe chain_interop where
  root := `ChainInterop

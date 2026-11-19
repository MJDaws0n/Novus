package codegen

// runtimeEnvpGlobal is the lowered symbol for the std library's private
// __novus_envp global. When that global is present, the POSIX entry stubs
// populate it with the environment pointer supplied by the operating system.
// This lets raw execve wrappers inherit the parent process environment.
const runtimeEnvpGlobal = "_g___novus_envp"

func hasRuntimeEnvpGlobal(mod *IRModule) bool {
	for _, global := range mod.Globals {
		if global.Name == runtimeEnvpGlobal {
			return true
		}
	}
	return false
}

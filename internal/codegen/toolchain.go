package codegen

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

// ---------------------------------------------------------------------------
// Toolchain — assembler + linker invocation for each target
// ---------------------------------------------------------------------------

// Toolchain represents the external programs used to assemble and link.
type Toolchain struct {
	Target     *Target
	BuildDir   string
	AsmFile    string // path to the assembly file
	ObjFile    string // path to the object file
	ExeFile    string // path to the final executable
	Verbose    bool
	NASMPath   string // custom NASM path (auto-downloaded on Windows)
	GoLinkPath string // custom GoLink path (auto-downloaded on Windows)
}

// NewToolchain creates a Toolchain for the given target and build directory.
func NewToolchain(target *Target, buildDir, baseName string) *Toolchain {
	return &Toolchain{
		Target:   target,
		BuildDir: buildDir,
		AsmFile:  filepath.Join(buildDir, baseName+target.FileExtAsm()),
		ObjFile:  filepath.Join(buildDir, baseName+target.FileExtObj()),
		ExeFile:  filepath.Join(buildDir, baseName+target.FileExtExe()),
	}
}

// WriteAssembly writes the assembly string to the .s/.asm file.
func (tc *Toolchain) WriteAssembly(asm string) error {
	return os.WriteFile(tc.AsmFile, []byte(asm), 0644)
}

// Assemble invokes the assembler to produce an object file from the assembly.
func (tc *Toolchain) Assemble() error {
	switch {
	case tc.Target.Arch == Arch_ARM64 || (tc.Target.IsX86Family() && tc.Target.Flavor == GAS):
		return tc.assembleGAS()
	case tc.Target.Flavor == NASM:
		return tc.assembleNASM()
	default:
		return tc.assembleGAS()
	}
}

// Link invokes the linker to produce the final executable.
func (tc *Toolchain) Link() error {
	switch tc.Target.OS {
	case OS_Darwin:
		return tc.linkDarwin()
	case OS_Linux:
		return tc.linkLinux()
	case OS_Windows:
		return tc.linkWindows()
	default:
		return fmt.Errorf("unsupported OS for linking: %s", tc.Target.OS)
	}
}

// ---------------------------------------------------------------------------
// Assembler backends
// ---------------------------------------------------------------------------

func (tc *Toolchain) assembleGAS() error {
	// Try 'as' (GNU assembler) or 'cc -c' (uses the system compiler's assembler).
	// On macOS, 'as' is part of Xcode CLT; on Linux it's GNU binutils.
	var cmd *exec.Cmd

	switch tc.Target.OS {
	case OS_Darwin:
		if tc.Target.Arch == Arch_ARM64 {
			// macOS ARM64 uses clang's integrated assembler.
			cmd = exec.Command("as", "-arch", "arm64", "-o", tc.ObjFile, tc.AsmFile)
		} else {
			cmd = exec.Command("as", "-arch", "x86_64", "-o", tc.ObjFile, tc.AsmFile)
		}
	case OS_Linux:
		switch tc.Target.Arch {
		case Arch_x86:
			cmd = exec.Command("as", "--32", "-o", tc.ObjFile, tc.AsmFile)
		case Arch_ARM64:
			cmd = exec.Command("as", "-o", tc.ObjFile, tc.AsmFile)
		default:
			cmd = exec.Command("as", "--64", "-o", tc.ObjFile, tc.AsmFile)
		}
	default:
		cmd = exec.Command("as", "-o", tc.ObjFile, tc.AsmFile)
	}

	return tc.runCmd(cmd, "assemble")
}

func (tc *Toolchain) assembleNASM() error {
	fmtArg := "win64"
	switch tc.Target.ObjFmt {
	case ObjELF:
		if tc.Target.Arch == Arch_x86 {
			fmtArg = "elf32"
		} else {
			fmtArg = "elf64"
		}
	case ObjMachO:
		fmtArg = "macho64"
	case ObjCOFF:
		fmtArg = "win64"
	}

	nasmBin := "nasm"
	if tc.NASMPath != "" {
		nasmBin = tc.NASMPath
	}

	cmd := exec.Command(nasmBin, "-f", fmtArg, "-o", tc.ObjFile, tc.AsmFile)
	return tc.runCmd(cmd, "assemble (nasm)")
}

// ---------------------------------------------------------------------------
// Linker backends
// ---------------------------------------------------------------------------

func (tc *Toolchain) linkDarwin() error {
	// On macOS, use ld with the correct arguments.
	// The linker requires the macOS SDK sysroot.
	sdkPath, err := findMacOSSDK()
	if err != nil {
		// Try without sysroot as fallback.
		sdkPath = ""
	}

	args := []string{
		"-o", tc.ExeFile,
		"-e", tc.Target.EntryPoint,
	}

	if tc.Target.Arch == Arch_ARM64 {
		args = append(args, "-arch", "arm64")
	} else {
		args = append(args, "-arch", "x86_64")
	}

	if sdkPath != "" {
		args = append(args, "-L"+sdkPath+"/usr/lib", "-lSystem")
	} else {
		args = append(args, "-lSystem")
	}

	args = append(args, tc.ObjFile)

	cmd := exec.Command("ld", args...)
	return tc.runCmd(cmd, "link")
}

func (tc *Toolchain) linkLinux() error {
	switch tc.Target.Arch {
	case Arch_x86:
		cmd := exec.Command("ld", "-m", "elf_i386", "-o", tc.ExeFile, tc.ObjFile)
		return tc.runCmd(cmd, "link")
	case Arch_ARM64:
		cmd := exec.Command("ld", "-o", tc.ExeFile, tc.ObjFile)
		return tc.runCmd(cmd, "link")
	default:
		cmd := exec.Command("ld", "-o", tc.ExeFile, tc.ObjFile)
		return tc.runCmd(cmd, "link")
	}
}

func (tc *Toolchain) linkWindows() error {
	// Try GoLink first (common in hobby compiler setups), then MSVC link.
	golinkBin := ""
	if tc.GoLinkPath != "" {
		golinkBin = tc.GoLinkPath
	} else if p, err := exec.LookPath("golink"); err == nil {
		golinkBin = p
	}

	if golinkBin != "" {
		cmd := exec.Command(golinkBin, "/entry", "main", "/console",
			tc.ObjFile,
			"kernel32.dll", "user32.dll", "gdi32.dll", "msvcrt.dll")
		return tc.runCmd(cmd, "link (golink)")
	}

	// Try MSVC link.exe.
	link, err := exec.LookPath("link")
	if err == nil {
		cmd := exec.Command(link,
			"/ENTRY:main",
			"/SUBSYSTEM:CONSOLE",
			fmt.Sprintf("/OUT:%s", tc.ExeFile),
			tc.ObjFile,
			"kernel32.lib", "user32.lib", "gdi32.lib", "msvcrt.lib",
		)
		return tc.runCmd(cmd, "link (msvc)")
	}

	return fmt.Errorf("no suitable linker found for Windows (tried golink, link.exe)")
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

func (tc *Toolchain) runCmd(cmd *exec.Cmd, stage string) error {
	if tc.Verbose {
		fmt.Printf("[toolchain] %s: %s\n", stage, strings.Join(cmd.Args, " "))
	}

	var stderr strings.Builder
	cmd.Stderr = &stderr
	cmd.Stdout = os.Stdout

	err := cmd.Run()
	if err != nil {
		return fmt.Errorf("%s failed: %v\n%s", stage, err, stderr.String())
	}
	return nil
}

func findMacOSSDK() (string, error) {
	if runtime.GOOS != "darwin" {
		return "", fmt.Errorf("not on macOS")
	}
	cmd := exec.Command("xcrun", "--show-sdk-path")
	out, err := cmd.Output()
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(out)), nil
}

// DetectToolchain checks whether the required external tools are available
// for the given target and returns a list of missing tools.
func DetectToolchain(target *Target) []string {
	return DetectToolchainWithPaths(target, "", "")
}

// DetectToolchainWithPaths checks for tools using custom paths for NASM/GoLink.
func DetectToolchainWithPaths(target *Target, nasmPath, golinkPath string) []string {
	var missing []string

	switch {
	case target.Flavor == NASM:
		if nasmPath != "" {
			if _, err := os.Stat(nasmPath); err != nil {
				missing = append(missing, "nasm")
			}
		} else if _, err := exec.LookPath("nasm"); err != nil {
			missing = append(missing, "nasm")
		}
	default:
		if _, err := exec.LookPath("as"); err != nil {
			// Try cc as fallback.
			if _, err := exec.LookPath("cc"); err != nil {
				missing = append(missing, "as (assembler)")
			}
		}
	}

	switch target.OS {
	case OS_Darwin:
		if _, err := exec.LookPath("ld"); err != nil {
			missing = append(missing, "ld (linker)")
		}
	case OS_Linux:
		if _, err := exec.LookPath("ld"); err != nil {
			missing = append(missing, "ld (linker)")
		}
	case OS_Windows:
		hasLinker := false
		if golinkPath != "" {
			if _, err := os.Stat(golinkPath); err == nil {
				hasLinker = true
			}
		}
		if !hasLinker {
			for _, l := range []string{"golink", "link"} {
				if _, err := exec.LookPath(l); err == nil {
					hasLinker = true
					break
				}
			}
		}
		if !hasLinker {
			missing = append(missing, "golink or link.exe (linker)")
		}
	}

	return missing
}

package codegen

import (
	"fmt"
	"runtime"
)

// ---------------------------------------------------------------------------
// OS / Architecture / Target enums
// ---------------------------------------------------------------------------

// OS represents a target operating system.
type OS int

const (
	OS_Linux  OS = iota
	OS_Darwin    // macOS
	OS_Windows
)

func (o OS) String() string {
	switch o {
	case OS_Linux:
		return "linux"
	case OS_Darwin:
		return "darwin"
	case OS_Windows:
		return "windows"
	default:
		return "unknown"
	}
}

// Arch represents a target CPU architecture.
type Arch int

const (
	Arch_x86_64 Arch = iota
	Arch_x86         // 32-bit x86
	Arch_ARM64       // AArch64
)

func (a Arch) String() string {
	switch a {
	case Arch_x86_64:
		return "x86_64"
	case Arch_x86:
		return "x86"
	case Arch_ARM64:
		return "arm64"
	default:
		return "unknown"
	}
}

// AsmFlavor controls whether we emit GAS (AT&T) or NASM (Intel) syntax.
type AsmFlavor int

const (
	GAS  AsmFlavor = iota // AT&T syntax (used on Linux/macOS with GNU as / clang)
	NASM                  // Intel syntax (used on Windows with NASM)
)

// ObjFormat is the object file format produced by the assembler.
type ObjFormat int

const (
	ObjELF   ObjFormat = iota // Linux
	ObjMachO                  // macOS
	ObjCOFF                   // Windows (PE/COFF)
)

func (f ObjFormat) String() string {
	switch f {
	case ObjELF:
		return "elf64"
	case ObjMachO:
		return "macho64"
	case ObjCOFF:
		return "win64"
	default:
		return "unknown"
	}
}

// ---------------------------------------------------------------------------
// Target — a fully-resolved compilation target
// ---------------------------------------------------------------------------

// Target holds all information about the compilation target: OS, architecture,
// assembly syntax flavour, object format, register names, calling convention
// details, and syscall convention.
type Target struct {
	OS     OS
	Arch   Arch
	Flavor AsmFlavor
	ObjFmt ObjFormat

	// PtrSize is the size of a pointer in bytes (4 or 8).
	PtrSize int

	// StackArgSlotSize is the byte size of each stack-passed argument slot.
	// On x86-64 this equals PtrSize (8).  On ARM64, each push is 16 bytes
	// due to the required 16-byte stack alignment.
	StackArgSlotSize int

	// Registers by role (mapped to physical register names).
	ReturnReg    string   // where function return values go
	StackPointer string   // stack pointer register
	BasePointer  string   // frame pointer register
	SyscallReg   string   // register holding the syscall number
	ArgRegs      []string // registers used to pass function arguments (in order)

	// SyscallArgRegs lists registers for syscall arguments in order.
	SyscallArgRegs []string

	// SyscallInstr is the instruction to invoke a syscall: "syscall" on x86-64 linux/darwin,
	// "int 0x80" on x86 linux, "svc #0x80" on arm64 darwin, "svc #0" on arm64 linux.
	SyscallInstr string

	// SymbolPrefix: macOS Mach-O prepends "_" to global symbols.
	SymbolPrefix string

	// EntryPoint is the linker entry-point symbol (after prefix).
	EntryPoint string

	// SyscallNumOffset is added to the raw syscall number on some systems.
	// macOS x86-64 uses the 0x2000000 class prefix for Unix syscalls.
	SyscallNumOffset int64
}

// HostTarget returns a Target matching the current Go runtime (GOOS/GOARCH).
func HostTarget() (*Target, error) {
	goos := runtime.GOOS
	goarch := runtime.GOARCH
	return ResolveTarget(goos, goarch)
}

// ResolveTarget builds a Target from OS/Arch name strings (same names Go uses).
func ResolveTarget(osName, archName string) (*Target, error) {
	t := &Target{}

	switch osName {
	case "linux":
		t.OS = OS_Linux
	case "darwin":
		t.OS = OS_Darwin
	case "windows":
		t.OS = OS_Windows
	default:
		return nil, fmt.Errorf("unsupported OS: %s", osName)
	}

	switch archName {
	case "amd64", "x86_64":
		t.Arch = Arch_x86_64
	case "386", "x86":
		t.Arch = Arch_x86
	case "arm64", "aarch64":
		t.Arch = Arch_ARM64
	default:
		return nil, fmt.Errorf("unsupported architecture: %s", archName)
	}

	// Fill target-specific details.
	switch t.Arch {
	case Arch_x86_64:
		t.fillX86_64()
	case Arch_x86:
		t.fillX86()
	case Arch_ARM64:
		t.fillARM64()
	}

	// OS-specific overrides.
	switch t.OS {
	case OS_Darwin:
		t.ObjFmt = ObjMachO
		t.SymbolPrefix = "_"
		t.EntryPoint = "_main"
		if t.Arch == Arch_x86_64 {
			t.SyscallNumOffset = 0x2000000 // macOS x86-64 syscall class prefix
		}
	case OS_Linux:
		t.ObjFmt = ObjELF
		t.SymbolPrefix = ""
		t.EntryPoint = "_start"
		t.SyscallNumOffset = 0
	case OS_Windows:
		t.ObjFmt = ObjCOFF
		t.SymbolPrefix = ""
		t.EntryPoint = "main"
		t.SyscallNumOffset = 0
	}

	return t, nil
}

// ---------------------------------------------------------------------------
// Architecture-specific initialization
// ---------------------------------------------------------------------------

func (t *Target) fillX86_64() {
	t.PtrSize = 8
	t.StackArgSlotSize = 8
	t.Flavor = GAS // default; Windows build pipeline will handle NASM differently
	t.ReturnReg = "rax"
	t.StackPointer = "rsp"
	t.BasePointer = "rbp"
	t.SyscallReg = "rax"
	t.SyscallInstr = "syscall"

	switch t.OS {
	case OS_Windows:
		// Microsoft x64 calling convention: rcx, rdx, r8, r9 (then stack)
		t.ArgRegs = []string{"rcx", "rdx", "r8", "r9"}
		t.SyscallArgRegs = []string{"rcx", "rdx", "r8", "r9", "r10"} // Windows doesn't use raw syscall, but ntdll stubs
		t.Flavor = NASM
	default:
		// System V AMD64 ABI (Linux, macOS, FreeBSD)
		t.ArgRegs = []string{"rdi", "rsi", "rdx", "rcx", "r8", "r9"}
		t.SyscallArgRegs = []string{"rdi", "rsi", "rdx", "r10", "r8", "r9"}
	}
}

func (t *Target) fillX86() {
	t.PtrSize = 4
	t.StackArgSlotSize = 4
	t.Flavor = GAS
	t.ReturnReg = "eax"
	t.StackPointer = "esp"
	t.BasePointer = "ebp"
	t.SyscallReg = "eax"
	t.SyscallInstr = "int $0x80"
	// cdecl: all args on stack
	t.ArgRegs = nil
	// Linux x86 32-bit syscall: ebx, ecx, edx, esi, edi, ebp
	t.SyscallArgRegs = []string{"ebx", "ecx", "edx", "esi", "edi", "ebp"}
}

func (t *Target) fillARM64() {
	t.PtrSize = 8
	t.StackArgSlotSize = 16
	t.Flavor = GAS
	t.ReturnReg = "x0"
	t.StackPointer = "sp"
	t.BasePointer = "x29"
	// Syscall conventions differ on ARM64:
	// - Linux AArch64: syscall number in x8, invoke with `svc #0`
	// - macOS (Darwin) ARM64: syscall number in x16, invoke with `svc #0x80`
	switch t.OS {
	case OS_Darwin:
		t.SyscallReg = "x16"
	default:
		t.SyscallReg = "x8"
	}

	// ARM64 AAPCS: x0–x7 for arguments
	t.ArgRegs = []string{"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"}
	t.SyscallArgRegs = []string{"x0", "x1", "x2", "x3", "x4", "x5"}

	switch t.OS {
	case OS_Darwin:
		t.SyscallInstr = "svc #0x80"
	default:
		t.SyscallInstr = "svc #0"
	}
}

// ---------------------------------------------------------------------------
// Helper queries
// ---------------------------------------------------------------------------

// FileExtObj returns the platform object file extension (.o or .obj).
func (t *Target) FileExtObj() string {
	if t.OS == OS_Windows {
		return ".obj"
	}
	return ".o"
}

// FileExtExe returns the platform executable extension ("" or ".exe").
func (t *Target) FileExtExe() string {
	if t.OS == OS_Windows {
		return ".exe"
	}
	return ""
}

// FileExtAsm returns the assembly file extension (.s or .asm).
func (t *Target) FileExtAsm() string {
	if t.Flavor == NASM {
		return ".asm"
	}
	return ".s"
}

// Sym returns a symbol name with the target prefix applied.
func (t *Target) Sym(name string) string {
	return t.SymbolPrefix + name
}

// Is64Bit reports whether the target is a 64-bit architecture.
func (t *Target) Is64Bit() bool {
	return t.PtrSize == 8
}

// IsX86Family reports whether the target is x86 or x86-64.
func (t *Target) IsX86Family() bool {
	return t.Arch == Arch_x86_64 || t.Arch == Arch_x86
}

// OSName returns the OS as a lowercase string matching the #if constant values.
func (t *Target) OSName() string {
	return t.OS.String()
}

// ArchName returns the architecture as a string matching the #if constant values.
// Uses Go-style names: "amd64", "arm64", "x86".
func (t *Target) ArchName() string {
	switch t.Arch {
	case Arch_x86_64:
		return "amd64"
	case Arch_x86:
		return "x86"
	case Arch_ARM64:
		return "arm64"
	default:
		return "unknown"
	}
}

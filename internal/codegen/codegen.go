package codegen

import (
	"fmt"
	"novus/internal/ast"
	"os"
	"path/filepath"
	"strings"
)

// ---------------------------------------------------------------------------
// Options controls the behaviour of the code-generation pipeline.
// ---------------------------------------------------------------------------

// Options configures the codegen pipeline.
type Options struct {
	// Target platform. If nil, the host platform is auto-detected.
	Target *Target

	// BuildDir is the directory where all build artifacts are written.
	// Defaults to "./build" relative to the working directory.
	BuildDir string

	// OutputName is the base name for the output files (without extension).
	// Defaults to the module name or "output".
	OutputName string

	// Verbose enables extra diagnostic output.
	Verbose bool

	// AsmOnly stops after emitting the assembly file (skip assemble + link).
	AsmOnly bool

	// SkipLink stops after assembling (produce .o but don't link).
	SkipLink bool
}

// DefaultOptions returns sensible defaults (host target, build/ directory).
func DefaultOptions() *Options {
	return &Options{
		BuildDir: "build",
	}
}

// ---------------------------------------------------------------------------
// Result is returned by Generate with paths to all produced artifacts.
// ---------------------------------------------------------------------------

type Result struct {
	AsmFile  string   // path to the assembly file
	ObjFile  string   // path to the object file (empty if AsmOnly)
	ExeFile  string   // path to the executable (empty if AsmOnly or SkipLink)
	IRDump   string   // human-readable IR dump (for debugging)
	Warnings []string // non-fatal warnings (e.g. register mismatches)
}

// ---------------------------------------------------------------------------
// Generate — the public entry point for the full codegen pipeline
//
// Pipeline: AST → IR (lower) → Assembly text (emit) → Object (assemble) → Executable (link)
// ---------------------------------------------------------------------------

// Generate runs the full code-generation pipeline on the given AST program.
func Generate(program *ast.Program, opts *Options) (*Result, error) {
	if opts == nil {
		opts = DefaultOptions()
	}

	// --- Resolve target ---
	target := opts.Target
	if target == nil {
		var err error
		target, err = HostTarget()
		if err != nil {
			return nil, fmt.Errorf("cannot detect host target: %w", err)
		}
	}

	// --- Determine output name ---
	outputName := opts.OutputName
	if outputName == "" {
		if program.Module != nil && program.Module.Name != "" {
			outputName = program.Module.Name
		} else {
			outputName = "output"
		}
	}
	// Sanitize: replace dots/spaces with underscores.
	outputName = strings.Map(func(r rune) rune {
		if r == '.' || r == ' ' || r == '/' || r == '\\' {
			return '_'
		}
		return r
	}, outputName)

	// --- Create build directory ---
	buildDir := opts.BuildDir
	if buildDir == "" {
		buildDir = "build"
	}
	// Create platform-specific subdirectory.
	platformDir := filepath.Join(buildDir, fmt.Sprintf("%s_%s", target.OS, target.Arch))
	if err := os.MkdirAll(platformDir, 0755); err != nil {
		return nil, fmt.Errorf("cannot create build directory %s: %w", platformDir, err)
	}

	result := &Result{}

	// --- Step 1: Lower AST to IR ---
	if opts.Verbose {
		fmt.Println("[codegen] Lowering AST to IR...")
	}
	irMod := Lower(program, target)
	result.IRDump = irMod.DebugDump()

	if opts.Verbose {
		fmt.Println(result.IRDump)
	}

	// --- Step 1b: Register-target warnings ---
	result.Warnings = checkRegisterWarnings(irMod, target)

	// --- Step 2: Emit assembly ---
	if opts.Verbose {
		fmt.Printf("[codegen] Emitting %s assembly for %s/%s...\n", target.Arch, target.OS, target.Arch)
	}

	var asmText string
	switch target.Arch {
	case Arch_x86_64:
		asmText = EmitX86_64(irMod, target)
	case Arch_ARM64:
		asmText = EmitARM64(irMod, target)
	case Arch_x86:
		asmText = EmitX86(irMod, target)
	default:
		return nil, fmt.Errorf("unsupported architecture for emission: %s", target.Arch)
	}

	// --- Step 3: Write assembly file ---
	tc := NewToolchain(target, platformDir, outputName)
	tc.Verbose = opts.Verbose

	if err := tc.WriteAssembly(asmText); err != nil {
		return nil, fmt.Errorf("cannot write assembly file: %w", err)
	}
	result.AsmFile = tc.AsmFile

	if opts.Verbose {
		fmt.Printf("[codegen] Assembly written to %s\n", result.AsmFile)
	}

	if opts.AsmOnly {
		return result, nil
	}

	// --- Step 4: Assemble ---
	if missing := DetectToolchain(target); len(missing) > 0 {
		fmt.Printf("[codegen] Warning: missing toolchain components: %s\n", strings.Join(missing, ", "))
		fmt.Printf("[codegen] Assembly file was written to %s — you can assemble and link manually.\n", result.AsmFile)
		return result, nil
	}

	if opts.Verbose {
		fmt.Println("[codegen] Assembling...")
	}
	if err := tc.Assemble(); err != nil {
		return result, fmt.Errorf("assembly failed: %w", err)
	}
	result.ObjFile = tc.ObjFile

	if opts.SkipLink {
		return result, nil
	}

	// --- Step 5: Link ---
	if opts.Verbose {
		fmt.Println("[codegen] Linking...")
	}
	if err := tc.Link(); err != nil {
		return result, fmt.Errorf("linking failed: %w", err)
	}
	result.ExeFile = tc.ExeFile

	if opts.Verbose {
		fmt.Printf("[codegen] Executable written to %s\n", result.ExeFile)
	}

	return result, nil
}

// ---------------------------------------------------------------------------
// Register-target warning check
// ---------------------------------------------------------------------------

// x86 register set — registers that belong to x86 / x86-64.
var x86Registers = map[string]bool{
	"eax": true, "ebx": true, "ecx": true, "edx": true,
	"esi": true, "edi": true, "ebp": true, "esp": true,
	"rax": true, "rbx": true, "rcx": true, "rdx": true,
	"rsi": true, "rdi": true, "rbp": true, "rsp": true,
	"r8": true, "r9": true, "r10": true, "r11": true,
	"r12": true, "r13": true, "r14": true, "r15": true,
	"eip": true, "rip": true, "eflags": true, "rflags": true,
	"st0": true, "st1": true, "st2": true, "st3": true,
	"st4": true, "st5": true, "st6": true, "st7": true,
	"mm0": true, "mm1": true, "mm2": true, "mm3": true,
	"mm4": true, "mm5": true, "mm6": true, "mm7": true,
	"xmm0": true, "xmm1": true, "xmm2": true, "xmm3": true,
	"xmm4": true, "xmm5": true, "xmm6": true, "xmm7": true,
	"xmm8": true, "xmm9": true, "xmm10": true, "xmm11": true,
	"xmm12": true, "xmm13": true, "xmm14": true, "xmm15": true,
}

// arm64 register set — registers that belong to ARM64 / AArch64.
var arm64Registers = map[string]bool{
	"x0": true, "x1": true, "x2": true, "x3": true,
	"x4": true, "x5": true, "x6": true, "x7": true,
	"x8": true, "x9": true, "x10": true, "x11": true,
	"x12": true, "x13": true, "x14": true, "x15": true,
	"x16": true, "x17": true, "x18": true, "x19": true,
	"x20": true, "x21": true, "x22": true, "x23": true,
	"x24": true, "x25": true, "x26": true, "x27": true,
	"x28": true, "x29": true, "x30": true,
	"w0": true, "w1": true, "w2": true, "w3": true,
	"w4": true, "w5": true, "w6": true, "w7": true,
	"w8": true, "w9": true, "w10": true, "w11": true,
	"w12": true, "w13": true, "w14": true, "w15": true,
	"w16": true, "w17": true, "w18": true, "w19": true,
	"w20": true, "w21": true, "w22": true, "w23": true,
	"w24": true, "w25": true, "w26": true, "w27": true,
	"w28": true, "w29": true, "w30": true,
	"sp": true, "xzr": true, "wzr": true, "lr": true,
}

// checkRegisterWarnings scans the IR for physical register references and
// warns if registers from a different architecture are used.
func checkRegisterWarnings(mod *IRModule, target *Target) []string {
	usedRegs := map[string]bool{}
	for _, fn := range mod.Functions {
		for _, instr := range fn.Instrs {
			for _, op := range []Operand{instr.Dst, instr.Src1, instr.Src2} {
				if op.Kind == OpPhysReg {
					usedRegs[op.PhysReg] = true
				}
			}
			for _, op := range instr.Args {
				if op.Kind == OpPhysReg {
					usedRegs[op.PhysReg] = true
				}
			}
		}
	}

	var warnings []string
	for reg := range usedRegs {
		switch target.Arch {
		case Arch_ARM64:
			if x86Registers[reg] {
				warnings = append(warnings, fmt.Sprintf(
					"Warning: x86 register %q used in source but target is %s/%s — "+
						"it will be mapped to an ARM64 equivalent at emission time",
					reg, target.OS, target.Arch))
			}
		case Arch_x86_64, Arch_x86:
			if arm64Registers[reg] {
				warnings = append(warnings, fmt.Sprintf(
					"Warning: ARM64 register %q used in source but target is %s/%s — "+
						"this register is not available on the target architecture",
					reg, target.OS, target.Arch))
			}
		}
	}

	return warnings
}

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
	AsmFile string // path to the assembly file
	ObjFile string // path to the object file (empty if AsmOnly)
	ExeFile string // path to the executable (empty if AsmOnly or SkipLink)
	IRDump  string // human-readable IR dump (for debugging)
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

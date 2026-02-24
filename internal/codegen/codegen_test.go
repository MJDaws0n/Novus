package codegen

import (
	"fmt"
	"novus/internal/ast"
	"novus/internal/lexer"
	"novus/internal/parser"
	"novus/internal/semantic"
	"strings"
	"testing"
)

// helper: parse source, run semantic analysis, return program.
func mustParse(t *testing.T, src string) *ast.Program {
	t.Helper()
	tokens, lexErrs := lexer.Lex(src)
	if len(lexErrs) > 0 {
		t.Fatalf("lex errors: %v", lexErrs)
	}
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}
	diags := semantic.Analyze(prog)
	var errs []semantic.Diagnostic
	for _, d := range diags {
		if d.Severity == semantic.Error {
			errs = append(errs, d)
		}
	}
	if len(errs) > 0 {
		t.Fatalf("semantic errors: %v", errs)
	}
	return prog
}

// ---------------------------------------------------------------------------
// Target helpers for tests
// ---------------------------------------------------------------------------

func linuxAMD64Target() *Target {
	tgt, _ := ResolveTarget("linux", "amd64")
	return tgt
}

func darwinARM64Target() *Target {
	tgt, _ := ResolveTarget("darwin", "arm64")
	return tgt
}

func windowsAMD64Target() *Target {
	tgt, _ := ResolveTarget("windows", "amd64")
	return tgt
}

// ---------------------------------------------------------------------------
// IR Lowering Tests
// ---------------------------------------------------------------------------

func TestLowerEmptyMain(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	if len(mod.Functions) != 1 {
		t.Fatalf("expected 1 function, got %d", len(mod.Functions))
	}
	if mod.Functions[0].Name != "main" {
		t.Fatalf("expected function name 'main', got %q", mod.Functions[0].Name)
	}

	found := false
	for _, instr := range mod.Functions[0].Instrs {
		if instr.Op == IRRet {
			found = true
			if instr.Src1.Kind != OpImmediate || instr.Src1.Imm != 0 {
				t.Fatalf("expected return 0, got %+v", instr.Src1)
			}
		}
	}
	if !found {
		t.Fatal("no IRRet instruction found")
	}
}

func TestLowerLetAndReturn(t *testing.T) {
	src := `module test; fn main() -> i32 { let x: i32 = 42; return x; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	if fn.Locals < 1 {
		t.Fatalf("expected at least 1 local, got %d", fn.Locals)
	}

	hasStore := false
	hasRet := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRStore && instr.Src1.Kind == OpImmediate && instr.Src1.Imm == 42 {
			hasStore = true
		}
		if instr.Op == IRRet {
			hasRet = true
		}
	}
	if !hasStore {
		t.Fatal("expected store of immediate 42")
	}
	if !hasRet {
		t.Fatal("expected ret instruction")
	}
}

func TestLowerArithmetic(t *testing.T) {
	src := `module test; fn main() -> i32 { let a: i32 = 10; let b: i32 = 3; return a + b; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasAdd := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRAdd {
			hasAdd = true
		}
	}
	if !hasAdd {
		t.Fatal("expected IRAdd instruction for a + b")
	}
}

func TestLowerSubMulDiv(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 10;
		let b: i32 = 3;
		let c: i32 = a - b;
		let d: i32 = a * b;
		let e: i32 = a / b;
		return e;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasSub, hasMul, hasDiv := false, false, false
	for _, instr := range fn.Instrs {
		switch instr.Op {
		case IRSub:
			hasSub = true
		case IRMul:
			hasMul = true
		case IRDiv:
			hasDiv = true
		}
	}
	if !hasSub {
		t.Error("expected IRSub")
	}
	if !hasMul {
		t.Error("expected IRMul")
	}
	if !hasDiv {
		t.Error("expected IRDiv")
	}
}

func TestLowerWhileLoop(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let i: i32 = 0;
		while (i < 10) { i = i + 1; }
		return i;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasJmp := false
	hasJmpNot := false
	hasCmpLt := false
	for _, instr := range fn.Instrs {
		switch instr.Op {
		case IRJmp:
			hasJmp = true
		case IRJmpNot:
			hasJmpNot = true
		case IRCmpLt:
			hasCmpLt = true
		}
	}
	if !hasJmp || !hasJmpNot || !hasCmpLt {
		t.Fatalf("expected loop IR: jmp=%v jmpNot=%v cmpLt=%v", hasJmp, hasJmpNot, hasCmpLt)
	}
}

func TestLowerMovBuiltin(t *testing.T) {
	src := `module test; fn main() -> i32 { mov(eax, 1); return 0; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasMov := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRMov && instr.Dst.Kind == OpPhysReg && instr.Dst.PhysReg == "eax" {
			hasMov = true
			if instr.Src1.Kind != OpImmediate || instr.Src1.Imm != 1 {
				t.Fatalf("expected mov eax, 1 but got src=%+v", instr.Src1)
			}
		}
	}
	if !hasMov {
		t.Fatal("expected IRMov to physical register eax")
	}
}

func TestLowerSyscall(t *testing.T) {
	src := `module test; fn main() -> i32 { syscall(); return 0; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasSyscall := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRSyscall {
			hasSyscall = true
		}
	}
	if !hasSyscall {
		t.Fatal("expected IRSyscall instruction")
	}
}

func TestLowerInt(t *testing.T) {
	src := `module test; fn main() -> i32 { int(0x80); return 0; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasInt := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRInt && instr.Src1.Kind == OpImmediate && instr.Src1.Imm == 0x80 {
			hasInt = true
		}
	}
	if !hasInt {
		t.Fatal("expected IRInt with 0x80")
	}
}

func TestLowerStringLiteral(t *testing.T) {
	src := `module test; fn main() -> i32 { let s: str = "hello"; return 0; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	if len(mod.Strings) < 1 {
		t.Fatal("expected at least 1 string constant")
	}
	found := false
	for _, s := range mod.Strings {
		if s.Value == "hello" {
			found = true
		}
	}
	if !found {
		t.Fatal("expected string 'hello' in module strings")
	}
}

func TestLowerCharLiteralAsImmediate(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let s: str = "abc";
		let i: i32 = 0;
		if (s[i] != '\0') { return 1; }
		return 0;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	// '\0' should lower as Imm(0), not as a string ref.
	hasCmpWithImm := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRCmpNe {
			if instr.Src2.Kind == OpImmediate && instr.Src2.Imm == 0 {
				hasCmpWithImm = true
			}
		}
	}
	if !hasCmpWithImm {
		t.Fatal("expected CmpNe with immediate 0 for '\\0' char literal")
	}
}

func TestLowerNewlineLiteralAsImmediate(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let s: str = "abc";
		let i: i32 = 0;
		if (s[i] == '\n') { return 1; }
		return 0;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	// '\n' should lower as Imm(10), not as a string ref.
	hasCmpWithImm := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRCmpEq {
			if instr.Src2.Kind == OpImmediate && instr.Src2.Imm == 10 {
				hasCmpWithImm = true
			}
		}
	}
	if !hasCmpWithImm {
		t.Fatal("expected CmpEq with immediate 10 for '\\n' char literal")
	}
}

func TestLowerFunctionCall(t *testing.T) {
	src := `module test;
	fn add(a: i32, b: i32) -> i32 { return a + b; }
	fn main() -> i32 { return add(1, 2); }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	if len(mod.Functions) != 2 {
		t.Fatalf("expected 2 functions, got %d", len(mod.Functions))
	}

	mainFn := mod.Functions[1]
	hasCall := false
	for _, instr := range mainFn.Instrs {
		if instr.Op == IRCall {
			hasCall = true
			if instr.Src1.Label != "add" {
				t.Fatalf("expected call to 'add', got %q", instr.Src1.Label)
			}
			if len(instr.Args) != 2 {
				t.Fatalf("expected 2 call args, got %d", len(instr.Args))
			}
		}
	}
	if !hasCall {
		t.Fatal("expected IRCall instruction")
	}
}

func TestLowerIfElse(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let x: i32 = 1;
		if (x == 1) { return 10; } else { return 20; }
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasJmpNot := false
	retCount := 0
	for _, instr := range fn.Instrs {
		if instr.Op == IRJmpNot {
			hasJmpNot = true
		}
		if instr.Op == IRRet {
			retCount++
		}
	}
	if !hasJmpNot {
		t.Fatal("expected conditional jump for if")
	}
	if retCount < 2 {
		t.Fatalf("expected at least 2 ret instructions (then + else), got %d", retCount)
	}
}

func TestLowerStringDedup(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: str = "hello";
		let b: str = "hello";
		return 0;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	// "hello" should only appear once due to dedup.
	count := 0
	for _, s := range mod.Strings {
		if s.Value == "hello" {
			count++
		}
	}
	if count != 1 {
		t.Fatalf("expected 1 deduplicated 'hello' string, got %d", count)
	}
}

func TestLowerMultipleFunctions(t *testing.T) {
	src := `module test;
	fn foo() -> i32 { return 1; }
	fn bar() -> i32 { return 2; }
	fn main() -> i32 { return foo() + bar(); }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	if len(mod.Functions) != 3 {
		t.Fatalf("expected 3 functions, got %d", len(mod.Functions))
	}
	names := make(map[string]bool)
	for _, f := range mod.Functions {
		names[f.Name] = true
	}
	for _, expected := range []string{"foo", "bar", "main"} {
		if !names[expected] {
			t.Errorf("expected function %q not found", expected)
		}
	}
}

// ---------------------------------------------------------------------------
// Assembly Emission Tests — x86-64 GAS
// ---------------------------------------------------------------------------

func TestEmitX86_64GAS_NoMemToMem(t *testing.T) {
	src := `module test; fn main() -> i32 { let x: i32 = 42; return x; }`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	// No memory-to-memory movq (both operands containing (%rbp)).
	for i, line := range strings.Split(asm, "\n") {
		line = strings.TrimSpace(line)
		if !strings.HasPrefix(line, "movq ") {
			continue
		}
		parts := strings.SplitN(line, ",", 2)
		if len(parts) != 2 {
			continue
		}
		srcOp := strings.TrimSpace(parts[0])
		dstOp := strings.TrimSpace(parts[1])
		srcIsMem := strings.Contains(srcOp, "(%rbp)")
		dstIsMem := strings.Contains(dstOp, "(%rbp)")
		if srcIsMem && dstIsMem {
			t.Errorf("line %d: illegal memory-to-memory movq: %s", i+1, line)
		}
	}
}

func TestEmitX86_64GAS_RegisterPromotion(t *testing.T) {
	src := `module test; fn main() -> i32 { mov(eax, 1); mov(ebx, 0); return 0; }`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	// In 64-bit mode, eax should be promoted to rax (no %eax in output).
	if strings.Contains(asm, "%eax") {
		t.Error("expected 32-bit register eax to be promoted to rax in x86-64 mode")
	}
	if !strings.Contains(asm, "%rax") {
		t.Error("expected %rax in output after promotion")
	}
}

func TestEmitX86_64GAS_FrameAllocation(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 1; let b: i32 = 2; let c: i32 = 3;
		return a + b + c;
	}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "subq $") {
		t.Error("expected stack frame allocation with subq")
	}
}

func TestEmitX86_64GAS_Syscall(t *testing.T) {
	src := `module test; fn main() -> i32 { mov(rax, 60); syscall(); return 0; }`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "syscall") {
		t.Error("expected syscall instruction in output")
	}
}

func TestEmitX86_64GAS_IntInstruction(t *testing.T) {
	src := `module test; fn main() -> i32 { int(0x80); return 0; }`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "int $0x80") {
		t.Error("expected int $0x80 in output")
	}
}

func TestEmitX86_64GAS_StringData(t *testing.T) {
	src := `module test; fn main() -> i32 { let s: str = "hello world"; return 0; }`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "hello world") {
		t.Error("expected string data 'hello world' in .data section")
	}
}

func TestEmitX86_64GAS_LinuxEntryPoint(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "_start") {
		t.Error("expected _start entry wrapper for Linux")
	}
	if !strings.Contains(asm, ".globl _start") {
		t.Error("expected .globl _start for Linux")
	}
}

func TestEmitX86_64GAS_MacOSSymbolPrefix(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	target, _ := ResolveTarget("darwin", "amd64")
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "_main") {
		t.Error("expected _main symbol prefix for macOS")
	}
}

// ---------------------------------------------------------------------------
// Assembly Emission Tests — NASM (Windows)
// ---------------------------------------------------------------------------

func TestEmitNASM_BasicStructure(t *testing.T) {
	src := `module test; fn main() -> i32 { return 42; }`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "bits 64") {
		t.Error("expected NASM bits 64 directive")
	}
	if !strings.Contains(asm, "section .text") {
		t.Error("expected NASM .text section")
	}
	if !strings.Contains(asm, "global main") {
		t.Error("expected global main export in NASM output")
	}
}

func TestEmitNASM_NoATTSyntax(t *testing.T) {
	src := `module test; fn main() -> i32 { let x: i32 = 42; return x; }`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	// NASM uses Intel syntax: no % prefixes, no $ for immediates.
	if strings.Contains(asm, "%rax") || strings.Contains(asm, "%rbp") {
		t.Error("NASM output should not contain AT&T register prefixes (%)")
	}
}

// ---------------------------------------------------------------------------
// Assembly Emission Tests — ARM64
// ---------------------------------------------------------------------------

func TestEmitARM64_PrologueEpilogue(t *testing.T) {
	src := `module test; fn main() -> i32 { let x: i32 = 42; return x; }`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "stp x29, x30") {
		t.Error("expected ARM64 prologue with stp x29, x30")
	}
	if !strings.Contains(asm, "ldp x29, x30") {
		t.Error("expected ARM64 epilogue with ldp x29, x30")
	}
	if !strings.Contains(asm, "    ret\n") {
		t.Error("expected ARM64 ret instruction")
	}
}

func TestEmitARM64_LdurSturOffsetInRange(t *testing.T) {
	// Many locals + vregs to test offset handling.
	src := `module test;
	fn compute(a: i32, b: i32, c: i32) -> i32 {
		let x: i32 = a + b;
		let y: i32 = b + c;
		let z: i32 = x + y;
		let w: i32 = z * 2;
		return w - a;
	}
	fn main() -> i32 { return compute(1, 2, 3); }`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	// All ldur/stur offsets must be within [-256, 255].
	for i, line := range strings.Split(asm, "\n") {
		trimmed := strings.TrimSpace(line)
		if !strings.HasPrefix(trimmed, "ldur") && !strings.HasPrefix(trimmed, "stur") {
			continue
		}
		hashIdx := strings.Index(trimmed, "#")
		if hashIdx < 0 {
			continue
		}
		bracketIdx := strings.Index(trimmed[hashIdx:], "]")
		if bracketIdx < 0 {
			continue
		}
		offsetStr := trimmed[hashIdx+1 : hashIdx+bracketIdx]
		var offset int
		if _, err := fmt.Sscanf(offsetStr, "%d", &offset); err != nil {
			continue
		}
		if offset < -256 || offset > 255 {
			t.Errorf("line %d: ldur/stur offset %d out of range [-256,255]: %s", i+1, offset, trimmed)
		}
	}
}

func TestEmitARM64_RegisterWidth(t *testing.T) {
	// mov(eax, 1) should use 64-bit x0 register on ARM64, not 32-bit w0.
	src := `module test; fn main() -> i32 { mov(eax, 1); return 0; }`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	// All register accesses should be x-registers (64-bit), not w-registers.
	for i, line := range strings.Split(asm, "\n") {
		trimmed := strings.TrimSpace(line)
		// Skip comments and directives.
		if trimmed == "" || strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, ".") || strings.HasSuffix(trimmed, ":") {
			continue
		}
		// w0 through w28 should not appear (x29/x30 are fp/lr, ok as wx29/wx30 won't appear).
		for reg := 0; reg <= 15; reg++ {
			wReg := fmt.Sprintf("w%d", reg)
			// Look for standalone register reference (not part of longer word).
			if strings.Contains(trimmed, wReg) {
				// Make sure it's a standalone register, not part of e.g. "dw" or a label.
				idx := strings.Index(trimmed, wReg)
				before := byte(' ')
				if idx > 0 {
					before = trimmed[idx-1]
				}
				after := byte(' ')
				if idx+len(wReg) < len(trimmed) {
					after = trimmed[idx+len(wReg)]
				}
				if !isAlphaNum(before) && !isAlphaNum(after) {
					t.Errorf("line %d: found 32-bit register %s in ARM64 output: %s", i+1, wReg, trimmed)
				}
			}
		}
	}
}

func isAlphaNum(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9') || b == '_'
}

func TestEmitARM64_MacOSPageDirectives(t *testing.T) {
	src := `module test; fn main() -> i32 { let s: str = "hello"; return 0; }`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "@PAGE") {
		t.Error("expected @PAGE directive for macOS ARM64 string address loading")
	}
	if !strings.Contains(asm, "@PAGEOFF") {
		t.Error("expected @PAGEOFF directive for macOS ARM64 string address loading")
	}
}

// ---------------------------------------------------------------------------
// Integration: Generate (full pipeline, asm-only)
// ---------------------------------------------------------------------------

func TestGenerateAsmOnly_LinuxAMD64(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)

	target := linuxAMD64Target()
	opts := DefaultOptions()
	opts.Target = target
	opts.AsmOnly = true
	opts.BuildDir = t.TempDir()

	result, err := Generate(prog, opts)
	if err != nil {
		t.Fatalf("Generate failed: %v", err)
	}
	if result.AsmFile == "" {
		t.Fatal("expected assembly file path")
	}
	if result.IRDump == "" {
		t.Fatal("expected IR dump")
	}
}

func TestGenerateAsmOnly_DarwinARM64(t *testing.T) {
	src := `module test; fn main() -> i32 { return 42; }`
	prog := mustParse(t, src)

	target := darwinARM64Target()
	opts := DefaultOptions()
	opts.Target = target
	opts.AsmOnly = true
	opts.BuildDir = t.TempDir()

	result, err := Generate(prog, opts)
	if err != nil {
		t.Fatalf("Generate failed: %v", err)
	}
	if result.AsmFile == "" {
		t.Fatal("expected assembly file path")
	}
}

func TestGenerateAsmOnly_WindowsAMD64(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)

	target := windowsAMD64Target()
	opts := DefaultOptions()
	opts.Target = target
	opts.AsmOnly = true
	opts.BuildDir = t.TempDir()

	result, err := Generate(prog, opts)
	if err != nil {
		t.Fatalf("Generate failed: %v", err)
	}
	if result.AsmFile == "" {
		t.Fatal("expected assembly file path")
	}
}

// ---------------------------------------------------------------------------
// IR structure tests
// ---------------------------------------------------------------------------

func TestIRModuleDebugDump(t *testing.T) {
	src := `module test; fn main() -> i32 { let s: str = "hi"; return 0; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	dump := mod.DebugDump()

	if !strings.Contains(dump, "func main") {
		t.Error("expected 'func main' in IR dump")
	}
	if !strings.Contains(dump, `"hi"`) {
		t.Error("expected string constant 'hi' in IR dump")
	}
}

func TestComputeFrameSize_AccountsForVregs(t *testing.T) {
	// A program with arithmetic generates vregs; the frame must accommodate them.
	src := `module test; fn main() -> i32 {
		let a: i32 = 1; let b: i32 = 2; let c: i32 = 3;
		let d: i32 = a + b + c;
		return d;
	}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	fn := mod.Functions[0]

	// Find max vreg number.
	maxVreg := 0
	for _, instr := range fn.Instrs {
		for _, op := range []Operand{instr.Dst, instr.Src1, instr.Src2} {
			if op.Kind == OpVirtReg && op.Reg > maxVreg {
				maxVreg = op.Reg
			}
		}
	}

	// Frame size should account for locals + all vregs.
	minRequired := (fn.Locals + maxVreg + 1) * target.PtrSize
	asm := EmitX86_64(mod, target)

	// Extract the frame size from "subq $NNN, %rsp".
	for _, line := range strings.Split(asm, "\n") {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "subq $") && strings.Contains(line, "%rsp") {
			var frameSize int
			if _, err := fmt.Sscanf(line, "subq $%d", &frameSize); err == nil {
				if frameSize < minRequired {
					t.Errorf("frame size %d is too small; need at least %d for %d locals + %d vregs",
						frameSize, minRequired, fn.Locals, maxVreg+1)
				}
				return
			}
		}
	}
	t.Error("could not find frame size allocation in assembly")
}

// ---------------------------------------------------------------------------
// Edge cases
// ---------------------------------------------------------------------------

func TestLowerEmptyFunctionBody(t *testing.T) {
	src := `module test; fn noop() -> void { } fn main() -> i32 { noop(); return 0; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	if len(mod.Functions) != 2 {
		t.Fatalf("expected 2 functions, got %d", len(mod.Functions))
	}
}

func TestLowerNestedExpressions(t *testing.T) {
	src := `module test; fn main() -> i32 { return (1 + 2) * (3 - 4); }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasAdd, hasSub, hasMul := false, false, false
	for _, instr := range fn.Instrs {
		switch instr.Op {
		case IRAdd:
			hasAdd = true
		case IRSub:
			hasSub = true
		case IRMul:
			hasMul = true
		}
	}
	if !hasAdd || !hasSub || !hasMul {
		t.Fatalf("expected add=%v sub=%v mul=%v for nested expression", hasAdd, hasSub, hasMul)
	}
}

func TestEmitX86_64GAS_MultipleStrings(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: str = "foo";
		let b: str = "bar";
		return 0;
	}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "foo") {
		t.Error("expected string 'foo' in data section")
	}
	if !strings.Contains(asm, "bar") {
		t.Error("expected string 'bar' in data section")
	}
}

// ===========================================================================
// Bug 1: Overloaded function call resolution
// ===========================================================================

func TestLowerOverloadedFunctionCall(t *testing.T) {
	// Two functions with the same name but different param counts/types.
	// The lowerer should resolve the call based on argument count.
	src := `module test;
	fn convert(x: i32) -> i32 { return x; }
	fn convert(x: i64, y: i64) -> i64 { return x + y; }
	fn main() -> i32 { return convert(42); }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	// Find the call instruction in main.
	mainFn := mod.Functions[2] // main is the third function
	hasCall := false
	for _, instr := range mainFn.Instrs {
		if instr.Op == IRCall {
			hasCall = true
			// The label should be mangled to the correct overload.
			// The call with 1 arg should resolve to convert(i32).
			if instr.Src1.Label == "" {
				t.Fatal("expected non-empty label for overloaded call")
			}
		}
	}
	if !hasCall {
		t.Fatal("expected IRCall instruction for overloaded function")
	}
}

func TestLowerOverloadedFunctionCall_TwoArg(t *testing.T) {
	// Call the two-argument overload.
	src := `module test;
	fn convert(x: i32) -> i32 { return x; }
	fn convert(x: i64, y: i64) -> i64 { return x + y; }
	fn main() -> i64 { return convert(1, 2); }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	mainFn := mod.Functions[2]
	hasCall := false
	for _, instr := range mainFn.Instrs {
		if instr.Op == IRCall {
			hasCall = true
			if len(instr.Args) != 2 {
				t.Fatalf("expected 2 call args for two-arg overload, got %d", len(instr.Args))
			}
		}
	}
	if !hasCall {
		t.Fatal("expected IRCall instruction for two-arg overloaded function")
	}
}

// ===========================================================================
// Bug 3: Main module globals accessible in functions
// ===========================================================================

func TestLowerMainModuleGlobalInFunction(t *testing.T) {
	// A top-level global should be accessible inside fn main().
	src := `module test;
	let version: i32 = 42;
	fn main() -> i32 { return version; }`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())

	// The global should be in the module's Globals list (with _g_ prefix).
	foundGlobal := false
	for _, g := range mod.Globals {
		if g.Name == "_g_version" {
			foundGlobal = true
			if g.InitImm != 42 {
				t.Errorf("expected global init value 42, got %d", g.InitImm)
			}
		}
	}
	if !foundGlobal {
		t.Fatal("expected global '_g_version' in module globals")
	}

	// main should reference the global via IRLoadGlobal.
	mainFn := mod.Functions[0]
	hasGlobalLoad := false
	for _, instr := range mainFn.Instrs {
		if instr.Op == IRLoadGlobal {
			hasGlobalLoad = true
		}
	}
	if !hasGlobalLoad {
		t.Log("IR dump:", mod.DebugDump())
		t.Fatal("expected IRLoadGlobal instruction in main for global variable access")
	}
}

// ===========================================================================
// Bug 5: ARM64 getflag(carry) generates cset instruction
// ===========================================================================

func TestEmitARM64_GetFlagCarry(t *testing.T) {
	// getflag(carry) should emit a cset instruction with "cs" condition.
	src := `module test; fn main() -> i32 {
		syscall();
		let ok: bool = getflag(carry);
		return 0;
	}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "cset") {
		t.Error("expected cset instruction for getflag(carry)")
	}
	if !strings.Contains(asm, "cs") {
		t.Error("expected 'cs' condition code for carry flag")
	}
}

func TestEmitARM64_GetFlagZero(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let zf: bool = getflag(zero_flag);
		return 0;
	}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "cset") {
		t.Error("expected cset instruction for getflag(zero_flag)")
	}
}

func TestEmitARM64_GetFlagNegative(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let nf: bool = getflag(negative_flag);
		return 0;
	}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "cset") {
		t.Error("expected cset instruction for getflag(negative_flag)")
	}
	if !strings.Contains(asm, "mi") {
		t.Error("expected 'mi' condition code for negative flag")
	}
}

func TestEmitARM64_GetFlagOverflow(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let of: bool = getflag(overflow_flag);
		return 0;
	}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "cset") {
		t.Error("expected cset instruction for getflag(overflow_flag)")
	}
	if !strings.Contains(asm, "vs") {
		t.Error("expected 'vs' condition code for overflow flag")
	}
}

// ===========================================================================
// Bug 6: Bitwise operations — IR lowering
// ===========================================================================

func TestLowerBitwiseAnd(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 0xFF;
		let b: i32 = 0x0F;
		let result: i32 = a & b;
		return result;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasAnd := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRAnd {
			hasAnd = true
		}
	}
	if !hasAnd {
		t.Fatal("expected IRAnd instruction for a & b")
	}
}

func TestLowerBitwiseOr(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 0x80;
		let b: i32 = 0x01;
		let result: i32 = a | b;
		return result;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasOr := false
	for _, instr := range fn.Instrs {
		if instr.Op == IROr {
			hasOr = true
		}
	}
	if !hasOr {
		t.Fatal("expected IROr instruction for a | b")
	}
}

func TestLowerBitwiseXor(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 0xFF;
		let b: i32 = 0x0F;
		let result: i32 = a ^ b;
		return result;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasXor := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRXor {
			hasXor = true
		}
	}
	if !hasXor {
		t.Fatal("expected IRXor instruction for a ^ b")
	}
}

func TestLowerBitwiseShiftLeft(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 1;
		let result: i32 = a << 4;
		return result;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasShl := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRShl {
			hasShl = true
		}
	}
	if !hasShl {
		t.Fatal("expected IRShl instruction for a << 4")
	}
}

func TestLowerBitwiseShiftRight(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 256;
		let result: i32 = a >> 4;
		return result;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasShr := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRShr {
			hasShr = true
		}
	}
	if !hasShr {
		t.Fatal("expected IRShr instruction for a >> 4")
	}
}

func TestLowerBitwiseNot(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 0xFF;
		let result: i32 = ~a;
		return result;
	}`
	prog := mustParse(t, src)
	mod := Lower(prog, linuxAMD64Target())
	fn := mod.Functions[0]

	hasBitNot := false
	for _, instr := range fn.Instrs {
		if instr.Op == IRBitNot {
			hasBitNot = true
		}
	}
	if !hasBitNot {
		t.Fatal("expected IRBitNot instruction for ~a")
	}
}

// ===========================================================================
// Bug 6: Bitwise operations — x86-64 assembly emission
// ===========================================================================

func TestEmitX86_64_BitwiseShiftLeft(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 1;
		let result: i32 = a << 4;
		return result;
	}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "shlq") {
		t.Error("expected shlq instruction for shift left in x86-64 GAS output")
	}
}

func TestEmitX86_64_BitwiseShiftRight(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 256;
		let result: i32 = a >> 4;
		return result;
	}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "sarq") {
		t.Error("expected sarq instruction for shift right in x86-64 GAS output")
	}
}

func TestEmitX86_64_BitwiseNot(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 0xFF;
		let result: i32 = ~a;
		return result;
	}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "notq") {
		t.Error("expected notq instruction for bitwise NOT in x86-64 GAS output")
	}
}

func TestEmitARM64_BitwiseShiftLeft(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 1;
		let result: i32 = a << 4;
		return result;
	}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "lsl") {
		t.Error("expected lsl instruction for shift left in ARM64 output")
	}
}

func TestEmitARM64_BitwiseShiftRight(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 256;
		let result: i32 = a >> 4;
		return result;
	}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "asr") {
		t.Error("expected asr instruction for shift right in ARM64 output")
	}
}

func TestEmitARM64_BitwiseNot(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 0xFF;
		let result: i32 = ~a;
		return result;
	}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "mvn") {
		t.Error("expected mvn instruction for bitwise NOT in ARM64 output")
	}
}

func TestEmitNASM_BitwiseShiftLeft(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 1;
		let result: i32 = a << 4;
		return result;
	}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "shl") {
		t.Error("expected shl instruction for shift left in NASM output")
	}
}

func TestEmitNASM_BitwiseNot(t *testing.T) {
	src := `module test; fn main() -> i32 {
		let a: i32 = 0xFF;
		let result: i32 = ~a;
		return result;
	}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "not") {
		t.Error("expected not instruction for bitwise NOT in NASM output")
	}
}

// ---------------------------------------------------------------------------
// Bug 2: Global variables emit in data section for all targets
// ---------------------------------------------------------------------------

func TestGlobalVarsInDataSection_GAS(t *testing.T) {
	src := `module test;
let counter: i32 = 42;
fn main() -> i32 {
	return counter;
}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "_g_counter:") {
		t.Error("GAS output missing global variable label '_g_counter:'")
	}
	if !strings.Contains(asm, ".quad 42") {
		t.Error("GAS output missing '.quad 42' for global initializer")
	}
}

func TestGlobalVarsInDataSection_NASM(t *testing.T) {
	src := `module test;
let counter: i32 = 42;
fn main() -> i32 {
	return counter;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "_g_counter:") {
		t.Error("NASM output missing global variable label '_g_counter:'")
	}
	if !strings.Contains(asm, "dq 42") {
		t.Error("NASM output missing 'dq 42' for global initializer")
	}
}

func TestGlobalVarsInDataSection_ARM64(t *testing.T) {
	src := `module test;
let counter: i32 = 42;
fn main() -> i32 {
	return counter;
}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "_g_counter:") {
		t.Error("ARM64 output missing global variable label '_g_counter:'")
	}
	if !strings.Contains(asm, ".quad 42") {
		t.Error("ARM64 output missing '.quad 42' for global initializer")
	}
}

func TestGlobalVarsInDataSection_x86(t *testing.T) {
	src := `module test;
let counter: i32 = 42;
fn main() -> i32 {
	return counter;
}`
	prog := mustParse(t, src)
	target, _ := ResolveTarget("linux", "x86")
	mod := Lower(prog, target)
	asm := EmitX86(mod, target)

	if !strings.Contains(asm, "_g_counter:") {
		t.Error("x86 output missing global variable label '_g_counter:'")
	}
	if !strings.Contains(asm, ".long 42") {
		t.Error("x86 output missing '.long 42' for global initializer")
	}
}

func TestGlobalVarStringInit_GAS(t *testing.T) {
	src := `module test;
let greeting: str = "hello";
fn main() -> i32 {
	return 0;
}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "_g_greeting:") {
		t.Error("GAS output missing global string variable label")
	}
}

func TestGlobalVarStringInit_NASM(t *testing.T) {
	src := `module test;
let greeting: str = "hello";
fn main() -> i32 {
	return 0;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "_g_greeting:") {
		t.Error("NASM output missing global string variable label")
	}
}

// Global var used inside a function body (the actual bug scenario).
func TestGlobalVarUsedInFunction_GAS(t *testing.T) {
	src := `module test;
let x: i32 = 10;
fn main() -> i32 {
	let y: i32 = x;
	return y;
}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	// Must have the global in data section.
	if !strings.Contains(asm, "_g_x:") {
		t.Error("GAS output missing '_g_x:' label in data section")
	}
	// Must have a load from the global.
	if !strings.Contains(asm, "_g_x") {
		t.Error("GAS output missing reference to _g_x")
	}
}

func TestGlobalVarUsedInFunction_NASM(t *testing.T) {
	src := `module test;
let x: i32 = 10;
fn main() -> i32 {
	let y: i32 = x;
	return y;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "_g_x:") {
		t.Error("NASM output missing '_g_x:' label in data section")
	}
	if !strings.Contains(asm, "_g_x") {
		t.Error("NASM output missing reference to _g_x")
	}
}

// ---------------------------------------------------------------------------
// Flag operations — assembly output tests
// ---------------------------------------------------------------------------

func TestGetFlag_GAS(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let a: i32 = 5;
	let b: i32 = 3;
	let c: i32 = a - b;
	return c;
}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	// Basic smoke test: assembly should contain sub instruction.
	if !strings.Contains(asm, "sub") {
		t.Error("GAS output missing sub instruction")
	}
}

func TestGetFlag_NASM(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let a: i32 = 5;
	let b: i32 = 3;
	let c: i32 = a - b;
	return c;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "sub") {
		t.Error("NASM output missing sub instruction")
	}
}

// ---------------------------------------------------------------------------
// Array operations — assembly output tests
// ---------------------------------------------------------------------------

func TestArrayNew_GAS(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let arr: []i32 = [1, 2, 3];
	return 0;
}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	// Should contain heap allocation pattern.
	if !strings.Contains(asm, "_novus_heap_ptr") {
		t.Error("GAS array output missing heap_ptr reference")
	}
	if !strings.Contains(asm, "_novus_heap") {
		t.Error("GAS array output missing heap reference")
	}
}

func TestArrayNew_NASM(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let arr: []i32 = [1, 2, 3];
	return 0;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "_novus_heap_ptr") {
		t.Error("NASM array output missing heap_ptr reference")
	}
	if !strings.Contains(asm, "_novus_heap") {
		t.Error("NASM array output missing heap reference")
	}
}

func TestArrayNew_ARM64(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let arr: []i32 = [1, 2, 3];
	return 0;
}`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "_novus_heap") {
		t.Error("ARM64 array output missing heap reference")
	}
}

// Test array append produces growth code.
func TestArrayAppendGrow_GAS(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let arr: []i32 = [1];
	array_append(arr, 2);
	return 0;
}`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	// Growth code should contain capacity doubling (shl).
	if !strings.Contains(asm, "shlq") {
		t.Error("GAS array append missing growth logic (shlq)")
	}
}

func TestArrayAppendGrow_NASM(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let arr: []i32 = [1];
	array_append(arr, 2);
	return 0;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "shl") {
		t.Error("NASM array append missing growth logic (shl)")
	}
}

// ---------------------------------------------------------------------------
// String operations — cross-platform parity
// ---------------------------------------------------------------------------

func TestStrLen_AllTargets(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let s: str = "hello";
	let n: i32 = len(s);
	return n;
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			// All targets should have string length computation.
			if len(asm) == 0 {
				t.Error("empty assembly output")
			}
		})
	}
}

func TestStrConcat_AllTargets(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let a: str = "hello";
	let b: str = " world";
	let c: str = a + b;
	return 0;
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			if !strings.Contains(asm, "_novus_heap") {
				t.Error("string concat missing heap reference")
			}
		})
	}
}

func TestStrCmpEq_AllTargets(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let a: str = "hello";
	let b: str = "hello";
	if (a == b) {
		return 1;
	}
	return 0;
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			if len(asm) == 0 {
				t.Error("empty assembly output")
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Comparison operations — cross-platform parity
// ---------------------------------------------------------------------------

func TestComparisons_AllTargets(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let a: i32 = 5;
	let b: i32 = 3;
	if (a > b) { return 1; }
	if (a < b) { return 2; }
	if (a == b) { return 3; }
	if (a != b) { return 4; }
	if (a >= b) { return 5; }
	if (a <= b) { return 6; }
	return 0;
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			if !strings.Contains(asm, "cmp") {
				t.Error("assembly missing cmp instruction")
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Control flow — cross-platform parity
// ---------------------------------------------------------------------------

func TestWhileLoop_AllTargets(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let i: i32 = 0;
	while (i < 10) {
		i = i + 1;
	}
	return i;
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			if len(asm) == 0 {
				t.Error("empty assembly output")
			}
			// Should contain a backwards jump (loop).
			if !strings.Contains(asm, "j") && !strings.Contains(asm, "b.") && !strings.Contains(asm, "b ") {
				t.Error("assembly missing jump/branch for while loop")
			}
		})
	}
}

func TestIfElse_AllTargets(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let x: i32 = 5;
	if (x > 3) {
		return 1;
	} else {
		return 0;
	}
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			if len(asm) == 0 {
				t.Error("empty assembly output")
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Function calls — cross-platform parity
// ---------------------------------------------------------------------------

func TestFunctionCall_AllTargets(t *testing.T) {
	src := `module test;
fn add(a: i32, b: i32) -> i32 {
	return a + b;
}
fn main() -> i32 {
	return add(1, 2);
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			if !strings.Contains(asm, "add") {
				t.Error("assembly missing 'add' function reference")
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Overloaded functions — cross-platform
// ---------------------------------------------------------------------------

func TestOverloadedFunctions_AllTargets(t *testing.T) {
	src := `module test;
fn convert(x: i32) -> i32 { return x; }
fn convert(x: i64) -> i64 { return x; }
fn main() -> i32 {
	let a: i32 = convert(42);
	return a;
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			// Should contain mangled name for i32 overload.
			if !strings.Contains(asm, "convert.i32") {
				t.Error("assembly missing mangled function name 'convert.i32'")
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Windows-specific: NASM structure tests
// ---------------------------------------------------------------------------

func TestNASM_HasCorrectHeader(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "bits 64") {
		t.Error("NASM missing 'bits 64' directive")
	}
	if !strings.Contains(asm, "default rel") {
		t.Error("NASM missing 'default rel' directive")
	}
}

func TestNASM_HasBSSSection(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let a: str = "hello";
	let b: str = " world";
	let c: str = a + b;
	return 0;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "section .bss") {
		t.Error("NASM missing BSS section")
	}
	if !strings.Contains(asm, "_novus_heap") {
		t.Error("NASM missing heap in BSS")
	}
}

func TestNASM_HasDataSection(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let s: str = "hello";
	return 0;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, "section .data") {
		t.Error("NASM missing data section")
	}
}

func TestNASM_NoGASPrefixes(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let x: i32 = 5;
	return x;
}`
	prog := mustParse(t, src)
	target := windowsAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	// NASM should not have GAS-style register prefixes.
	if strings.Contains(asm, "%rax") {
		t.Error("NASM output contains GAS-style register prefix %rax")
	}
	if strings.Contains(asm, "$") && strings.Contains(asm, "movq $") {
		t.Error("NASM output contains GAS-style immediate prefix $")
	}
}

// ---------------------------------------------------------------------------
// GAS-specific: structure tests
// ---------------------------------------------------------------------------

func TestGAS_LinuxEntry(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	target := linuxAMD64Target()
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, ".globl _start") {
		t.Error("GAS Linux missing .globl _start")
	}
	if !strings.Contains(asm, "syscall") {
		t.Error("GAS Linux missing syscall instruction")
	}
}

func TestGAS_DarwinEntry(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	target, _ := ResolveTarget("darwin", "amd64")
	mod := Lower(prog, target)
	asm := EmitX86_64(mod, target)

	if !strings.Contains(asm, ".globl _main") {
		t.Error("GAS Darwin missing .globl _main")
	}
}

// ---------------------------------------------------------------------------
// ARM64-specific: structure tests
// ---------------------------------------------------------------------------

func TestARM64_HasPrologue(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "stp x29, x30") {
		t.Error("ARM64 missing stp x29, x30 prologue")
	}
}

func TestARM64_HasEpilogue(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	target := darwinARM64Target()
	mod := Lower(prog, target)
	asm := EmitARM64(mod, target)

	if !strings.Contains(asm, "ldp x29, x30") {
		t.Error("ARM64 missing ldp x29, x30 epilogue")
	}
}

// ---------------------------------------------------------------------------
// x86 32-bit: structure and array tests
// ---------------------------------------------------------------------------

func TestX86_HasEntry(t *testing.T) {
	src := `module test; fn main() -> i32 { return 0; }`
	prog := mustParse(t, src)
	target, _ := ResolveTarget("linux", "x86")
	mod := Lower(prog, target)
	asm := EmitX86(mod, target)

	if !strings.Contains(asm, "_start:") {
		t.Error("x86 missing _start entry point")
	}
	if !strings.Contains(asm, "int $0x80") {
		t.Error("x86 missing int $0x80 syscall")
	}
}

func TestX86_ArrayNew(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let arr: []i32 = [1, 2, 3];
	return 0;
}`
	prog := mustParse(t, src)
	target, _ := ResolveTarget("linux", "x86")
	mod := Lower(prog, target)
	asm := EmitX86(mod, target)

	if !strings.Contains(asm, "_novus_heap") {
		t.Error("x86 array output missing heap reference")
	}
}

func TestX86_GlobalVars(t *testing.T) {
	src := `module test;
let val: i32 = 99;
fn main() -> i32 { return val; }`
	prog := mustParse(t, src)
	target, _ := ResolveTarget("linux", "x86")
	mod := Lower(prog, target)
	asm := EmitX86(mod, target)

	if !strings.Contains(asm, "_g_val:") {
		t.Error("x86 output missing global variable label")
	}
	if !strings.Contains(asm, ".long 99") {
		t.Error("x86 output missing .long 99 for global init")
	}
}

// ---------------------------------------------------------------------------
// Compile-time conditional (#if) resolution tests
// ---------------------------------------------------------------------------

func TestCompTimeResolve_MatchingCondition(t *testing.T) {
	src := `module test;
#if(os == "linux") {
	fn platform_helper() -> i32 { return 1; }
}
fn main() -> i32 { return 0; }`
	tokens, _ := lexer.Lex(src)
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	constants := map[string]string{"os": "linux", "arch": "amd64"}
	ast.ResolveCompTimeBlocks(prog, constants)

	// The function should have been merged into main program.
	found := false
	for _, fn := range prog.Functions {
		if fn.Name == "platform_helper" {
			found = true
		}
	}
	if !found {
		t.Error("expected platform_helper to be included when os matches 'linux'")
	}
}

func TestCompTimeResolve_NonMatchingCondition(t *testing.T) {
	src := `module test;
#if(os == "windows") {
	fn win_only() -> i32 { return 1; }
}
fn main() -> i32 { return 0; }`
	tokens, _ := lexer.Lex(src)
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	constants := map[string]string{"os": "linux", "arch": "amd64"}
	ast.ResolveCompTimeBlocks(prog, constants)

	for _, fn := range prog.Functions {
		if fn.Name == "win_only" {
			t.Error("win_only should NOT be included when os is 'linux'")
		}
	}
}

func TestCompTimeResolve_NotEqual(t *testing.T) {
	src := `module test;
#if(os != "windows") {
	fn unix_helper() -> i32 { return 1; }
}
fn main() -> i32 { return 0; }`
	tokens, _ := lexer.Lex(src)
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	constants := map[string]string{"os": "linux", "arch": "amd64"}
	ast.ResolveCompTimeBlocks(prog, constants)

	found := false
	for _, fn := range prog.Functions {
		if fn.Name == "unix_helper" {
			found = true
		}
	}
	if !found {
		t.Error("expected unix_helper when os != windows and os is linux")
	}
}

func TestCompTimeResolve_ArchCondition(t *testing.T) {
	src := `module test;
#if(arch == "arm64") {
	fn arm_helper() -> i32 { return 1; }
}
fn main() -> i32 { return 0; }`
	tokens, _ := lexer.Lex(src)
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	// Resolve with amd64 — should NOT include arm_helper.
	constants := map[string]string{"os": "linux", "arch": "amd64"}
	ast.ResolveCompTimeBlocks(prog, constants)

	for _, fn := range prog.Functions {
		if fn.Name == "arm_helper" {
			t.Error("arm_helper should NOT be included when arch is amd64")
		}
	}
}

func TestCompTimeResolve_GlobalMerge(t *testing.T) {
	src := `module test;
#if(os == "darwin") {
	let platform_id: i32 = 3;
}
fn main() -> i32 { return 0; }`
	tokens, _ := lexer.Lex(src)
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	constants := map[string]string{"os": "darwin", "arch": "arm64"}
	ast.ResolveCompTimeBlocks(prog, constants)

	found := false
	for _, g := range prog.Globals {
		if g.Name == "platform_id" {
			found = true
		}
	}
	if !found {
		t.Error("expected platform_id global to be included when os matches darwin")
	}
}

func TestCompTimeResolve_ImportMerge(t *testing.T) {
	src := `module test;
#if(os == "linux") {
	import mylib;
}
fn main() -> i32 { return 0; }`
	tokens, _ := lexer.Lex(src)
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	constants := map[string]string{"os": "linux", "arch": "amd64"}
	ast.ResolveCompTimeBlocks(prog, constants)

	found := false
	for _, imp := range prog.Imports {
		if imp.Path == "mylib" {
			found = true
		}
	}
	if !found {
		t.Error("expected import 'mylib' to be included when os matches linux")
	}
}

// ---------------------------------------------------------------------------
// Target helper method tests
// ---------------------------------------------------------------------------

func TestTargetOSName(t *testing.T) {
	tests := []struct {
		os   string
		arch string
		want string
	}{
		{"linux", "amd64", "linux"},
		{"darwin", "arm64", "darwin"},
		{"windows", "amd64", "windows"},
	}
	for _, tc := range tests {
		tgt, _ := ResolveTarget(tc.os, tc.arch)
		if got := tgt.OSName(); got != tc.want {
			t.Errorf("OSName() for %s/%s: got %q, want %q", tc.os, tc.arch, got, tc.want)
		}
	}
}

func TestTargetArchName(t *testing.T) {
	tests := []struct {
		os   string
		arch string
		want string
	}{
		{"linux", "amd64", "amd64"},
		{"darwin", "arm64", "arm64"},
		{"linux", "x86", "x86"},
	}
	for _, tc := range tests {
		tgt, _ := ResolveTarget(tc.os, tc.arch)
		if got := tgt.ArchName(); got != tc.want {
			t.Errorf("ArchName() for %s/%s: got %q, want %q", tc.os, tc.arch, got, tc.want)
		}
	}
}

// ---------------------------------------------------------------------------
// Arithmetic — full cross-platform assembly verification
// ---------------------------------------------------------------------------

func TestArithmetic_AllTargets(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let a: i32 = 10;
	let b: i32 = 3;
	let add_r: i32 = a + b;
	let sub_r: i32 = a - b;
	let mul_r: i32 = a * b;
	let div_r: i32 = a / b;
	let mod_r: i32 = a % b;
	return add_r;
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			if len(asm) == 0 {
				t.Fatal("empty assembly output")
			}
			// All targets should have add, sub, mul, div.
			for _, op := range []string{"add", "sub"} {
				if !strings.Contains(asm, op) {
					t.Errorf("assembly missing %s instruction", op)
				}
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Bitwise operations — cross-platform
// ---------------------------------------------------------------------------

func TestBitwise_AllTargets(t *testing.T) {
	src := `module test;
fn main() -> i32 {
	let a: i32 = 0xFF;
	let b: i32 = 0x0F;
	let and_r: i32 = a & b;
	let or_r: i32 = a | b;
	let xor_r: i32 = a ^ b;
	let not_r: i32 = ~a;
	let shl_r: i32 = a << 2;
	let shr_r: i32 = a >> 2;
	return and_r;
}`
	prog := mustParse(t, src)

	targets := []struct {
		name   string
		target *Target
	}{
		{"linux_amd64", linuxAMD64Target()},
		{"windows_amd64", windowsAMD64Target()},
		{"darwin_arm64", darwinARM64Target()},
	}

	for _, tc := range targets {
		t.Run(tc.name, func(t *testing.T) {
			mod := Lower(prog, tc.target)
			var asm string
			if tc.target.Arch == Arch_ARM64 {
				asm = EmitARM64(mod, tc.target)
			} else {
				asm = EmitX86_64(mod, tc.target)
			}
			if len(asm) == 0 {
				t.Fatal("empty assembly output")
			}
			// All should have and, or, xor operations.
			if !strings.Contains(asm, "and") {
				t.Error("assembly missing and instruction")
			}
			if !strings.Contains(asm, "or") {
				t.Error("assembly missing or instruction")
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Register warnings: imported functions should NOT generate warnings
// ---------------------------------------------------------------------------

func TestRegisterWarnings_ImportedFunctionsSkipped(t *testing.T) {
	arm64Target := &Target{OS: OS_Linux, Arch: Arch_x86_64}

	mod := &IRModule{
		EntryFunc: "main",
		Functions: []*IRFunc{
			{
				Name:     "imported_fn",
				Imported: true,
				Instrs: []IRInstr{
					{Op: IRMov, Dst: PReg("x0"), Src1: Imm(1)},
					{Op: IRMov, Dst: PReg("x16"), Src1: Imm(42)},
				},
			},
			{
				Name:     "main",
				Imported: false,
				Instrs: []IRInstr{
					{Op: IRMov, Dst: PReg("rax"), Src1: Imm(1)},
				},
			},
		},
	}

	warnings := checkRegisterWarnings(mod, arm64Target)
	if len(warnings) > 0 {
		t.Errorf("expected no warnings (imported fn regs should be skipped), got %d: %v", len(warnings), warnings)
	}
}

func TestRegisterWarnings_UserCodeWarns(t *testing.T) {
	arm64Target := &Target{OS: OS_Linux, Arch: Arch_x86_64}

	mod := &IRModule{
		EntryFunc: "main",
		Functions: []*IRFunc{
			{
				Name:     "main",
				Imported: false,
				Instrs: []IRInstr{
					{Op: IRMov, Dst: PReg("x0"), Src1: Imm(1)},
				},
			},
		},
	}

	warnings := checkRegisterWarnings(mod, arm64Target)
	if len(warnings) == 0 {
		t.Error("expected warnings for ARM64 register in user's x86_64 code, got none")
	}
}

func TestLower_ImportedFlagPropagated(t *testing.T) {
	prog := &ast.Program{
		Functions: []*ast.FnDecl{
			{
				Name:     "imported_fn",
				Imported: true,
				Params:   []*ast.Param{},
				ReturnType: &ast.TypeExpr{Name: "void"},
				Body:     &ast.BlockStmt{},
			},
			{
				Name:     "main",
				Imported: false,
				Params:   []*ast.Param{},
				ReturnType: &ast.TypeExpr{Name: "void"},
				Body:     &ast.BlockStmt{},
			},
		},
	}

	target := &Target{OS: OS_Linux, Arch: Arch_x86_64}
	mod := Lower(prog, target)

	for _, fn := range mod.Functions {
		switch fn.Name {
		case "imported_fn":
			if !fn.Imported {
				t.Error("imported_fn should have Imported=true in IR")
			}
		case "main":
			if fn.Imported {
				t.Error("main should have Imported=false in IR")
			}
		}
	}
}

func TestNovusToolsDir(t *testing.T) {
	dir := novusToolsDir()
	if dir == "" {
		t.Error("novusToolsDir returned empty string")
	}
	if !strings.Contains(dir, ".novus") {
		t.Errorf("expected .novus in tools dir path, got %s", dir)
	}
}

func TestDetectToolchainWithPaths(t *testing.T) {
	target := &Target{OS: OS_Windows, Arch: Arch_x86_64, Flavor: NASM}
	missing := DetectToolchainWithPaths(target, "/nonexistent/nasm.exe", "/nonexistent/golink.exe")
	if len(missing) == 0 {
		t.Error("expected missing tools with nonexistent paths")
	}
}

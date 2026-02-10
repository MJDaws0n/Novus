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

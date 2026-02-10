package semantic_test

import (
	"novus/internal/lexer"
	"novus/internal/parser"
	"novus/internal/semantic"
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

func analyze(t *testing.T, input string) []semantic.Diagnostic {
	t.Helper()
	tokens, lexErrs := lexer.Lex(input)
	if len(lexErrs) > 0 {
		t.Fatalf("lex errors: %v", lexErrs)
	}
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}
	return semantic.Analyze(prog)
}

func countErrors(diags []semantic.Diagnostic) int {
	n := 0
	for _, d := range diags {
		if d.Severity == semantic.Error {
			n++
		}
	}
	return n
}

func countWarnings(diags []semantic.Diagnostic) int {
	n := 0
	for _, d := range diags {
		if d.Severity == semantic.Warning {
			n++
		}
	}
	return n
}

func expectErrors(t *testing.T, diags []semantic.Diagnostic, want int) {
	t.Helper()
	got := countErrors(diags)
	if got != want {
		t.Errorf("expected %d error(s), got %d", want, got)
		for _, d := range diags {
			t.Logf("  %s", d.Error())
		}
	}
}

func expectWarnings(t *testing.T, diags []semantic.Diagnostic, want int) {
	t.Helper()
	got := countWarnings(diags)
	if got != want {
		t.Errorf("expected %d warning(s), got %d", want, got)
		for _, d := range diags {
			t.Logf("  %s", d.Error())
		}
	}
}

func expectNoDiagnostics(t *testing.T, diags []semantic.Diagnostic) {
	t.Helper()
	if len(diags) > 0 {
		t.Errorf("expected no diagnostics, got %d", len(diags))
		for _, d := range diags {
			t.Logf("  %s", d.Error())
		}
	}
}

func expectErrorContains(t *testing.T, diags []semantic.Diagnostic, substr string) {
	t.Helper()
	for _, d := range diags {
		if d.Severity == semantic.Error && strings.Contains(d.Message, substr) {
			return
		}
	}
	t.Errorf("expected an error containing %q, diagnostics:", substr)
	for _, d := range diags {
		t.Logf("  %s", d.Error())
	}
}

// ---------------------------------------------------------------------------
// Valid programs
// ---------------------------------------------------------------------------

func TestValidEmptyVoidFunction(t *testing.T) {
	diags := analyze(t, "fn main() -> void {}")
	expectNoDiagnostics(t, diags)
}

func TestValidReturnValue(t *testing.T) {
	src := "fn add(a: i32, b: i32) -> i32 { return a + b; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidFunctionCalls(t *testing.T) {
	src := "fn add(a: i32, b: i32) -> i32 { return a + b; } fn main() -> void { let x: i32 = add(1, 2); }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidRecursion(t *testing.T) {
	src := "fn fib(n: i32) -> i32 { if (n <= 1) { return n; } return fib(n - 1) + fib(n - 2); }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidForwardCall(t *testing.T) {
	src := "fn caller() -> i32 { return callee(); } fn callee() -> i32 { return 42; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidIfElseReturn(t *testing.T) {
	src := "fn abs(x: i32) -> i32 { if (x > 0) { return x; } else { return 0 - x; } }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidIfElseIfReturn(t *testing.T) {
	src := "fn classify(x: i32) -> i32 { if (x > 0) { return 1; } else if (x == 0) { return 0; } else { return 0 - 1; } }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidWhileLoop(t *testing.T) {
	src := "fn loop() -> void { let i: i32 = 0; while (i < 10) { i = i + 1; } }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidForLoop(t *testing.T) {
	src := "fn loop() -> void { for (let i: i32 = 0; i < 10; i = i + 1) { let x: i32 = i; } }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidBreakContinueInsideLoop(t *testing.T) {
	src := "fn f() -> void { while (true) { break; } for (let i: i32 = 0; i < 10; i = i + 1) { continue; } }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidStringConcatenation(t *testing.T) {
	src := "fn greet() -> str { let msg: str = \"hello\" + \" world\"; return msg; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidBoolOperators(t *testing.T) {
	src := "fn logic(a: bool, b: bool) -> bool { return a && b || !a; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidComparison(t *testing.T) {
	src := "fn cmp(a: i32, b: i32) -> bool { return a < b; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidEquality(t *testing.T) {
	src := "fn eq(a: str, b: str) -> bool { return a == b; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidBareReturnVoid(t *testing.T) {
	src := "fn doNothing() -> void { return; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidUnaryMinus(t *testing.T) {
	src := "fn negate(x: i32) -> i32 { return -x; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidFloat(t *testing.T) {
	src := "fn pi() -> f64 { return 3.14; }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

func TestValidNestedScopes(t *testing.T) {
	src := "fn f() -> void { let x: i32 = 1; if (true) { let y: i32 = x + 1; } }"
	diags := analyze(t, src)
	expectNoDiagnostics(t, diags)
}

// ---------------------------------------------------------------------------
// Undefined identifiers
// ---------------------------------------------------------------------------

func TestUndefinedVariable(t *testing.T) {
	src := "fn main() -> void { let x: i32 = y; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "undefined identifier \"y\"")
}

func TestUndefinedInExpression(t *testing.T) {
	src := "fn main() -> void { let x: i32 = 1 + unknown; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "undefined identifier \"unknown\"")
}

func TestUndefinedFunction(t *testing.T) {
	src := "fn main() -> void { foo(); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "undefined function \"foo\"")
}

// ---------------------------------------------------------------------------
// Duplicate declarations
// ---------------------------------------------------------------------------

func TestDuplicateVariableInSameScope(t *testing.T) {
	src := "fn main() -> void { let x: i32 = 1; let x: i32 = 2; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "already declared in this scope")
}

func TestDuplicateFunction(t *testing.T) {
	src := "fn foo() -> void {} fn foo() -> void {}"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "function \"foo\" already declared")
}

func TestDuplicateParameter(t *testing.T) {
	src := "fn bad(x: i32, x: i32) -> void {}"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "duplicate parameter \"x\"")
}

// ---------------------------------------------------------------------------
// Shadowing (warnings)
// ---------------------------------------------------------------------------

func TestShadowingWarning(t *testing.T) {
	src := "fn main() -> void { let x: i32 = 1; if (true) { let x: i32 = 2; } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
	expectWarnings(t, diags, 1)
}

func TestShadowingFunctionParam(t *testing.T) {
	src := "fn f(x: i32) -> void { let x: i32 = 42; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
	expectWarnings(t, diags, 1)
}

func TestShadowingNestedLoops(t *testing.T) {
	src := "fn f() -> void { for (let i: i32 = 0; i < 10; i = i + 1) { for (let i: i32 = 0; i < 5; i = i + 1) { let x: i32 = i; } } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
	expectWarnings(t, diags, 1)
}

// ---------------------------------------------------------------------------
// Type mismatches
// ---------------------------------------------------------------------------

func TestTypeMismatchLetStmt(t *testing.T) {
	src := "fn main() -> void { let x: i32 = true; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot assign bool")
}

func TestTypeMismatchReturn(t *testing.T) {
	src := "fn main() -> i32 { return true; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot return bool")
}

func TestTypeMismatchAssignment(t *testing.T) {
	src := "fn main() -> void { let x: i32 = 1; x = true; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot assign bool to i32")
}

func TestMismatchedArithmeticTypes(t *testing.T) {
	src := "fn f() -> void { let a: i32 = 1; let b: f64 = 2.0; let c: i32 = a + b; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "mismatched types")
}

func TestStringPlusInt(t *testing.T) {
	src := "fn f() -> void { let x: str = \"hello\" + 1; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "numeric or string operands")
}

func TestBoolArithmetic(t *testing.T) {
	src := "fn f() -> void { let x: i32 = 1 - true; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "numeric operands")
}

func TestComparisonTypeMismatch(t *testing.T) {
	src := "fn f() -> bool { return 1 < true; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "numeric operands")
}

func TestEqualityMismatch(t *testing.T) {
	src := "fn f() -> bool { return 1 == true; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "mismatched types")
}

// ---------------------------------------------------------------------------
// Unary operator checks
// ---------------------------------------------------------------------------

func TestUnaryNotRequiresBool(t *testing.T) {
	src := "fn f() -> void { let x: bool = !42; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "requires bool operand")
}

func TestUnaryMinusRequiresNumeric(t *testing.T) {
	src := "fn f() -> void { let x: bool = -true; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "requires numeric operand")
}

// ---------------------------------------------------------------------------
// Logical operator checks
// ---------------------------------------------------------------------------

func TestLogicalAndRequiresBool(t *testing.T) {
	src := "fn f() -> bool { let x: i32 = 1; return x && true; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "requires bool operands")
}

func TestLogicalOrRequiresBool(t *testing.T) {
	src := "fn f() -> bool { return true || 42; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "requires bool operands")
}

// ---------------------------------------------------------------------------
// Condition type checks
// ---------------------------------------------------------------------------

func TestIfConditionMustBeBool(t *testing.T) {
	src := "fn f() -> void { if (42) { return; } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "if condition must be bool")
}

func TestWhileConditionMustBeBool(t *testing.T) {
	src := "fn f() -> void { while (1) { break; } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "while condition must be bool")
}

func TestForConditionMustBeBool(t *testing.T) {
	src := "fn f() -> void { for (let i: i32 = 0; i; i = i + 1) { break; } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "for condition must be bool")
}

// ---------------------------------------------------------------------------
// Break / continue outside loop
// ---------------------------------------------------------------------------

func TestBreakOutsideLoop(t *testing.T) {
	src := "fn f() -> void { break; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "break statement outside of loop")
}

func TestContinueOutsideLoop(t *testing.T) {
	src := "fn f() -> void { continue; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "continue statement outside of loop")
}

func TestBreakInNestedLoop(t *testing.T) {
	src := "fn f() -> void { while (true) { while (true) { break; } break; } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

// ---------------------------------------------------------------------------
// Function call checks
// ---------------------------------------------------------------------------

func TestCallArityTooFew(t *testing.T) {
	src := "fn add(a: i32, b: i32) -> i32 { return a + b; } fn main() -> void { let x: i32 = add(1); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "expects 2 arguments, got 1")
}

func TestCallArityTooMany(t *testing.T) {
	src := "fn add(a: i32, b: i32) -> i32 { return a + b; } fn main() -> void { let x: i32 = add(1, 2, 3); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "expects 2 arguments, got 3")
}

func TestCallArgTypeMismatch(t *testing.T) {
	src := "fn greet(name: str) -> void { return; } fn main() -> void { greet(42); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "expected str, got untyped int")
}

func TestCallNotAFunction(t *testing.T) {
	src := "fn f() -> void { let x: i32 = 1; x(); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "\"x\" is not a function")
}

// ---------------------------------------------------------------------------
// Return path analysis
// ---------------------------------------------------------------------------

func TestMissingReturnNonVoid(t *testing.T) {
	src := "fn f() -> i32 { let x: i32 = 1; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "must return a value")
}

func TestMissingReturnIfWithoutElse(t *testing.T) {
	src := "fn f(x: i32) -> i32 { if (x > 0) { return 1; } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "must return a value")
}

func TestReturnInBothBranches(t *testing.T) {
	src := "fn f(x: bool) -> i32 { if (x) { return 1; } else { return 0; } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestVoidFunctionReturnsValue(t *testing.T) {
	src := "fn f() -> void { return 42; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "void function")
}

func TestNonVoidBareReturn(t *testing.T) {
	src := "fn f() -> i32 { return; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "expects return type i32, got void")
}

// ---------------------------------------------------------------------------
// Unknown / void types
// ---------------------------------------------------------------------------

func TestUnknownType(t *testing.T) {
	src := "fn f() -> void { let x: MyType = 1; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "unknown type \"MyType\"")
}

func TestVoidVariable(t *testing.T) {
	src := "fn f() -> void { let x: void = 1; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot have type void")
}

func TestVoidParameter(t *testing.T) {
	src := "fn f(x: void) -> void {}"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "parameter \"x\" cannot have type void")
}

// ---------------------------------------------------------------------------
// HasErrors helper
// ---------------------------------------------------------------------------

func TestHasErrorsTrue(t *testing.T) {
	src := "fn f() -> void { let x: i32 = y; }"
	diags := analyze(t, src)
	if !semantic.HasErrors(diags) {
		t.Error("HasErrors should return true")
	}
}

func TestHasErrorsFalse(t *testing.T) {
	diags := analyze(t, "fn f() -> void {}")
	if semantic.HasErrors(diags) {
		t.Error("HasErrors should return false")
	}
}

// ---------------------------------------------------------------------------
// Multiple errors in one program
// ---------------------------------------------------------------------------

func TestMultipleErrors(t *testing.T) {
	src := "fn f() -> i32 { let x: i32 = true; let y: str = 42; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 3) // bool->i32, untyped int->str, missing return
}

// ---------------------------------------------------------------------------
// Scope isolation
// ---------------------------------------------------------------------------

func TestVariableNotVisibleOutsideScope(t *testing.T) {
	src := "fn f() -> void { if (true) { let x: i32 = 1; } let y: i32 = x; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "undefined identifier \"x\"")
}

func TestForLoopVarNotVisibleOutside(t *testing.T) {
	src := "fn f() -> void { for (let i: i32 = 0; i < 10; i = i + 1) {} let x: i32 = i; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "undefined identifier \"i\"")
}

// ---------------------------------------------------------------------------
// All built-in types can be used
// ---------------------------------------------------------------------------

func TestAllBuiltinTypes(t *testing.T) {
	src := "fn f(a: u8, b: u16, c: u32, d: u64, e: i8, g: i32, h: i64, i: f32, j: f64, k: bool, l: str) -> void {}"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

// ---------------------------------------------------------------------------
// Diagnostic positions
// ---------------------------------------------------------------------------

func TestDiagnosticPosition(t *testing.T) {
	src := "fn f() -> void {\nlet x: i32 = y;\n}"
	diags := analyze(t, src)
	if len(diags) == 0 {
		t.Fatal("expected a diagnostic")
	}
	d := diags[0]
	if d.Pos.Line != 2 {
		t.Errorf("expected line 2, got %d", d.Pos.Line)
	}
}

// ===========================================================================
// NEW: Untyped numeric literal flexibility
// ===========================================================================

func TestUntypedIntAssignableToAllIntTypes(t *testing.T) {
	src := `fn f() -> void {
		let a: u8 = 0;
		let b: u16 = 0;
		let c: u32 = 0;
		let d: u64 = 0;
		let e: i8 = 0;
		let g: i16 = 0;
		let h: i32 = 0;
		let i: i64 = 0;
	}`
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestUntypedIntAssignableToFloat(t *testing.T) {
	src := "fn f() -> void { let x: f64 = 1; let y: f32 = 2; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestUntypedFloatAssignableToFloatTypes(t *testing.T) {
	src := "fn f() -> void { let x: f64 = 1.5; let y: f32 = 2.5; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestUntypedFloatNotAssignableToInt(t *testing.T) {
	src := "fn f() -> void { let x: i32 = 1.5; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot assign untyped float")
}

func TestUntypedIntNotAssignableToStr(t *testing.T) {
	src := "fn f() -> void { let x: str = 42; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot assign untyped int")
}

func TestUntypedIntNotAssignableToBool(t *testing.T) {
	src := "fn f() -> void { let x: bool = 0; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot assign untyped int")
}

func TestUntypedArithmetic(t *testing.T) {
	src := "fn f() -> void { let x: u64 = 1 + 2 * 3; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestUntypedMixedWithConcrete(t *testing.T) {
	src := "fn f(n: i32) -> i32 { return n + 1; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestUntypedReturnValue(t *testing.T) {
	src := "fn f() -> u64 { return 42; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestUntypedComparison(t *testing.T) {
	src := "fn f(n: i32) -> bool { return n < 10; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

// ===========================================================================
// NEW: Built-in intrinsic functions
// ===========================================================================

func TestBuiltinMovValid(t *testing.T) {
	src := "fn f() -> void { mov(eax, 4); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinLeaValid(t *testing.T) {
	src := "fn f(msg: str) -> void { lea(ecx, msg); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinIntValid(t *testing.T) {
	src := "fn f() -> void { int(0x80); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinPushValid(t *testing.T) {
	src := "fn f() -> void { push(42); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinPopReturnsU64(t *testing.T) {
	src := "fn f() -> u64 { return pop(); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinNopValid(t *testing.T) {
	src := "fn f() -> void { nop(); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinSyscallValid(t *testing.T) {
	src := "fn f() -> void { syscall(); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinRetValid(t *testing.T) {
	src := "fn f() -> void { ret(); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinCallValid(t *testing.T) {
	src := "fn f() -> void { call(eax); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinGetregReturnsU64(t *testing.T) {
	src := "fn f() -> void { let val: u64 = getreg(eax); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinGetflagReturnsBool(t *testing.T) {
	src := "fn f() -> void { let flag: bool = getflag(eax); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinSetregValid(t *testing.T) {
	src := "fn f() -> void { setreg(eax, 42); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinSetflagValid(t *testing.T) {
	src := "fn f() -> void { setflag(eax, true); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinArityError(t *testing.T) {
	src := "fn f() -> void { mov(eax); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "expects 2 arguments, got 1")
}

func TestBuiltinArityErrorTooMany(t *testing.T) {
	src := "fn f() -> void { nop(1); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "expects 0 arguments, got 1")
}

func TestBuiltinSkipsArgTypeCheck(t *testing.T) {
	// mov accepts any arg types (register + int, register + str, etc.)
	src := "fn f() -> void { mov(eax, \"hello\"); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinGetregUsableInExpression(t *testing.T) {
	src := "fn f() -> bool { let x: u64 = getreg(eax); return x == 0; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestBuiltinGetflagUsableInCondition(t *testing.T) {
	src := "fn f() -> void { if (getflag(eax)) { nop(); } }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestCannotRedeclareBuiltin(t *testing.T) {
	src := "fn mov() -> void {}"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot redeclare built-in function")
}

func TestCannotDeclareVarWithBuiltinName(t *testing.T) {
	src := "fn f() -> void { let mov: i32 = 1; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "reserved name")
}

// ===========================================================================
// NEW: Reserved CPU registers
// ===========================================================================

func TestRegistersAccessible(t *testing.T) {
	src := "fn f() -> void { mov(eax, 0); mov(ebx, 0); mov(ecx, 0); mov(edx, 0); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestRegisters64Bit(t *testing.T) {
	src := "fn f() -> void { mov(rax, 0); mov(r8, 0); mov(r15, 0); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestRegistersXMM(t *testing.T) {
	src := "fn f() -> void { mov(xmm0, 0); mov(xmm15, 0); }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestCannotDeclareVarWithRegisterName(t *testing.T) {
	src := "fn f() -> void { let eax: i32 = 1; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "reserved name")
}

func TestCannotUseFunctionNameAsRegister(t *testing.T) {
	src := "fn eax() -> void {}"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "reserved register name")
}

func TestCannotUseRegisterAsParam(t *testing.T) {
	src := "fn f(eax: i32) -> void {}"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "reserved name")
}

func TestRegisterTypeIsRegister(t *testing.T) {
	// Registers have type "register" — can't assign to i32 directly
	src := "fn f() -> void { let x: i32 = eax; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "cannot assign register")
}

// ===========================================================================
// NEW: Index expressions (string indexing)
// ===========================================================================

func TestStringIndexValid(t *testing.T) {
	src := "fn f(s: str) -> str { let i: i32 = 0; return s[i]; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestStringIndexWithLiteral(t *testing.T) {
	src := "fn f(s: str) -> str { return s[0]; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestStringIndexComparison(t *testing.T) {
	// s[i] returns str, so comparing with a char literal should work.
	src := "fn f(s: str) -> bool { return s[0] != \"x\"; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

func TestIndexNonStringErrors(t *testing.T) {
	src := "fn f() -> void { let x: i32 = 5; let y: i32 = x[0]; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "not indexable")
}

func TestIndexWithNonIntegerErrors(t *testing.T) {
	src := "fn f(s: str) -> void { let c: str = s[true]; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 1)
	expectErrorContains(t, diags, "index must be an integer")
}

func TestIndexWithU64Works(t *testing.T) {
	src := "fn f(s: str) -> str { let i: u64 = 0; return s[i]; }"
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

// ===========================================================================
// NEW: example.nov integration style test
// ===========================================================================

func TestExampleNovStyle(t *testing.T) {
	src := `
fn len(s: str) -> i32 {
    let count: i32 = 0;
    let i: i32 = 0;
    while (s[i] != '\0') {
        count = count + 1;
        i = i + 1;
    }
    return count;
}

fn print(msg: str) -> void {
    msg = msg + '\r\n';
    mov(eax, 4);
    mov(ebx, 1);
    lea(ecx, msg);
    mov(edx, len(msg));
    int(0x80);
}

fn main() -> i32 {
    let msg: str = "Hello, World!";
    print(msg);
    mov(eax, 1);
    mov(ebx, 0);
    int(0x80);
    return 0;
}
`
	diags := analyze(t, src)
	expectErrors(t, diags, 0)
}

package parser_test

import (
	"novus/internal/ast"
	"novus/internal/lexer"
	"novus/internal/parser"
	"os"
	"testing"
)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

func parseInput(t *testing.T, input string) *ast.Program {
	t.Helper()
	tokens, lexErrs := lexer.Lex(input)
	if len(lexErrs) > 0 {
		t.Fatalf("lex errors: %v", lexErrs)
	}
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		for _, e := range parseErrs {
			t.Errorf("parse error: %s", e.Error())
		}
		t.FailNow()
	}
	return prog
}

func parseInputExpectErrors(t *testing.T, input string) (*ast.Program, []parser.ParseError) {
	t.Helper()
	tokens, _ := lexer.Lex(input)
	return parser.Parse(tokens)
}

// ---------------------------------------------------------------------------
// Module / Import
// ---------------------------------------------------------------------------

func TestParseModuleDecl(t *testing.T) {
	prog := parseInput(t, "module foo;")
	if prog.Module == nil {
		t.Fatal("expected module declaration")
	}
	if prog.Module.Name != "foo" {
		t.Errorf("module name: got %q, want %q", prog.Module.Name, "foo")
	}
}

func TestParseImportDecl(t *testing.T) {
	prog := parseInput(t, "import bar;")
	if len(prog.Imports) != 1 {
		t.Fatalf("expected 1 import, got %d", len(prog.Imports))
	}
	if prog.Imports[0].Name != "bar" {
		t.Errorf("import name: got %q, want %q", prog.Imports[0].Name, "bar")
	}
}

func TestParseModuleAndImports(t *testing.T) {
	prog := parseInput(t, "module app; import foo; import bar;")
	if prog.Module == nil || prog.Module.Name != "app" {
		t.Errorf("module: got %v", prog.Module)
	}
	if len(prog.Imports) != 2 {
		t.Fatalf("expected 2 imports, got %d", len(prog.Imports))
	}
	if prog.Imports[0].Name != "foo" || prog.Imports[1].Name != "bar" {
		t.Errorf("imports: got %q %q", prog.Imports[0].Name, prog.Imports[1].Name)
	}
}

// ---------------------------------------------------------------------------
// Function declarations
// ---------------------------------------------------------------------------

func TestParseFnDeclEmpty(t *testing.T) {
	prog := parseInput(t, "fn main() -> void {}")
	if len(prog.Functions) != 1 {
		t.Fatalf("expected 1 function, got %d", len(prog.Functions))
	}
	fn := prog.Functions[0]
	if fn.Name != "main" {
		t.Errorf("fn name: got %q, want %q", fn.Name, "main")
	}
	if len(fn.Params) != 0 {
		t.Errorf("expected 0 params, got %d", len(fn.Params))
	}
	if fn.ReturnType == nil || fn.ReturnType.Name != "void" {
		t.Errorf("return type: got %v", fn.ReturnType)
	}
	if len(fn.Body.Stmts) != 0 {
		t.Errorf("expected empty body, got %d stmts", len(fn.Body.Stmts))
	}
}

func TestParseFnDeclWithParams(t *testing.T) {
	prog := parseInput(t, "fn add(a: i32, b: i32) -> i32 { return a + b; }")
	if len(prog.Functions) != 1 {
		t.Fatalf("expected 1 function, got %d", len(prog.Functions))
	}
	fn := prog.Functions[0]
	if fn.Name != "add" {
		t.Errorf("fn name: got %q", fn.Name)
	}
	if len(fn.Params) != 2 {
		t.Fatalf("expected 2 params, got %d", len(fn.Params))
	}
	if fn.Params[0].Name != "a" || fn.Params[0].Type.Name != "i32" {
		t.Errorf("param 0: %s: %s", fn.Params[0].Name, fn.Params[0].Type.Name)
	}
	if fn.Params[1].Name != "b" || fn.Params[1].Type.Name != "i32" {
		t.Errorf("param 1: %s: %s", fn.Params[1].Name, fn.Params[1].Type.Name)
	}
	if fn.ReturnType.Name != "i32" {
		t.Errorf("return type: got %q", fn.ReturnType.Name)
	}
	if len(fn.Body.Stmts) != 1 {
		t.Fatalf("expected 1 statement in body, got %d", len(fn.Body.Stmts))
	}
	ret, ok := fn.Body.Stmts[0].(*ast.ReturnStmt)
	if !ok {
		t.Fatalf("expected ReturnStmt, got %T", fn.Body.Stmts[0])
	}
	bin, ok := ret.Value.(*ast.BinaryExpr)
	if !ok {
		t.Fatalf("expected BinaryExpr, got %T", ret.Value)
	}
	if bin.Op != "+" {
		t.Errorf("binary op: got %q", bin.Op)
	}
}

func TestParseMultipleFunctions(t *testing.T) {
	prog := parseInput(t, "fn foo() -> void {} fn bar() -> void {}")
	if len(prog.Functions) != 2 {
		t.Fatalf("expected 2 functions, got %d", len(prog.Functions))
	}
	if prog.Functions[0].Name != "foo" || prog.Functions[1].Name != "bar" {
		t.Errorf("names: %q %q", prog.Functions[0].Name, prog.Functions[1].Name)
	}
}

// ---------------------------------------------------------------------------
// Let statements
// ---------------------------------------------------------------------------

func TestParseLetStmt(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { let x: i32 = 42; }")
	fn := prog.Functions[0]
	if len(fn.Body.Stmts) != 1 {
		t.Fatalf("expected 1 stmt, got %d", len(fn.Body.Stmts))
	}
	let, ok := fn.Body.Stmts[0].(*ast.LetStmt)
	if !ok {
		t.Fatalf("expected LetStmt, got %T", fn.Body.Stmts[0])
	}
	if let.Name != "x" {
		t.Errorf("let name: got %q", let.Name)
	}
	if let.Type.Name != "i32" {
		t.Errorf("let type: got %q", let.Type.Name)
	}
	lit, ok := let.Value.(*ast.IntLitExpr)
	if !ok {
		t.Fatalf("expected IntLitExpr, got %T", let.Value)
	}
	if lit.Value != "42" {
		t.Errorf("let value: got %q", lit.Value)
	}
}

func TestParseLetStmtStringType(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { let msg: str = \"hello\"; }")
	let := prog.Functions[0].Body.Stmts[0].(*ast.LetStmt)
	if let.Type.Name != "str" {
		t.Errorf("let type: got %q, want %q", let.Type.Name, "str")
	}
	s, ok := let.Value.(*ast.StringLitExpr)
	if !ok {
		t.Fatalf("expected StringLitExpr, got %T", let.Value)
	}
	if s.Value != "\"hello\"" {
		t.Errorf("string value: got %q", s.Value)
	}
}

// ---------------------------------------------------------------------------
// Return / Break / Continue
// ---------------------------------------------------------------------------

func TestParseReturnStmt(t *testing.T) {
	prog := parseInput(t, "fn f() -> i32 { return 0; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	lit := ret.Value.(*ast.IntLitExpr)
	if lit.Value != "0" {
		t.Errorf("return value: got %q", lit.Value)
	}
}

func TestParseBareReturn(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { return; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	if ret.Value != nil {
		t.Errorf("expected nil return value, got %v", ret.Value)
	}
}

func TestParseBreakContinue(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { break; continue; }")
	stmts := prog.Functions[0].Body.Stmts
	if len(stmts) != 2 {
		t.Fatalf("expected 2 stmts, got %d", len(stmts))
	}
	if _, ok := stmts[0].(*ast.BreakStmt); !ok {
		t.Errorf("expected BreakStmt, got %T", stmts[0])
	}
	if _, ok := stmts[1].(*ast.ContinueStmt); !ok {
		t.Errorf("expected ContinueStmt, got %T", stmts[1])
	}
}

// ---------------------------------------------------------------------------
// If / Else / Else-If
// ---------------------------------------------------------------------------

func TestParseIfStmt(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { if (x > 0) { return; } }")
	ifStmt := prog.Functions[0].Body.Stmts[0].(*ast.IfStmt)
	bin := ifStmt.Condition.(*ast.BinaryExpr)
	if bin.Op != ">" {
		t.Errorf("condition op: got %q", bin.Op)
	}
	if len(ifStmt.Then.Stmts) != 1 {
		t.Errorf("then block: expected 1 stmt, got %d", len(ifStmt.Then.Stmts))
	}
	if ifStmt.Else != nil {
		t.Errorf("expected no else, got %T", ifStmt.Else)
	}
}

func TestParseIfElseStmt(t *testing.T) {
	prog := parseInput(t, "fn f() -> i32 { if (x > 0) { return 1; } else { return 0; } }")
	ifStmt := prog.Functions[0].Body.Stmts[0].(*ast.IfStmt)
	if ifStmt.Else == nil {
		t.Fatal("expected else branch")
	}
	elseBlock, ok := ifStmt.Else.(*ast.BlockStmt)
	if !ok {
		t.Fatalf("expected BlockStmt for else, got %T", ifStmt.Else)
	}
	if len(elseBlock.Stmts) != 1 {
		t.Errorf("else block: expected 1 stmt, got %d", len(elseBlock.Stmts))
	}
}

func TestParseIfElseIfStmt(t *testing.T) {
	input := "fn f() -> i32 { if (x > 0) { return 1; } else if (x == 0) { return 0; } else { return -1; } }"
	prog := parseInput(t, input)
	ifStmt := prog.Functions[0].Body.Stmts[0].(*ast.IfStmt)
	elseIf, ok := ifStmt.Else.(*ast.IfStmt)
	if !ok {
		t.Fatalf("expected IfStmt for else-if, got %T", ifStmt.Else)
	}
	bin := elseIf.Condition.(*ast.BinaryExpr)
	if bin.Op != "==" {
		t.Errorf("else-if condition op: got %q", bin.Op)
	}
	if elseIf.Else == nil {
		t.Fatal("expected final else")
	}
	_, ok = elseIf.Else.(*ast.BlockStmt)
	if !ok {
		t.Fatalf("expected BlockStmt for final else, got %T", elseIf.Else)
	}
}

// ---------------------------------------------------------------------------
// While
// ---------------------------------------------------------------------------

func TestParseWhileStmt(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { while (i < 10) { i = i + 1; } }")
	w := prog.Functions[0].Body.Stmts[0].(*ast.WhileStmt)
	bin := w.Condition.(*ast.BinaryExpr)
	if bin.Op != "<" {
		t.Errorf("while condition op: got %q", bin.Op)
	}
	if len(w.Body.Stmts) != 1 {
		t.Fatalf("while body: expected 1 stmt, got %d", len(w.Body.Stmts))
	}
	assign := w.Body.Stmts[0].(*ast.AssignStmt)
	ident := assign.Target.(*ast.IdentExpr)
	if ident.Name != "i" {
		t.Errorf("assign target: got %q", ident.Name)
	}
}

// ---------------------------------------------------------------------------
// For
// ---------------------------------------------------------------------------

func TestParseForStmt(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { for (let i: i32 = 0; i < 10; i = i + 1) { break; } }")
	forStmt := prog.Functions[0].Body.Stmts[0].(*ast.ForStmt)

	let, ok := forStmt.Init.(*ast.LetStmt)
	if !ok {
		t.Fatalf("for init: expected LetStmt, got %T", forStmt.Init)
	}
	if let.Name != "i" || let.Type.Name != "i32" {
		t.Errorf("for init: let %s: %s", let.Name, let.Type.Name)
	}

	bin := forStmt.Condition.(*ast.BinaryExpr)
	if bin.Op != "<" {
		t.Errorf("for condition op: got %q", bin.Op)
	}

	assign, ok := forStmt.Update.(*ast.AssignStmt)
	if !ok {
		t.Fatalf("for update: expected AssignStmt, got %T", forStmt.Update)
	}
	target := assign.Target.(*ast.IdentExpr)
	if target.Name != "i" {
		t.Errorf("for update target: got %q", target.Name)
	}

	if len(forStmt.Body.Stmts) != 1 {
		t.Fatalf("for body: expected 1 stmt, got %d", len(forStmt.Body.Stmts))
	}
	_, ok = forStmt.Body.Stmts[0].(*ast.BreakStmt)
	if !ok {
		t.Errorf("for body: expected BreakStmt, got %T", forStmt.Body.Stmts[0])
	}
}

// ---------------------------------------------------------------------------
// Assignments
// ---------------------------------------------------------------------------

func TestParseAssignment(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { x = 1; }")
	assign := prog.Functions[0].Body.Stmts[0].(*ast.AssignStmt)
	target := assign.Target.(*ast.IdentExpr)
	if target.Name != "x" {
		t.Errorf("assign target: got %q", target.Name)
	}
	val := assign.Value.(*ast.IntLitExpr)
	if val.Value != "1" {
		t.Errorf("assign value: got %q", val.Value)
	}
}

func TestParseAssignMember(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { obj.field = 42; }")
	assign := prog.Functions[0].Body.Stmts[0].(*ast.AssignStmt)
	mem := assign.Target.(*ast.MemberExpr)
	if mem.Field != "field" {
		t.Errorf("member field: got %q", mem.Field)
	}
}

// ---------------------------------------------------------------------------
// Expression statements and calls
// ---------------------------------------------------------------------------

func TestParseExprStmtCall(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { push(0); }")
	es := prog.Functions[0].Body.Stmts[0].(*ast.ExprStmt)
	call := es.Expression.(*ast.CallExpr)
	callee := call.Callee.(*ast.IdentExpr)
	if callee.Name != "push" {
		t.Errorf("callee: got %q", callee.Name)
	}
	if len(call.Args) != 1 {
		t.Fatalf("expected 1 arg, got %d", len(call.Args))
	}
}

func TestParseMemberCall(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { win32.ExitProcess(0); }")
	es := prog.Functions[0].Body.Stmts[0].(*ast.ExprStmt)
	call := es.Expression.(*ast.CallExpr)
	mem := call.Callee.(*ast.MemberExpr)
	if mem.Field != "ExitProcess" {
		t.Errorf("member field: got %q", mem.Field)
	}
	obj := mem.Object.(*ast.IdentExpr)
	if obj.Name != "win32" {
		t.Errorf("member object: got %q", obj.Name)
	}
}

func TestParseNestedCall(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { push(len(msg)); }")
	es := prog.Functions[0].Body.Stmts[0].(*ast.ExprStmt)
	outerCall := es.Expression.(*ast.CallExpr)
	innerCall := outerCall.Args[0].(*ast.CallExpr)
	if ast.ExprString(innerCall.Callee) != "len" {
		t.Errorf("inner callee: got %q", ast.ExprString(innerCall.Callee))
	}
}

func TestParseCallNoArgs(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { ret(); }")
	es := prog.Functions[0].Body.Stmts[0].(*ast.ExprStmt)
	call := es.Expression.(*ast.CallExpr)
	if len(call.Args) != 0 {
		t.Errorf("expected 0 args, got %d", len(call.Args))
	}
}

func TestParseCallMultipleArgs(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { foo(a, b, c); }")
	es := prog.Functions[0].Body.Stmts[0].(*ast.ExprStmt)
	call := es.Expression.(*ast.CallExpr)
	if len(call.Args) != 3 {
		t.Fatalf("expected 3 args, got %d", len(call.Args))
	}
}

// ---------------------------------------------------------------------------
// Expression parsing: precedence, associativity, unary, grouping
// ---------------------------------------------------------------------------

func TestParsePrecedenceMultiplyOverAdd(t *testing.T) {
	prog := parseInput(t, "fn f() -> i32 { return a + b * c; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	s := ast.ExprString(ret.Value)
	expected := "(a + (b * c))"
	if s != expected {
		t.Errorf("got %q, want %q", s, expected)
	}
}

func TestParsePrecedenceGrouping(t *testing.T) {
	prog := parseInput(t, "fn f() -> i32 { return (a + b) * c; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	s := ast.ExprString(ret.Value)
	// GroupExpr wraps its inner expression, so we get double parens in the
	// debug string: the outer pair from GroupExpr, the inner from BinaryExpr.
	expected := "(((a + b)) * c)"
	if s != expected {
		t.Errorf("got %q, want %q", s, expected)
	}
}

func TestParseLeftAssociativity(t *testing.T) {
	prog := parseInput(t, "fn f() -> i32 { return a - b - c; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	s := ast.ExprString(ret.Value)
	expected := "((a - b) - c)"
	if s != expected {
		t.Errorf("got %q, want %q", s, expected)
	}
}

func TestParseLogicalPrecedence(t *testing.T) {
	prog := parseInput(t, "fn f() -> bool { return a || b && c; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	s := ast.ExprString(ret.Value)
	expected := "(a || (b && c))"
	if s != expected {
		t.Errorf("got %q, want %q", s, expected)
	}
}

func TestParseComparisonPrecedence(t *testing.T) {
	prog := parseInput(t, "fn f() -> bool { return a + 1 > b * 2; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	s := ast.ExprString(ret.Value)
	expected := "((a + 1) > (b * 2))"
	if s != expected {
		t.Errorf("got %q, want %q", s, expected)
	}
}

func TestParseUnaryNot(t *testing.T) {
	prog := parseInput(t, "fn f() -> bool { return !x; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	u := ret.Value.(*ast.UnaryExpr)
	if u.Op != "!" {
		t.Errorf("op: got %q", u.Op)
	}
}

func TestParseUnaryMinus(t *testing.T) {
	prog := parseInput(t, "fn f() -> i32 { return -x; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	u := ret.Value.(*ast.UnaryExpr)
	if u.Op != "-" {
		t.Errorf("op: got %q", u.Op)
	}
}

func TestParseAddressOf(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { push(&x); }")
	es := prog.Functions[0].Body.Stmts[0].(*ast.ExprStmt)
	call := es.Expression.(*ast.CallExpr)
	addr := call.Args[0].(*ast.AddressOfExpr)
	ident := addr.Operand.(*ast.IdentExpr)
	if ident.Name != "x" {
		t.Errorf("address-of operand: got %q", ident.Name)
	}
}

func TestParseBoolLiterals(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { let a: bool = true; let b: bool = false; }")
	stmts := prog.Functions[0].Body.Stmts
	a := stmts[0].(*ast.LetStmt).Value.(*ast.BoolLitExpr)
	b := stmts[1].(*ast.LetStmt).Value.(*ast.BoolLitExpr)
	if !a.Value {
		t.Error("expected true")
	}
	if b.Value {
		t.Error("expected false")
	}
}

func TestParseFloatLiteral(t *testing.T) {
	prog := parseInput(t, "fn f() -> f64 { return 3.14; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	f := ret.Value.(*ast.FloatLitExpr)
	if f.Value != "3.14" {
		t.Errorf("float: got %q", f.Value)
	}
}

// ---------------------------------------------------------------------------
// Type keywords used as identifiers in expressions
// ---------------------------------------------------------------------------

func TestParseTypeKeywordAsIdent(t *testing.T) {
	prog := parseInput(t, "fn f() -> void { str = str + '\\r\\n'; }")
	assign := prog.Functions[0].Body.Stmts[0].(*ast.AssignStmt)
	target := assign.Target.(*ast.IdentExpr)
	if target.Name != "str" {
		t.Errorf("target: got %q", target.Name)
	}
	bin := assign.Value.(*ast.BinaryExpr)
	left := bin.Left.(*ast.IdentExpr)
	if left.Name != "str" {
		t.Errorf("left: got %q", left.Name)
	}
}

// ---------------------------------------------------------------------------
// Equality operators
// ---------------------------------------------------------------------------

func TestParseEqualityOperator(t *testing.T) {
	prog := parseInput(t, "fn f() -> bool { return x == 0; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	bin := ret.Value.(*ast.BinaryExpr)
	if bin.Op != "==" {
		t.Errorf("op: got %q, want %q", bin.Op, "==")
	}
}

func TestParseNotEqual(t *testing.T) {
	prog := parseInput(t, "fn f() -> bool { return x != 1; }")
	ret := prog.Functions[0].Body.Stmts[0].(*ast.ReturnStmt)
	bin := ret.Value.(*ast.BinaryExpr)
	if bin.Op != "!=" {
		t.Errorf("op: got %q, want %q", bin.Op, "!=")
	}
}

// ---------------------------------------------------------------------------
// Error recovery
// ---------------------------------------------------------------------------

func TestParseErrorMissingSemicolon(t *testing.T) {
	_, errs := parseInputExpectErrors(t, "fn f() -> void { let x: i32 = 5 }")
	if len(errs) == 0 {
		t.Fatal("expected parse errors for missing semicolon")
	}
}

func TestParseErrorRecovery(t *testing.T) {
	prog, errs := parseInputExpectErrors(t, "fn bad() -> void { let = ; } fn good() -> void { return; }")
	if len(errs) == 0 {
		t.Fatal("expected parse errors")
	}
	found := false
	for _, fn := range prog.Functions {
		if fn.Name == "good" {
			found = true
		}
	}
	if !found {
		t.Error("expected to find function 'good' after error recovery")
	}
}

// ---------------------------------------------------------------------------
// Debug string smoke test
// ---------------------------------------------------------------------------

func TestDebugString(t *testing.T) {
	prog := parseInput(t, "module test; import io; fn main() -> i32 { let x: i32 = 42; return x; }")
	s := ast.DebugString(prog)
	if len(s) == 0 {
		t.Fatal("DebugString returned empty string")
	}
	t.Logf("DebugString output:\n%s", s)
}

// ---------------------------------------------------------------------------
// Integration: parse example.nov
// ---------------------------------------------------------------------------

func TestParseExampleNovFile(t *testing.T) {
	content, err := os.ReadFile("../../example.nov")
	if err != nil {
		t.Skipf("skipping: %v", err)
	}

	tokens, lexErrs := lexer.Lex(string(content))
	if len(lexErrs) > 0 {
		for _, e := range lexErrs {
			t.Errorf("lex error: %s", e.Error())
		}
		t.FailNow()
	}

	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		for _, e := range parseErrs {
			t.Errorf("parse error: %s", e.Error())
		}
		t.FailNow()
	}

	if prog.Module == nil || prog.Module.Name != "hello_world_macos_silicon" {
		t.Errorf("module: got %v", prog.Module)
	}

	if len(prog.Imports) != 0 {
		t.Errorf("imports: got %v", prog.Imports)
	}

	if len(prog.Functions) != 4 {
		t.Fatalf("expected 4 functions, got %d", len(prog.Functions))
	}
	if prog.Functions[0].Name != "len" {
		t.Errorf("fn[0] name: got %q", prog.Functions[0].Name)
	}
	if prog.Functions[1].Name != "print" {
		t.Errorf("fn[1] name: got %q", prog.Functions[1].Name)
	}
	if prog.Functions[2].Name != "main" {
		t.Errorf("fn[2] name: got %q", prog.Functions[2].Name)
	}
	if prog.Functions[3].Name != "exit" {
		t.Errorf("fn[3] name: got %q", prog.Functions[3].Name)
	}

	t.Logf("AST:\n%s", ast.DebugString(prog))
}

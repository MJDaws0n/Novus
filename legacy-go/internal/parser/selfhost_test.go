package parser_test

import (
	"novus/internal/ast"
	"testing"
)

func TestForOptionalClauses(t *testing.T) {
	for _, source := range []string{
		`fn main() -> void { for (;;) { break; } }`,
		`fn main() -> void { let i: i32 = 0; for (; i < 2; i++) {} }`,
		`fn main() -> void { let i: i32 = 0; for (; i < 2;) { i++; } }`,
	} {
		program := parseInput(t, source)
		body := program.Functions[0].Body.Stmts
		loop := body[len(body)-1].(*ast.ForStmt)
		if loop.Init != nil || loop.Condition == nil {
			t.Fatalf("invalid loop: %#v", loop)
		}
	}
}

func TestNullLiteral(t *testing.T) {
	program := parseInput(t, `class Link { pub let next: Link = null; }`)
	if _, ok := program.Classes[0].Fields[0].Value.(*ast.NullLitExpr); !ok {
		t.Fatal("null was not parsed as a null literal")
	}
}

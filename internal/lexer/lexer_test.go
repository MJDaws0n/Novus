package lexer

import (
	"os"
	"strings"
	"testing"
)

func tokenTypes(tokens []Token) []string {
	out := make([]string, len(tokens))
	for i, t := range tokens {
		out[i] = t.Type
	}
	return out
}

func TestKeywordsAndIdentifiers(t *testing.T) {
	tokens, errs := Lex("module fn let return import void str u32 i32 if else for while break continue foo _bar baz42")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	expected := []struct {
		typ string
		val string
	}{
		{MODULE, "module"},
		{FN, "fn"},
		{LET, "let"},
		{RETURN, "return"},
		{IMPORT, "import"},
		{VOID, "void"},
		{STR, "str"},
		{U32, "u32"},
		{I32, "i32"},
		{IF, "if"},
		{ELSE, "else"},
		{FOR, "for"},
		{WHILE, "while"},
		{BREAK, "break"},
		{CONTINUE, "continue"},
		{IDENT, "foo"},
		{IDENT, "_bar"},
		{IDENT, "baz42"},
		{EOF, ""},
	}
	if len(tokens) != len(expected) {
		t.Fatalf("token count: got %d, want %d", len(tokens), len(expected))
	}
	for i, exp := range expected {
		if tokens[i].Type != exp.typ || tokens[i].Value != exp.val {
			t.Errorf("token[%d]: got (%s, %q), want (%s, %q)",
				i, tokens[i].Type, tokens[i].Value, exp.typ, exp.val)
		}
	}
}

func TestIntegerLiterals(t *testing.T) {
	tokens, errs := Lex("0 42 0xFF 0X1A")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	expected := []string{"0", "42", "0xFF", "0X1A"}
	for i, exp := range expected {
		if tokens[i].Type != INT || tokens[i].Value != exp {
			t.Errorf("token[%d]: got (%s, %q), want (INT, %q)",
				i, tokens[i].Type, tokens[i].Value, exp)
		}
	}
}

func TestFloatLiterals(t *testing.T) {
	tokens, errs := Lex("3.14 0.5 1.0 100.001")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	expected := []string{"3.14", "0.5", "1.0", "100.001"}
	for i, exp := range expected {
		if tokens[i].Type != FLOAT || tokens[i].Value != exp {
			t.Errorf("token[%d]: got (%s, %q), want (FLOAT, %q)",
				i, tokens[i].Type, tokens[i].Value, exp)
		}
	}
}

func TestFloatScientificNotation(t *testing.T) {
	tokens, errs := Lex("1.5e10 2.0E-3 3e4 5E+2")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	expected := []string{"1.5e10", "2.0E-3", "3e4", "5E+2"}
	for i, exp := range expected {
		if tokens[i].Type != FLOAT || tokens[i].Value != exp {
			t.Errorf("token[%d]: got (%s, %q), want (FLOAT, %q)",
				i, tokens[i].Type, tokens[i].Value, exp)
		}
	}
}

func TestNumberDotIdentifier(t *testing.T) {
	// "42.method" should be INT DOT IDENT, not a float
	tokens, errs := Lex("42.method")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	types := tokenTypes(tokens)
	expected := []string{INT, DOT, IDENT, EOF}
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	for i, exp := range expected {
		if types[i] != exp {
			t.Errorf("token[%d]: got %s, want %s (value=%q)", i, types[i], exp, tokens[i].Value)
		}
	}
}

func TestNumericTypeKeywords(t *testing.T) {
	tokens, errs := Lex("u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 bool")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	expected := []struct {
		typ string
		val string
	}{
		{U8, "u8"}, {U16, "u16"}, {U32, "u32"}, {U64, "u64"},
		{I8, "i8"}, {I16, "i16"}, {I32, "i32"}, {I64, "i64"},
		{F32, "f32"}, {F64, "f64"}, {BOOL, "bool"},
		{EOF, ""},
	}
	if len(tokens) != len(expected) {
		t.Fatalf("token count: got %d, want %d", len(tokens), len(expected))
	}
	for i, exp := range expected {
		if tokens[i].Type != exp.typ || tokens[i].Value != exp.val {
			t.Errorf("token[%d]: got (%s, %q), want (%s, %q)",
				i, tokens[i].Type, tokens[i].Value, exp.typ, exp.val)
		}
	}
}

func TestBoolLiterals(t *testing.T) {
	tokens, errs := Lex("true false")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if tokens[0].Type != TRUE || tokens[0].Value != "true" {
		t.Errorf("token[0]: got (%s, %q), want (TRUE, \"true\")", tokens[0].Type, tokens[0].Value)
	}
	if tokens[1].Type != FALSE || tokens[1].Value != "false" {
		t.Errorf("token[1]: got (%s, %q), want (FALSE, \"false\")", tokens[1].Type, tokens[1].Value)
	}
}

func TestStringDoubleQuote(t *testing.T) {
	tokens, errs := Lex(`"Hello, World!"`)
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if tokens[0].Type != STRING || tokens[0].Value != `"Hello, World!"` {
		t.Errorf("got (%s, %q), want (STRING, %q)", tokens[0].Type, tokens[0].Value, `"Hello, World!"`)
	}
}

func TestStringSingleQuote(t *testing.T) {
	tokens, errs := Lex(`'single'`)
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if tokens[0].Type != STRING || tokens[0].Value != `'single'` {
		t.Errorf("got (%s, %q), want (STRING, %q)", tokens[0].Type, tokens[0].Value, `'single'`)
	}
}

func TestStringEscapeSequences(t *testing.T) {
	input := `"esc: \n\r\t\\\"\0"`
	tokens, errs := Lex(input)
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if tokens[0].Type != STRING {
		t.Errorf("got type %s, want STRING", tokens[0].Type)
	}
	if tokens[0].Value != input {
		t.Errorf("got value %q, want %q", tokens[0].Value, input)
	}
}

func TestStringEscapedQuoteInside(t *testing.T) {
	input := `'\''`
	tokens, errs := Lex(input)
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if tokens[0].Type != STRING || tokens[0].Value != input {
		t.Errorf("got (%s, %q), want (STRING, %q)", tokens[0].Type, tokens[0].Value, input)
	}
}

func TestDelimitersAndOperators(t *testing.T) {
	tokens, errs := Lex("( ) { } ; : , . -> = + - * / % & ! == != < > <= >= && ||")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	expected := []string{
		LPAREN, RPAREN, LBRACE, RBRACE,
		SEMICOLON, COLON, COMMA, DOT,
		ARROW, ASSIGN, PLUS, MINUS, STAR, SLASH, PERCENT, AMPERSAND, BANG,
		EQ, NEQ, LT, GT, LTE, GTE,
		AND, OR,
		EOF,
	}
	types := tokenTypes(tokens)
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	for i, exp := range expected {
		if types[i] != exp {
			t.Errorf("token[%d]: got type %s, want %s", i, types[i], exp)
		}
	}
}

func TestArrowVsMinus(t *testing.T) {
	tokens, errs := Lex("-> -x")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if tokens[0].Type != ARROW || tokens[0].Value != "->" {
		t.Errorf("token[0]: got (%s, %q), want (ARROW, %q)", tokens[0].Type, tokens[0].Value, "->")
	}
	if tokens[1].Type != MINUS || tokens[1].Value != "-" {
		t.Errorf("token[1]: got (%s, %q), want (MINUS, %q)", tokens[1].Type, tokens[1].Value, "-")
	}
}

func TestComparisonOperatorAmbiguity(t *testing.T) {
	// Make sure = vs == and ! vs != are distinguished correctly
	tokens, errs := Lex("x = 1; x == 1; x != 2; x <= 3; x >= 4")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	types := tokenTypes(tokens)
	expected := []string{
		IDENT, ASSIGN, INT, SEMICOLON,
		IDENT, EQ, INT, SEMICOLON,
		IDENT, NEQ, INT, SEMICOLON,
		IDENT, LTE, INT, SEMICOLON,
		IDENT, GTE, INT,
		EOF,
	}
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	for i, exp := range expected {
		if types[i] != exp {
			t.Errorf("token[%d]: got %s, want %s (value=%q)", i, types[i], exp, tokens[i].Value)
		}
	}
}

func TestControlFlowKeywords(t *testing.T) {
	tokens, errs := Lex("if else for while break continue")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	expected := []struct {
		typ string
		val string
	}{
		{IF, "if"}, {ELSE, "else"}, {FOR, "for"},
		{WHILE, "while"}, {BREAK, "break"}, {CONTINUE, "continue"},
		{EOF, ""},
	}
	if len(tokens) != len(expected) {
		t.Fatalf("token count: got %d, want %d", len(tokens), len(expected))
	}
	for i, exp := range expected {
		if tokens[i].Type != exp.typ || tokens[i].Value != exp.val {
			t.Errorf("token[%d]: got (%s, %q), want (%s, %q)",
				i, tokens[i].Type, tokens[i].Value, exp.typ, exp.val)
		}
	}
}

func TestIfElseSnippet(t *testing.T) {
	input := `if (x > 0) { return 1; } else { return 0; }`
	tokens, errs := Lex(input)
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	types := tokenTypes(tokens)
	expected := []string{
		IF, LPAREN, IDENT, GT, INT, RPAREN, LBRACE,
		RETURN, INT, SEMICOLON, RBRACE,
		ELSE, LBRACE,
		RETURN, INT, SEMICOLON, RBRACE,
		EOF,
	}
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	for i, exp := range expected {
		if types[i] != exp {
			t.Errorf("token[%d]: got %s, want %s", i, types[i], exp)
		}
	}
}

func TestWhileLoopSnippet(t *testing.T) {
	input := `while (i < 10) { i = i + 1; }`
	tokens, errs := Lex(input)
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	types := tokenTypes(tokens)
	expected := []string{
		WHILE, LPAREN, IDENT, LT, INT, RPAREN, LBRACE,
		IDENT, ASSIGN, IDENT, PLUS, INT, SEMICOLON,
		RBRACE, EOF,
	}
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	for i, exp := range expected {
		if types[i] != exp {
			t.Errorf("token[%d]: got %s, want %s", i, types[i], exp)
		}
	}
}

func TestForLoopSnippet(t *testing.T) {
	input := `for (let i: i32 = 0; i < 10; i = i + 1) { break; }`
	tokens, errs := Lex(input)
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	types := tokenTypes(tokens)
	expected := []string{
		FOR, LPAREN,
		LET, IDENT, COLON, I32, ASSIGN, INT, SEMICOLON,
		IDENT, LT, INT, SEMICOLON,
		IDENT, ASSIGN, IDENT, PLUS, INT,
		RPAREN, LBRACE, BREAK, SEMICOLON, RBRACE, EOF,
	}
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	for i, exp := range expected {
		if types[i] != exp {
			t.Errorf("token[%d]: got %s, want %s", i, types[i], exp)
		}
	}
}

func TestLogicalOperators(t *testing.T) {
	input := `if (a && b || !c) {}`
	tokens, errs := Lex(input)
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	types := tokenTypes(tokens)
	expected := []string{
		IF, LPAREN, IDENT, AND, IDENT, OR, BANG, IDENT, RPAREN,
		LBRACE, RBRACE, EOF,
	}
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	for i, exp := range expected {
		if types[i] != exp {
			t.Errorf("token[%d]: got %s, want %s", i, types[i], exp)
		}
	}
}

func TestSingleLineComment(t *testing.T) {
	tokens, errs := Lex("let x // comment\nlet y")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	types := tokenTypes(tokens)
	expected := []string{LET, IDENT, LET, IDENT, EOF}
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	if tokens[2].Line != 2 {
		t.Errorf("second 'let' should be on line 2, got line %d", tokens[2].Line)
	}
}

func TestBlockComment(t *testing.T) {
	tokens, errs := Lex("let /* skip\nthis */ x")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	types := tokenTypes(tokens)
	expected := []string{LET, IDENT, EOF}
	if len(types) != len(expected) {
		t.Fatalf("token count: got %d, want %d; types: %v", len(types), len(expected), types)
	}
	if tokens[1].Line != 2 {
		t.Errorf("'x' should be on line 2, got line %d", tokens[1].Line)
	}
}

func TestLineColumnTracking(t *testing.T) {
	tokens, errs := Lex("module hello_win32;")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if tokens[0].Column != 1 {
		t.Errorf("'module' column: got %d, want 1", tokens[0].Column)
	}
	if tokens[1].Column != 8 {
		t.Errorf("'hello_win32' column: got %d, want 8", tokens[1].Column)
	}
	if tokens[2].Column != 19 {
		t.Errorf("';' column: got %d, want 19", tokens[2].Column)
	}
}

func TestUnterminatedString(t *testing.T) {
	_, errs := Lex(`"hello`)
	if len(errs) == 0 {
		t.Fatal("expected error for unterminated string")
	}
	if !strings.Contains(errs[0].Message, "unterminated") {
		t.Errorf("error message should mention 'unterminated', got: %s", errs[0].Message)
	}
}

func TestNewlineInString(t *testing.T) {
	_, errs := Lex("\"hello\nworld\"")
	if len(errs) == 0 {
		t.Fatal("expected error for newline in string")
	}
	if !strings.Contains(errs[0].Message, "newline") {
		t.Errorf("error message should mention 'newline', got: %s", errs[0].Message)
	}
}

func TestUnterminatedBlockComment(t *testing.T) {
	_, errs := Lex("/* oops")
	if len(errs) == 0 {
		t.Fatal("expected error for unterminated block comment")
	}
	if !strings.Contains(errs[0].Message, "unterminated") {
		t.Errorf("error message should mention 'unterminated', got: %s", errs[0].Message)
	}
}

func TestInvalidEscapeSequence(t *testing.T) {
	tokens, errs := Lex(`"bad\q"`)
	if len(errs) == 0 {
		t.Fatal("expected error for invalid escape sequence")
	}
	if !strings.Contains(errs[0].Message, "invalid escape") {
		t.Errorf("error message should mention 'invalid escape', got: %s", errs[0].Message)
	}
	if len(tokens) < 2 || tokens[0].Type != STRING {
		t.Errorf("string token should still be emitted after invalid escape, got types: %v", tokenTypes(tokens))
	}
}

func TestUnknownCharacter(t *testing.T) {
	tokens, errs := Lex("let # x")
	if len(errs) == 0 {
		t.Fatal("expected error for unknown character '#'")
	}
	if errs[0].Lexeme != "#" {
		t.Errorf("error lexeme: got %q, want %q", errs[0].Lexeme, "#")
	}
	types := tokenTypes(tokens)
	if len(types) != 3 {
		t.Errorf("expected 3 tokens after recovery, got %d: %v", len(types), types)
	}
}

func TestEmptyInput(t *testing.T) {
	tokens, errs := Lex("")
	if len(errs) > 0 {
		t.Fatalf("unexpected errors: %v", errs)
	}
	if len(tokens) != 1 || tokens[0].Type != EOF {
		t.Errorf("empty input should produce only EOF, got %v", tokenTypes(tokens))
	}
}

func TestMultipleErrorRecovery(t *testing.T) {
	tokens, errs := Lex("@ \"oops\nlet x")
	if len(errs) < 2 {
		t.Fatalf("expected at least 2 errors, got %d: %v", len(errs), errs)
	}
	found := false
	for _, tok := range tokens {
		if tok.Type == LET {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected to find LET token after error recovery")
	}
}

func TestIndexingTokens(t *testing.T) {
	tokens, errs := Lex("a[0];")
	if len(errs) > 0 {
		t.Fatalf("unexpected lex errors: %v", errs)
	}
	want := []string{IDENT, LBRACKET, INT, RBRACKET, SEMICOLON, EOF}
	got := tokenTypes(tokens)
	if len(got) != len(want) {
		t.Fatalf("token count: got %d, want %d (%v)", len(got), len(want), got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("token[%d]: got %s, want %s", i, got[i], want[i])
		}
	}
}

func TestExampleNovFile(t *testing.T) {
	content, err := os.ReadFile("../../example.nov")
	if err != nil {
		t.Skipf("skipping integration test: %v", err)
	}
	tokens, errs := Lex(string(content))
	if len(errs) > 0 {
		for _, e := range errs {
			t.Errorf("lex error: %s", e.Error())
		}
		t.FailNow()
	}
	for i, tok := range tokens {
		if tok.Type == ILLEGAL {
			t.Errorf("token[%d]: unexpected ILLEGAL token %q at line %d, col %d",
				i, tok.Value, tok.Line, tok.Column)
		}
	}
	expectedStart := []struct {
		typ string
		val string
	}{
		{MODULE, "module"},
		{IDENT, "hello_world_macos_silicon"},
		{SEMICOLON, ";"},
	}
	for i, exp := range expectedStart {
		if i >= len(tokens) {
			t.Fatalf("ran out of tokens at index %d", i)
		}
		if tokens[i].Type != exp.typ || tokens[i].Value != exp.val {
			t.Errorf("token[%d]: got (%s, %q), want (%s, %q)",
				i, tokens[i].Type, tokens[i].Value, exp.typ, exp.val)
		}
	}
	last := tokens[len(tokens)-1]
	if last.Type != EOF {
		t.Errorf("last token should be EOF, got %s", last.Type)
	}
	foundHelloWorld := false
	foundIndexing := false
	foundReturn := false
	for _, tok := range tokens {
		if tok.Type == STRING && tok.Value == `"Hello, World!"` {
			foundHelloWorld = true
		}
		if tok.Type == LBRACKET {
			foundIndexing = true
		}
		if tok.Type == RETURN {
			foundReturn = true
		}
	}
	if !foundHelloWorld {
		t.Error("did not find string token for Hello, World!")
	}
	if !foundIndexing {
		t.Error("did not find '[' token (expected indexing in example.nov)")
	}
	if !foundReturn {
		t.Error("did not find RETURN keyword")
	}
}

package parser

import (
	"fmt"
	"novus/internal/ast"
	"novus/internal/lexer"
)

// ---------------------------------------------------------------------------
// Precedence levels for Pratt expression parsing
// ---------------------------------------------------------------------------

const (
	precNone       = iota
	precOr         // ||
	precAnd        // &&
	precBitOr      // |
	precBitXor     // ^
	precBitAnd     // &
	precEquality   // == !=
	precComparison // < > <= >=
	precShift      // << >>
	precAdditive   // + -
	precMultiply   // * / %
	precUnary      // ! - & ~
	precCall       // () . []
)

// ---------------------------------------------------------------------------
// ParseError
// ---------------------------------------------------------------------------

// ParseError represents a single error found during parsing.
type ParseError struct {
	Message string
	Line    int
	Column  int
}

func (e ParseError) Error() string {
	return fmt.Sprintf("line %d, col %d: %s", e.Line, e.Column, e.Message)
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

// Parser holds the state for a single parse pass over a token stream.
type Parser struct {
	tokens []lexer.Token
	pos    int
	errors []ParseError
}

// Parse is the main entry point. It takes a token slice (as produced by
// lexer.Lex) and returns an AST program plus any parse errors collected.
func Parse(tokens []lexer.Token) (*ast.Program, []ParseError) {
	p := &Parser{tokens: tokens, pos: 0}
	prog := p.parseProgram()
	return prog, p.errors
}

// ---------------------------------------------------------------------------
// Token helpers
// ---------------------------------------------------------------------------

// peek returns the current token without consuming it.
func (p *Parser) peek() lexer.Token {
	if p.pos < len(p.tokens) {
		return p.tokens[p.pos]
	}
	return lexer.Token{Type: lexer.EOF}
}

// peekAt returns the token at a given offset from the current position.
func (p *Parser) peekAt(offset int) lexer.Token {
	idx := p.pos + offset
	if idx >= 0 && idx < len(p.tokens) {
		return p.tokens[idx]
	}
	return lexer.Token{Type: lexer.EOF}
}

// advance consumes and returns the current token.
func (p *Parser) advance() lexer.Token {
	tok := p.peek()
	if tok.Type != lexer.EOF {
		p.pos++
	}
	return tok
}

// previous returns the most recently consumed token.
func (p *Parser) previous() lexer.Token {
	if p.pos > 0 {
		return p.tokens[p.pos-1]
	}
	return lexer.Token{Type: lexer.EOF}
}

// check returns true if the current token has the given type.
func (p *Parser) check(typ string) bool {
	return p.peek().Type == typ
}

// match consumes the current token if it matches any of the given types.
func (p *Parser) match(types ...string) bool {
	for _, t := range types {
		if p.check(t) {
			p.advance()
			return true
		}
	}
	return false
}

// expect consumes the current token if it matches typ; otherwise it records
// an error and returns the current token WITHOUT advancing.
func (p *Parser) expect(typ string, msg string) lexer.Token {
	if p.check(typ) {
		return p.advance()
	}
	tok := p.peek()
	p.addError(tok, fmt.Sprintf("%s (got %s %q)", msg, tok.Type, tok.Value))
	return tok
}

// addError appends a ParseError at the given token's location.
func (p *Parser) addError(tok lexer.Token, msg string) {
	p.errors = append(p.errors, ParseError{
		Message: msg,
		Line:    tok.Line,
		Column:  tok.Column,
	})
}

// synchronize advances past tokens until it reaches a likely statement
// boundary, allowing the parser to recover from an error and keep going.
func (p *Parser) synchronize() {
	p.advance()
	for !p.check(lexer.EOF) {
		// If we just passed a semicolon, we're at a fresh statement.
		if p.previous().Type == lexer.SEMICOLON {
			return
		}
		// If the current token starts a new construct, stop here.
		switch p.peek().Type {
		case lexer.FN, lexer.LET, lexer.IF, lexer.WHILE, lexer.FOR,
			lexer.RETURN, lexer.BREAK, lexer.CONTINUE,
			lexer.MODULE, lexer.IMPORT, lexer.RBRACE:
			return
		}
		p.advance()
	}
}

// position converts a token into an ast.Position.
func (p *Parser) position(tok lexer.Token) ast.Position {
	return ast.Position{Line: tok.Line, Column: tok.Column}
}

// isTypeKeyword returns true for type-keyword token types (i32, str, …).
func isTypeKeyword(typ string) bool {
	switch typ {
	case lexer.VOID, lexer.BOOL, lexer.STR,
		lexer.U8, lexer.U16, lexer.U32, lexer.U64,
		lexer.I8, lexer.I16, lexer.I32, lexer.I64,
		lexer.F32, lexer.F64:
		return true
	}
	return false
}

// =========================================================================
// Top-level parsing
// =========================================================================

func (p *Parser) parseProgram() *ast.Program {
	prog := &ast.Program{Pos: p.position(p.peek())}

	// Optional module declaration.
	if p.check(lexer.MODULE) {
		prog.Module = p.parseModuleDecl()
	}

	// Zero or more import declarations.
	for p.check(lexer.IMPORT) {
		prog.Imports = append(prog.Imports, p.parseImportDecl())
	}

	// Zero or more top-level declarations (functions or global variables).
	for !p.check(lexer.EOF) {
		if p.check(lexer.FN) {
			fn := p.parseFnDecl()
			if fn != nil {
				prog.Functions = append(prog.Functions, fn)
			}
		} else if p.check(lexer.LET) {
			g := p.parseGlobalVar()
			if g != nil {
				prog.Globals = append(prog.Globals, g)
			}
		} else {
			p.addError(p.peek(), fmt.Sprintf("expected function or variable declaration, got %s", p.peek().Type))
			p.synchronize()
		}
	}

	return prog
}

// parseGlobalVar parses a top-level: let <name>: <type> = <value>;
func (p *Parser) parseGlobalVar() *ast.GlobalVar {
	tok := p.advance() // consume LET
	name := p.expect(lexer.IDENT, "expected variable name")
	p.expect(lexer.COLON, "expected ':' after variable name")
	typ := p.parseType()
	p.expect(lexer.ASSIGN, "expected '=' in global variable declaration")
	value := p.parseExpression()
	p.expect(lexer.SEMICOLON, "expected ';' after global variable declaration")
	return &ast.GlobalVar{
		Name:  name.Value,
		Type:  typ,
		Value: value,
		Pos:   p.position(tok),
	}
}

func (p *Parser) parseModuleDecl() *ast.ModuleDecl {
	tok := p.advance() // consume MODULE
	name := p.expect(lexer.IDENT, "expected module name")
	p.expect(lexer.SEMICOLON, "expected ';' after module declaration")
	return &ast.ModuleDecl{Name: name.Value, Pos: p.position(tok)}
}

func (p *Parser) parseImportDecl() *ast.ImportDecl {
	tok := p.advance() // consume IMPORT

	// Parse the import path which can be:
	//   import foo;                      → path="foo"
	//   import foo/bar/baz;              → path="foo/bar/baz"
	//   import ../lib/foo;               → path="../lib/foo"
	//   import foo bar;                  → path="foo", alias="bar"
	//   import foo[fn1, fn2];            → path="foo", selectFns=["fn1","fn2"]
	//   import foo[fn1, fn2] bar;        → path="foo", selectFns=["fn1","fn2"], alias="bar"
	//   import foo/bar baz;              → path="foo/bar", alias="baz"
	//   import foo/bar[fn1] baz;         → path="foo/bar", selectFns=["fn1"], alias="baz"

	// Parse first path segment: can be ".." (DOT DOT) or an IDENT.
	path := ""
	if p.check(lexer.DOT) && p.peekAt(1).Type == lexer.DOT {
		p.advance() // consume first '.'
		p.advance() // consume second '.'
		path = ".."
	} else {
		first := p.expect(lexer.IDENT, "expected import path")
		path = first.Value
	}

	// Consume additional path segments: /ident or /.. sequences.
	for p.check(lexer.SLASH) {
		p.advance() // consume '/'
		if p.check(lexer.DOT) && p.peekAt(1).Type == lexer.DOT {
			p.advance() // consume first '.'
			p.advance() // consume second '.'
			path += "/.."
		} else {
			seg := p.expect(lexer.IDENT, "expected path segment after '/'")
			path += "/" + seg.Value
		}
	}

	// Optional selective imports: [fn1, fn2, ...]
	var selectFns []string
	if p.check(lexer.LBRACKET) {
		p.advance() // consume '['
		if !p.check(lexer.RBRACKET) {
			fn := p.expect(lexer.IDENT, "expected function name in import selector")
			selectFns = append(selectFns, fn.Value)
			for p.match(lexer.COMMA) {
				fn = p.expect(lexer.IDENT, "expected function name in import selector")
				selectFns = append(selectFns, fn.Value)
			}
		}
		p.expect(lexer.RBRACKET, "expected ']' after import selector")
	}

	// Optional alias: the next IDENT before the semicolon.
	alias := ""
	if p.check(lexer.IDENT) {
		alias = p.advance().Value
	}

	p.expect(lexer.SEMICOLON, "expected ';' after import declaration")
	return &ast.ImportDecl{
		Path:      path,
		SelectFns: selectFns,
		Alias:     alias,
		Pos:       p.position(tok),
	}
}

func (p *Parser) parseFnDecl() *ast.FnDecl {
	tok := p.advance() // consume FN
	name := p.expect(lexer.IDENT, "expected function name")
	p.expect(lexer.LPAREN, "expected '(' after function name")

	params := p.parseParamList()

	p.expect(lexer.RPAREN, "expected ')' after parameters")
	p.expect(lexer.ARROW, "expected '->' before return type")

	retType := p.parseType()
	body := p.parseBlock()

	return &ast.FnDecl{
		Name:       name.Value,
		Params:     params,
		ReturnType: retType,
		Body:       body,
		Pos:        p.position(tok),
	}
}

func (p *Parser) parseParamList() []*ast.Param {
	var params []*ast.Param

	if p.check(lexer.RPAREN) {
		return params
	}

	params = append(params, p.parseParam())
	for p.match(lexer.COMMA) {
		params = append(params, p.parseParam())
	}
	return params
}

func (p *Parser) parseParam() *ast.Param {
	name := p.expect(lexer.IDENT, "expected parameter name")
	p.expect(lexer.COLON, "expected ':' after parameter name")
	typ := p.parseType()

	// Optional default value: param: type = expr
	var defaultVal ast.Expr
	if p.match(lexer.ASSIGN) {
		defaultVal = p.parseExpression()
	}

	return &ast.Param{
		Name:    name.Value,
		Type:    typ,
		Default: defaultVal,
		Pos:     p.position(name),
	}
}

// parseType parses a type annotation. Type keywords (str, i32, void, …)
// and plain identifiers are both accepted.
func (p *Parser) parseType() *ast.TypeExpr {
	tok := p.peek()

	// Array type: []<element_type>
	if tok.Type == lexer.LBRACKET {
		p.advance() // consume '['
		p.expect(lexer.RBRACKET, "expected ']' in array type")
		elemType := p.parseType()
		return &ast.TypeExpr{
			Name:     "[]" + elemType.Name,
			IsArray:  true,
			ElemName: elemType.Name,
			Pos:      p.position(tok),
		}
	}

	if isTypeKeyword(tok.Type) {
		p.advance()
		return &ast.TypeExpr{Name: tok.Value, Pos: p.position(tok)}
	}
	if tok.Type == lexer.IDENT {
		p.advance()
		return &ast.TypeExpr{Name: tok.Value, Pos: p.position(tok)}
	}

	p.addError(tok, fmt.Sprintf("expected type name, got %s", tok.Type))
	return &ast.TypeExpr{Name: "<error>", Pos: p.position(tok)}
}

// =========================================================================
// Block and statement parsing
// =========================================================================

func (p *Parser) parseBlock() *ast.BlockStmt {
	tok := p.expect(lexer.LBRACE, "expected '{'")
	block := &ast.BlockStmt{Pos: p.position(tok)}

	for !p.check(lexer.RBRACE) && !p.check(lexer.EOF) {
		startPos := p.pos
		stmt := p.parseStatement()
		if stmt != nil {
			block.Stmts = append(block.Stmts, stmt)
		}
		// Safety: if no tokens were consumed, skip one to avoid an infinite loop.
		if p.pos == startPos {
			p.advance()
		}
	}

	p.expect(lexer.RBRACE, "expected '}'")
	return block
}

func (p *Parser) parseStatement() ast.Stmt {
	switch p.peek().Type {
	case lexer.LET:
		return p.parseLetStmt()
	case lexer.RETURN:
		return p.parseReturnStmt()
	case lexer.BREAK:
		return p.parseBreakStmt()
	case lexer.CONTINUE:
		return p.parseContinueStmt()
	case lexer.IF:
		return p.parseIfStmt()
	case lexer.WHILE:
		return p.parseWhileStmt()
	case lexer.FOR:
		return p.parseForStmt()
	default:
		return p.parseExprOrAssignStmt()
	}
}

// ---- Let ----

func (p *Parser) parseLetStmt() *ast.LetStmt {
	tok := p.advance() // consume LET
	name := p.expect(lexer.IDENT, "expected variable name")
	p.expect(lexer.COLON, "expected ':' after variable name")
	typ := p.parseType()
	p.expect(lexer.ASSIGN, "expected '=' in let declaration")
	value := p.parseExpression()
	p.expect(lexer.SEMICOLON, "expected ';' after let statement")
	return &ast.LetStmt{
		Name:  name.Value,
		Type:  typ,
		Value: value,
		Pos:   p.position(tok),
	}
}

// ---- Return ----

func (p *Parser) parseReturnStmt() *ast.ReturnStmt {
	tok := p.advance() // consume RETURN
	var value ast.Expr
	if !p.check(lexer.SEMICOLON) {
		value = p.parseExpression()
	}
	p.expect(lexer.SEMICOLON, "expected ';' after return statement")
	return &ast.ReturnStmt{Value: value, Pos: p.position(tok)}
}

// ---- Break / Continue ----

func (p *Parser) parseBreakStmt() *ast.BreakStmt {
	tok := p.advance()
	p.expect(lexer.SEMICOLON, "expected ';' after break")
	return &ast.BreakStmt{Pos: p.position(tok)}
}

func (p *Parser) parseContinueStmt() *ast.ContinueStmt {
	tok := p.advance()
	p.expect(lexer.SEMICOLON, "expected ';' after continue")
	return &ast.ContinueStmt{Pos: p.position(tok)}
}

// ---- If ----

func (p *Parser) parseIfStmt() *ast.IfStmt {
	tok := p.advance() // consume IF
	p.expect(lexer.LPAREN, "expected '(' after 'if'")
	cond := p.parseExpression()
	p.expect(lexer.RPAREN, "expected ')' after if condition")
	body := p.parseBlock()

	var elseStmt ast.Stmt
	if p.match(lexer.ELSE) {
		if p.check(lexer.IF) {
			elseStmt = p.parseIfStmt()
		} else {
			elseStmt = p.parseBlock()
		}
	}

	return &ast.IfStmt{
		Condition: cond,
		Then:      body,
		Else:      elseStmt,
		Pos:       p.position(tok),
	}
}

// ---- While ----

func (p *Parser) parseWhileStmt() *ast.WhileStmt {
	tok := p.advance() // consume WHILE
	p.expect(lexer.LPAREN, "expected '(' after 'while'")
	cond := p.parseExpression()
	p.expect(lexer.RPAREN, "expected ')' after while condition")
	body := p.parseBlock()
	return &ast.WhileStmt{Condition: cond, Body: body, Pos: p.position(tok)}
}

// ---- For ----

func (p *Parser) parseForStmt() *ast.ForStmt {
	tok := p.advance() // consume FOR
	p.expect(lexer.LPAREN, "expected '(' after 'for'")

	// Init clause (including its trailing semicolon).
	var init ast.Stmt
	if p.check(lexer.LET) {
		init = p.parseLetStmt() // consumes trailing ;
	} else {
		init = p.parseExprOrAssignStmt() // consumes trailing ;
	}

	// Condition expression, then semicolon.
	cond := p.parseExpression()
	p.expect(lexer.SEMICOLON, "expected ';' after for condition")

	// Update clause – NO trailing semicolon before ')'.
	update := p.parseExprOrAssignStmtNoSemicolon()

	p.expect(lexer.RPAREN, "expected ')' after for clauses")
	body := p.parseBlock()

	return &ast.ForStmt{
		Init:      init,
		Condition: cond,
		Update:    update,
		Body:      body,
		Pos:       p.position(tok),
	}
}

// ---- Expression-or-Assignment statement ----

// parseExprOrAssignStmt parses either an expression statement or an
// assignment statement, consuming the trailing semicolon.
func (p *Parser) parseExprOrAssignStmt() ast.Stmt {
	expr := p.parseExpression()

	if p.check(lexer.ASSIGN) {
		p.advance() // consume =
		value := p.parseExpression()
		p.expect(lexer.SEMICOLON, "expected ';' after assignment")
		return &ast.AssignStmt{
			Target: expr,
			Value:  value,
			Pos:    expr.GetPos(),
		}
	}

	p.expect(lexer.SEMICOLON, "expected ';' after expression statement")
	return &ast.ExprStmt{Expression: expr, Pos: expr.GetPos()}
}

// parseExprOrAssignStmtNoSemicolon is the same but does NOT expect a
// trailing semicolon. Used for the update clause of a for-loop.
func (p *Parser) parseExprOrAssignStmtNoSemicolon() ast.Stmt {
	expr := p.parseExpression()

	if p.check(lexer.ASSIGN) {
		p.advance()
		value := p.parseExpression()
		return &ast.AssignStmt{
			Target: expr,
			Value:  value,
			Pos:    expr.GetPos(),
		}
	}

	return &ast.ExprStmt{Expression: expr, Pos: expr.GetPos()}
}

// =========================================================================
// Pratt expression parser
// =========================================================================

// parseExpression is the public entry point for expression parsing.
func (p *Parser) parseExpression() ast.Expr {
	return p.parsePrecedence(precOr)
}

// parsePrecedence parses an expression with at least the given minimum
// precedence. This is the core of the Pratt algorithm.
func (p *Parser) parsePrecedence(minPrec int) ast.Expr {
	left := p.parsePrefix()

	for {
		prec := infixPrecedence(p.peek().Type)
		if prec < minPrec {
			break
		}
		left = p.parseInfix(left, prec)
	}

	return left
}

// ---- Prefix (atoms & unary operators) ----

func (p *Parser) parsePrefix() ast.Expr {
	tok := p.peek()

	switch tok.Type {
	case lexer.IDENT:
		p.advance()
		return &ast.IdentExpr{Name: tok.Value, Pos: p.position(tok)}

	case lexer.INT:
		p.advance()
		return &ast.IntLitExpr{Value: tok.Value, Pos: p.position(tok)}

	case lexer.FLOAT:
		p.advance()
		return &ast.FloatLitExpr{Value: tok.Value, Pos: p.position(tok)}

	case lexer.STRING:
		p.advance()
		return &ast.StringLitExpr{Value: tok.Value, Pos: p.position(tok)}

	case lexer.TRUE:
		p.advance()
		return &ast.BoolLitExpr{Value: true, Pos: p.position(tok)}

	case lexer.FALSE:
		p.advance()
		return &ast.BoolLitExpr{Value: false, Pos: p.position(tok)}

	case lexer.LPAREN:
		return p.parseGroupExpr()

	case lexer.BANG, lexer.MINUS:
		p.advance()
		operand := p.parsePrecedence(precUnary)
		return &ast.UnaryExpr{Op: tok.Value, Operand: operand, Pos: p.position(tok)}

	case lexer.TILDE:
		p.advance()
		operand := p.parsePrecedence(precUnary)
		return &ast.UnaryExpr{Op: tok.Value, Operand: operand, Pos: p.position(tok)}

	case lexer.AMPERSAND:
		p.advance()
		operand := p.parsePrecedence(precUnary)
		return &ast.AddressOfExpr{Operand: operand, Pos: p.position(tok)}

	case lexer.LBRACKET:
		return p.parseArrayLitExpr()

	default:
		// Type keywords (str, i32, …) used as identifiers in expression
		// context. This lets code like  str = str + '\r\n';  work even
		// though the lexer tokenises "str" as STR.
		if isTypeKeyword(tok.Type) {
			p.advance()
			return &ast.IdentExpr{Name: tok.Value, Pos: p.position(tok)}
		}

		p.addError(tok, fmt.Sprintf("unexpected token %s in expression", tok.Type))
		p.advance() // consume the bad token so we make progress
		return &ast.IdentExpr{Name: "<error>", Pos: p.position(tok)}
	}
}

// parseGroupExpr parses a parenthesised expression: ( <expr> )
func (p *Parser) parseGroupExpr() ast.Expr {
	tok := p.advance() // consume (
	expr := p.parseExpression()
	p.expect(lexer.RPAREN, "expected ')' after expression")
	return &ast.GroupExpr{Expression: expr, Pos: p.position(tok)}
}

// parseArrayLitExpr parses [expr, expr, ...] or [] (empty array).
func (p *Parser) parseArrayLitExpr() ast.Expr {
	tok := p.advance() // consume '['
	var elems []ast.Expr
	if p.peek().Type != lexer.RBRACKET {
		elems = append(elems, p.parseExpression())
		for p.peek().Type == lexer.COMMA {
			p.advance() // consume ','
			if p.peek().Type == lexer.RBRACKET {
				break // allow trailing comma
			}
			elems = append(elems, p.parseExpression())
		}
	}
	p.expect(lexer.RBRACKET, "expected ']' after array elements")
	return &ast.ArrayLitExpr{Elems: elems, Pos: p.position(tok)}
}

// ---- Infix precedence table ----

func infixPrecedence(typ string) int {
	switch typ {
	case lexer.OR:
		return precOr
	case lexer.AND:
		return precAnd
	case lexer.PIPE:
		return precBitOr
	case lexer.CARET:
		return precBitXor
	case lexer.AMPERSAND:
		return precBitAnd
	case lexer.EQ, lexer.NEQ:
		return precEquality
	case lexer.LT, lexer.GT, lexer.LTE, lexer.GTE:
		return precComparison
	case lexer.SHL, lexer.SHR:
		return precShift
	case lexer.PLUS, lexer.MINUS:
		return precAdditive
	case lexer.STAR, lexer.SLASH, lexer.PERCENT:
		return precMultiply
	case lexer.LPAREN, lexer.DOT, lexer.LBRACKET:
		return precCall
	default:
		return precNone
	}
}

// ---- Infix / postfix dispatch ----

func (p *Parser) parseInfix(left ast.Expr, prec int) ast.Expr {
	tok := p.peek()

	switch tok.Type {
	case lexer.LPAREN:
		return p.parseCallExpr(left)
	case lexer.DOT:
		return p.parseMemberExpr(left)
	case lexer.LBRACKET:
		return p.parseIndexExpr(left)
	default:
		// Binary operator (left-associative: recurse with prec+1).
		p.advance()
		right := p.parsePrecedence(prec + 1)
		return &ast.BinaryExpr{
			Op:    tok.Value,
			Left:  left,
			Right: right,
			Pos:   p.position(tok),
		}
	}
}

// parseIndexExpr: <object> [ <index> ]
func (p *Parser) parseIndexExpr(object ast.Expr) ast.Expr {
	tok := p.advance() // consume [
	index := p.parseExpression()
	p.expect(lexer.RBRACKET, "expected ']' after index expression")
	return &ast.IndexExpr{Object: object, Index: index, Pos: p.position(tok)}
}

// parseCallExpr: <callee> ( [args] )
func (p *Parser) parseCallExpr(callee ast.Expr) ast.Expr {
	tok := p.advance() // consume (
	var args []ast.Expr

	if !p.check(lexer.RPAREN) {
		args = append(args, p.parseExpression())
		for p.match(lexer.COMMA) {
			args = append(args, p.parseExpression())
		}
	}

	p.expect(lexer.RPAREN, "expected ')' after arguments")
	return &ast.CallExpr{Callee: callee, Args: args, Pos: p.position(tok)}
}

// parseMemberExpr: <object> . <field>
func (p *Parser) parseMemberExpr(object ast.Expr) ast.Expr {
	dotTok := p.advance() // consume .
	tok := p.peek()

	// Allow type keywords as field names (e.g. obj.str) by treating them
	// as identifiers here.
	if tok.Type == lexer.IDENT || isTypeKeyword(tok.Type) {
		p.advance()
		return &ast.MemberExpr{
			Object: object,
			Field:  tok.Value,
			Pos:    p.position(dotTok),
		}
	}

	p.addError(tok, "expected field name after '.'")
	return &ast.MemberExpr{
		Object: object,
		Field:  "<error>",
		Pos:    p.position(dotTok),
	}
}

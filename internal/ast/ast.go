package ast

import (
	"fmt"
	"strings"
)

// ---------------------------------------------------------------------------
// Source position
// ---------------------------------------------------------------------------

// Position represents a line/column pair in source code (1-based).
type Position struct {
	Line   int
	Column int
}

func (p Position) String() string {
	return fmt.Sprintf("%d:%d", p.Line, p.Column)
}

// ---------------------------------------------------------------------------
// Interfaces
// ---------------------------------------------------------------------------

// Node is implemented by every AST node.
type Node interface {
	GetPos() Position
}

// Stmt is implemented by every statement node.
type Stmt interface {
	Node
	stmtNode()
}

// Expr is implemented by every expression node.
type Expr interface {
	Node
	exprNode()
}

// ---------------------------------------------------------------------------
// Program (root)
// ---------------------------------------------------------------------------

type Program struct {
	Module    *ModuleDecl
	Imports   []*ImportDecl
	Functions []*FnDecl
	Pos       Position
}

func (n *Program) GetPos() Position { return n.Pos }

// ---------------------------------------------------------------------------
// Top-level declarations
// ---------------------------------------------------------------------------

type ModuleDecl struct {
	Name string
	Pos  Position
}

func (n *ModuleDecl) GetPos() Position { return n.Pos }

type ImportDecl struct {
	Name string
	Pos  Position
}

func (n *ImportDecl) GetPos() Position { return n.Pos }

// Param represents a single function parameter (name: type).
type Param struct {
	Name string
	Type *TypeExpr
	Pos  Position
}

// TypeExpr represents a type annotation such as "i32", "str", "void".
type TypeExpr struct {
	Name string
	Pos  Position
}

type FnDecl struct {
	Name       string
	Params     []*Param
	ReturnType *TypeExpr
	Body       *BlockStmt
	Pos        Position
}

func (n *FnDecl) GetPos() Position { return n.Pos }

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

// BlockStmt is a brace-delimited list of statements.
type BlockStmt struct {
	Stmts []Stmt
	Pos   Position
}

func (n *BlockStmt) GetPos() Position { return n.Pos }
func (n *BlockStmt) stmtNode()        {}

// LetStmt: let <name>: <type> = <value>;
type LetStmt struct {
	Name  string
	Type  *TypeExpr
	Value Expr
	Pos   Position
}

func (n *LetStmt) GetPos() Position { return n.Pos }
func (n *LetStmt) stmtNode()        {}

// ReturnStmt: return [<value>];
type ReturnStmt struct {
	Value Expr // nil for bare "return;"
	Pos   Position
}

func (n *ReturnStmt) GetPos() Position { return n.Pos }
func (n *ReturnStmt) stmtNode()        {}

// BreakStmt: break;
type BreakStmt struct {
	Pos Position
}

func (n *BreakStmt) GetPos() Position { return n.Pos }
func (n *BreakStmt) stmtNode()        {}

// ContinueStmt: continue;
type ContinueStmt struct {
	Pos Position
}

func (n *ContinueStmt) GetPos() Position { return n.Pos }
func (n *ContinueStmt) stmtNode()        {}

// IfStmt: if (<cond>) <then> [else <else>]
type IfStmt struct {
	Condition Expr
	Then      *BlockStmt
	Else      Stmt // nil, *BlockStmt, or *IfStmt (else-if chain)
	Pos       Position
}

func (n *IfStmt) GetPos() Position { return n.Pos }
func (n *IfStmt) stmtNode()        {}

// WhileStmt: while (<cond>) <body>
type WhileStmt struct {
	Condition Expr
	Body      *BlockStmt
	Pos       Position
}

func (n *WhileStmt) GetPos() Position { return n.Pos }
func (n *WhileStmt) stmtNode()        {}

// ForStmt: for (<init>; <cond>; <update>) <body>
type ForStmt struct {
	Init      Stmt // *LetStmt, *AssignStmt, or *ExprStmt
	Condition Expr
	Update    Stmt // *AssignStmt or *ExprStmt (no trailing semicolon)
	Body      *BlockStmt
	Pos       Position
}

func (n *ForStmt) GetPos() Position { return n.Pos }
func (n *ForStmt) stmtNode()        {}

// ExprStmt wraps a bare expression used as a statement.
type ExprStmt struct {
	Expression Expr
	Pos        Position
}

func (n *ExprStmt) GetPos() Position { return n.Pos }
func (n *ExprStmt) stmtNode()        {}

// AssignStmt: <target> = <value>;
type AssignStmt struct {
	Target Expr // *IdentExpr or *MemberExpr
	Value  Expr
	Pos    Position
}

func (n *AssignStmt) GetPos() Position { return n.Pos }
func (n *AssignStmt) stmtNode()        {}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

// IdentExpr is a plain identifier reference.
type IdentExpr struct {
	Name string
	Pos  Position
}

func (n *IdentExpr) GetPos() Position { return n.Pos }
func (n *IdentExpr) exprNode()        {}

// IntLitExpr is an integer literal (value kept as the original lexeme).
type IntLitExpr struct {
	Value string
	Pos   Position
}

func (n *IntLitExpr) GetPos() Position { return n.Pos }
func (n *IntLitExpr) exprNode()        {}

// FloatLitExpr is a float literal.
type FloatLitExpr struct {
	Value string
	Pos   Position
}

func (n *FloatLitExpr) GetPos() Position { return n.Pos }
func (n *FloatLitExpr) exprNode()        {}

// StringLitExpr is a string literal (value includes surrounding quotes).
type StringLitExpr struct {
	Value string
	Pos   Position
}

func (n *StringLitExpr) GetPos() Position { return n.Pos }
func (n *StringLitExpr) exprNode()        {}

// BoolLitExpr is true or false.
type BoolLitExpr struct {
	Value bool
	Pos   Position
}

func (n *BoolLitExpr) GetPos() Position { return n.Pos }
func (n *BoolLitExpr) exprNode()        {}

// UnaryExpr: <op><operand>  (e.g. !x, -y)
type UnaryExpr struct {
	Op      string
	Operand Expr
	Pos     Position
}

func (n *UnaryExpr) GetPos() Position { return n.Pos }
func (n *UnaryExpr) exprNode()        {}

// BinaryExpr: <left> <op> <right>
type BinaryExpr struct {
	Op    string
	Left  Expr
	Right Expr
	Pos   Position
}

func (n *BinaryExpr) GetPos() Position { return n.Pos }
func (n *BinaryExpr) exprNode()        {}

// CallExpr: <callee>(<args>)
type CallExpr struct {
	Callee Expr
	Args   []Expr
	Pos    Position
}

func (n *CallExpr) GetPos() Position { return n.Pos }
func (n *CallExpr) exprNode()        {}

// MemberExpr: <object>.<field>
type MemberExpr struct {
	Object Expr
	Field  string
	Pos    Position
}

func (n *MemberExpr) GetPos() Position { return n.Pos }
func (n *MemberExpr) exprNode()        {}

// IndexExpr: <object>[<index>]
type IndexExpr struct {
	Object Expr
	Index  Expr
	Pos    Position
}

func (n *IndexExpr) GetPos() Position { return n.Pos }
func (n *IndexExpr) exprNode()        {}

// GroupExpr: (<expression>)
type GroupExpr struct {
	Expression Expr
	Pos        Position
}

func (n *GroupExpr) GetPos() Position { return n.Pos }
func (n *GroupExpr) exprNode()        {}

// AddressOfExpr: &<operand>
type AddressOfExpr struct {
	Operand Expr
	Pos     Position
}

func (n *AddressOfExpr) GetPos() Position { return n.Pos }
func (n *AddressOfExpr) exprNode()        {}

// ---------------------------------------------------------------------------
// Debug printer – produces a human-readable tree representation
// ---------------------------------------------------------------------------

// DebugString returns a readable multi-line representation of the AST.
func DebugString(prog *Program) string {
	var b strings.Builder
	debugProgram(&b, prog, 0)
	return b.String()
}

func writeIndent(b *strings.Builder, level int) {
	for i := 0; i < level; i++ {
		b.WriteString("  ")
	}
}

func debugProgram(b *strings.Builder, prog *Program, level int) {
	writeIndent(b, level)
	b.WriteString("Program\n")

	if prog.Module != nil {
		writeIndent(b, level+1)
		fmt.Fprintf(b, "Module: %s\n", prog.Module.Name)
	}
	for _, imp := range prog.Imports {
		writeIndent(b, level+1)
		fmt.Fprintf(b, "Import: %s\n", imp.Name)
	}
	for _, fn := range prog.Functions {
		debugFnDecl(b, fn, level+1)
	}
}

func debugFnDecl(b *strings.Builder, fn *FnDecl, level int) {
	writeIndent(b, level)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name + ": " + p.Type.Name
	}
	retName := "void"
	if fn.ReturnType != nil {
		retName = fn.ReturnType.Name
	}
	fmt.Fprintf(b, "Fn %s(%s) -> %s\n", fn.Name, strings.Join(params, ", "), retName)
	debugBlock(b, fn.Body, level+1)
}

func debugBlock(b *strings.Builder, block *BlockStmt, level int) {
	writeIndent(b, level)
	fmt.Fprintf(b, "Block [%d statements]\n", len(block.Stmts))
	for _, s := range block.Stmts {
		debugStmt(b, s, level+1)
	}
}

func debugStmt(b *strings.Builder, s Stmt, level int) {
	switch s := s.(type) {
	case *LetStmt:
		writeIndent(b, level)
		fmt.Fprintf(b, "LetStmt %s: %s = %s\n", s.Name, s.Type.Name, ExprString(s.Value))
	case *ReturnStmt:
		writeIndent(b, level)
		if s.Value != nil {
			fmt.Fprintf(b, "ReturnStmt %s\n", ExprString(s.Value))
		} else {
			b.WriteString("ReturnStmt\n")
		}
	case *BreakStmt:
		writeIndent(b, level)
		b.WriteString("BreakStmt\n")
	case *ContinueStmt:
		writeIndent(b, level)
		b.WriteString("ContinueStmt\n")
	case *IfStmt:
		writeIndent(b, level)
		fmt.Fprintf(b, "IfStmt (%s)\n", ExprString(s.Condition))
		debugBlock(b, s.Then, level+1)
		if s.Else != nil {
			writeIndent(b, level+1)
			b.WriteString("Else:\n")
			switch e := s.Else.(type) {
			case *BlockStmt:
				debugBlock(b, e, level+2)
			case *IfStmt:
				debugStmt(b, e, level+2)
			}
		}
	case *WhileStmt:
		writeIndent(b, level)
		fmt.Fprintf(b, "WhileStmt (%s)\n", ExprString(s.Condition))
		debugBlock(b, s.Body, level+1)
	case *ForStmt:
		writeIndent(b, level)
		b.WriteString("ForStmt\n")
		writeIndent(b, level+1)
		fmt.Fprintf(b, "Init: ")
		debugStmtInline(b, s.Init)
		writeIndent(b, level+1)
		fmt.Fprintf(b, "Cond: %s\n", ExprString(s.Condition))
		writeIndent(b, level+1)
		fmt.Fprintf(b, "Update: ")
		debugStmtInline(b, s.Update)
		debugBlock(b, s.Body, level+1)
	case *AssignStmt:
		writeIndent(b, level)
		fmt.Fprintf(b, "AssignStmt %s = %s\n", ExprString(s.Target), ExprString(s.Value))
	case *ExprStmt:
		writeIndent(b, level)
		fmt.Fprintf(b, "ExprStmt %s\n", ExprString(s.Expression))
	case *BlockStmt:
		debugBlock(b, s, level)
	default:
		writeIndent(b, level)
		b.WriteString("<unknown stmt>\n")
	}
}

// debugStmtInline writes a one-line summary (used inside for-loop headers).
func debugStmtInline(b *strings.Builder, s Stmt) {
	switch s := s.(type) {
	case *LetStmt:
		fmt.Fprintf(b, "let %s: %s = %s\n", s.Name, s.Type.Name, ExprString(s.Value))
	case *AssignStmt:
		fmt.Fprintf(b, "%s = %s\n", ExprString(s.Target), ExprString(s.Value))
	case *ExprStmt:
		fmt.Fprintf(b, "%s\n", ExprString(s.Expression))
	default:
		b.WriteString("<stmt>\n")
	}
}

// ExprString returns a concise one-line representation of an expression.
func ExprString(e Expr) string {
	if e == nil {
		return "<nil>"
	}
	switch e := e.(type) {
	case *IdentExpr:
		return e.Name
	case *IntLitExpr:
		return e.Value
	case *FloatLitExpr:
		return e.Value
	case *StringLitExpr:
		return e.Value
	case *BoolLitExpr:
		if e.Value {
			return "true"
		}
		return "false"
	case *UnaryExpr:
		return fmt.Sprintf("(%s%s)", e.Op, ExprString(e.Operand))
	case *BinaryExpr:
		return fmt.Sprintf("(%s %s %s)", ExprString(e.Left), e.Op, ExprString(e.Right))
	case *CallExpr:
		args := make([]string, len(e.Args))
		for i, a := range e.Args {
			args[i] = ExprString(a)
		}
		return fmt.Sprintf("%s(%s)", ExprString(e.Callee), strings.Join(args, ", "))
	case *MemberExpr:
		return fmt.Sprintf("%s.%s", ExprString(e.Object), e.Field)
	case *IndexExpr:
		return fmt.Sprintf("%s[%s]", ExprString(e.Object), ExprString(e.Index))
	case *GroupExpr:
		return fmt.Sprintf("(%s)", ExprString(e.Expression))
	case *AddressOfExpr:
		return fmt.Sprintf("(&%s)", ExprString(e.Operand))
	default:
		return "<unknown expr>"
	}
}

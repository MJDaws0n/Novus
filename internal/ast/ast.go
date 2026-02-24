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
	Module         *ModuleDecl
	Imports        []*ImportDecl
	Globals        []*GlobalVar
	Functions      []*FnDecl
	CompTimeBlocks []*CompTimeIf
	Pos            Position
}

func (n *Program) GetPos() Position { return n.Pos }

// ---------------------------------------------------------------------------
// Top-level declarations
// ---------------------------------------------------------------------------

// GlobalVar represents a top-level variable: let <name>: <type> = <value>;
type GlobalVar struct {
	Name  string
	Type  *TypeExpr
	Value Expr
	Pos   Position
}

func (n *GlobalVar) GetPos() Position { return n.Pos }

type ModuleDecl struct {
	Name string
	Pos  Position
}

func (n *ModuleDecl) GetPos() Position { return n.Pos }

type ImportDecl struct {
	Path      string   // e.g. "standard_lib" or "test/cool/standard_lib" (no .nov extension)
	Alias     string   // optional alias: import standard_lib std → Alias="std", empty string = direct access
	SelectFns []string // optional selective imports: import foo[bar, baz] → SelectFns=["bar","baz"]
	Pos       Position
}

func (n *ImportDecl) GetPos() Position { return n.Pos }

// CompTimeIf represents a compile-time conditional block:
//   #if(os == "windows") { ... }
// The condition is a simple variable == "value" or variable != "value" check.
// Available variables: os ("windows", "linux", "darwin"), arch ("amd64", "arm64", "x86").
// When the condition is true, the body (imports, functions, globals) is merged
// into the program; otherwise the block is discarded before semantic analysis.
type CompTimeIf struct {
	Variable  string       // "os" or "arch"
	Operator  string       // "==" or "!="
	Value     string       // e.g. "windows", "amd64"
	Imports   []*ImportDecl
	Functions []*FnDecl
	Globals   []*GlobalVar
	Pos       Position
}

func (n *CompTimeIf) GetPos() Position { return n.Pos }

// Param represents a single function parameter (name: type) or (name: type = default).
type Param struct {
	Name    string
	Type    *TypeExpr
	Default Expr // optional default value expression (nil if no default)
	Pos     Position
}

// TypeExpr represents a type annotation such as "i32", "str", "void", "[]i32".
type TypeExpr struct {
	Name     string // e.g. "i32", "[]i32"
	IsArray  bool   // true for array types like []i32
	ElemName string // element type name for arrays (e.g. "i32" for []i32)
	Pos      Position
}

type FnDecl struct {
	Name        string
	MangledName string // set by semantic analysis; used as the assembly/IR label
	Params      []*Param
	ReturnType  *TypeExpr
	Body        *BlockStmt
	Pos         Position
	Imported    bool // true if this function came from an imported module
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
	Callee         Expr
	Args           []Expr
	ResolvedCallee string // set by semantic analysis; the mangled function name to call
	Pos            Position
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

// ArrayLitExpr: [expr, expr, ...] or [] (empty array literal)
type ArrayLitExpr struct {
	Elems []Expr
	Pos   Position
}

func (n *ArrayLitExpr) GetPos() Position { return n.Pos }
func (n *ArrayLitExpr) exprNode()        {}

// ---------------------------------------------------------------------------
// Compile-time conditional resolution
// ---------------------------------------------------------------------------

// ResolveCompTimeBlocks evaluates all #if blocks in the program and merges
// the contents of matching blocks into the program's imports, functions, and
// globals.  Non-matching blocks are discarded.
//
// Constants is a map of compile-time variable values, e.g.:
//
//	{"os": "windows", "arch": "amd64"}
func ResolveCompTimeBlocks(prog *Program, constants map[string]string) {
	for _, ct := range prog.CompTimeBlocks {
		actual, ok := constants[ct.Variable]
		if !ok {
			continue // unknown variable — skip block
		}
		match := false
		switch ct.Operator {
		case "==":
			match = actual == ct.Value
		case "!=":
			match = actual != ct.Value
		}
		if !match {
			continue
		}
		// Merge matching block contents into the program.
		prog.Imports = append(prog.Imports, ct.Imports...)
		prog.Functions = append(prog.Functions, ct.Functions...)
		prog.Globals = append(prog.Globals, ct.Globals...)
	}
	prog.CompTimeBlocks = nil // resolved; no longer needed
}

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
		info := imp.Path
		if len(imp.SelectFns) > 0 {
			info += "[" + strings.Join(imp.SelectFns, ", ") + "]"
		}
		if imp.Alias != "" {
			info += " as " + imp.Alias
		}
		fmt.Fprintf(b, "Import: %s\n", info)
	}
	for _, g := range prog.Globals {
		writeIndent(b, level+1)
		typeName := "unknown"
		if g.Type != nil {
			typeName = g.Type.Name
		}
		fmt.Fprintf(b, "Global: let %s: %s = %s\n", g.Name, typeName, ExprString(g.Value))
	}
	for _, ct := range prog.CompTimeBlocks {
		writeIndent(b, level+1)
		fmt.Fprintf(b, "#if(%s %s %q)\n", ct.Variable, ct.Operator, ct.Value)
	}
	for _, fn := range prog.Functions {
		debugFnDecl(b, fn, level+1)
	}
}

func debugFnDecl(b *strings.Builder, fn *FnDecl, level int) {
	writeIndent(b, level)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		s := p.Name + ": " + p.Type.Name
		if p.Default != nil {
			s += " = " + ExprString(p.Default)
		}
		params[i] = s
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
	case *ArrayLitExpr:
		elems := make([]string, len(e.Elems))
		for i, el := range e.Elems {
			elems[i] = ExprString(el)
		}
		return fmt.Sprintf("[%s]", strings.Join(elems, ", "))
	default:
		return "<unknown expr>"
	}
}

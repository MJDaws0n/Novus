package semantic

import (
	"fmt"
	"novus/internal/ast"
	"strings"
)

// ---------------------------------------------------------------------------
// Diagnostic severity
// ---------------------------------------------------------------------------

// Severity indicates whether a diagnostic is an error or a warning.
type Severity int

const (
	Error Severity = iota
	Warning
)

func (s Severity) String() string {
	switch s {
	case Error:
		return "error"
	case Warning:
		return "warning"
	default:
		return "unknown"
	}
}

// ---------------------------------------------------------------------------
// Diagnostic
// ---------------------------------------------------------------------------

// Diagnostic represents a single message produced by the semantic analyser.
type Diagnostic struct {
	Message  string
	Pos      ast.Position
	Severity Severity
}

func (d Diagnostic) Error() string {
	return fmt.Sprintf("line %d, col %d: %s: %s", d.Pos.Line, d.Pos.Column, d.Severity, d.Message)
}

// HasErrors returns true if any diagnostic in the slice is an error.
func HasErrors(diags []Diagnostic) bool {
	for _, d := range diags {
		if d.Severity == Error {
			return true
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// Type system
// ---------------------------------------------------------------------------

// Type represents a semantic type in the Novus language.
type Type struct {
	Name     string
	IsArray  bool  // true for array types (e.g. []i32)
	ElemType *Type // element type for arrays (e.g. TypeI32 for []i32)
}

// Built-in type singletons — concrete types.
var (
	TypeVoid = &Type{Name: "void"}
	TypeBool = &Type{Name: "bool"}
	TypeStr  = &Type{Name: "str"}
	TypeU8   = &Type{Name: "u8"}
	TypeU16  = &Type{Name: "u16"}
	TypeU32  = &Type{Name: "u32"}
	TypeU64  = &Type{Name: "u64"}
	TypeI8   = &Type{Name: "i8"}
	TypeI16  = &Type{Name: "i16"}
	TypeI32  = &Type{Name: "i32"}
	TypeI64  = &Type{Name: "i64"}
	TypeF32  = &Type{Name: "f32"}
	TypeF64  = &Type{Name: "f64"}
)

// Special type singletons — untyped literals and register references.
var (
	TypeUntypedInt   = &Type{Name: "untyped int"}   // integer literals (adapts to any integer or float)
	TypeUntypedFloat = &Type{Name: "untyped float"} // float literals (adapts to any float)
	TypeReg          = &Type{Name: "register"}      // CPU register reference
)

// builtinTypes maps type-name strings to their Type singletons.
var builtinTypes = map[string]*Type{
	"void": TypeVoid,
	"bool": TypeBool,
	"str":  TypeStr,
	"u8":   TypeU8,
	"u16":  TypeU16,
	"u32":  TypeU32,
	"u64":  TypeU64,
	"i8":   TypeI8,
	"i16":  TypeI16,
	"i32":  TypeI32,
	"i64":  TypeI64,
	"f32":  TypeF32,
	"f64":  TypeF64,
}

// LookupType resolves a type name string to a *Type, or nil if unknown.
// Supports array types like "[]i32" by dynamically creating array type wrappers.
func LookupType(name string) *Type {
	if t, ok := builtinTypes[name]; ok {
		return t
	}
	// Array type: []<elem>
	if len(name) > 2 && name[:2] == "[]" {
		elemName := name[2:]
		elemType := LookupType(elemName)
		if elemType == nil {
			return nil
		}
		// Cache the array type so future lookups return the same pointer.
		arrType := &Type{Name: name, IsArray: true, ElemType: elemType}
		builtinTypes[name] = arrType
		return arrType
	}
	return nil
}

// RegisterType adds a new type to the registry (for future extensibility).
func RegisterType(name string, t *Type) {
	builtinTypes[name] = t
}

// ---------------------------------------------------------------------------
// Type predicates
// ---------------------------------------------------------------------------

// isInteger reports whether t is one of the concrete integer types.
func isInteger(t *Type) bool {
	switch t {
	case TypeU8, TypeU16, TypeU32, TypeU64,
		TypeI8, TypeI16, TypeI32, TypeI64:
		return true
	}
	return false
}

// isFloat reports whether t is one of the concrete floating-point types.
func isFloat(t *Type) bool {
	return t == TypeF32 || t == TypeF64
}

// isNumeric reports whether t is a numeric type (concrete integer, concrete
// float, or an untyped numeric literal).
func isNumeric(t *Type) bool {
	return isInteger(t) || isFloat(t) || t == TypeUntypedInt || t == TypeUntypedFloat
}

// isAssignableTo reports whether a value of type src can be used where dst
// is expected.  This enables untyped integer literals to flow into any
// integer or float slot, and untyped float literals to flow into any float
// slot — making the language easy to use without explicit casts.
func isAssignableTo(dst, src *Type) bool {
	if dst == src {
		return true
	}
	if src == TypeUntypedInt {
		// Untyped int adapts to any integer, any float, or untyped float.
		return isInteger(dst) || isFloat(dst) || dst == TypeUntypedFloat
	}
	if src == TypeUntypedFloat {
		// Untyped float adapts to any concrete float.
		return isFloat(dst)
	}
	// Array types: compatible if both are arrays and element types match
	// (or src is an empty-array placeholder).
	if dst.IsArray && src.IsArray {
		if src.Name == "[]<unknown>" {
			return true // empty array literal adapts to any array type
		}
		// Allow element-level coercion (e.g. untyped int -> i32).
		return isAssignableTo(dst.ElemType, src.ElemType)
	}
	return false
}

// isComparable reports whether values of type a and b can be compared with
// == or !=.
func isComparable(a, b *Type) bool {
	if a == b {
		return true
	}
	return isAssignableTo(a, b) || isAssignableTo(b, a)
}

// resolveNumericPair determines the concrete result type when two numeric
// values are combined in a binary operation.  Returns nil on mismatch.
func resolveNumericPair(a, b *Type) *Type {
	if a == b {
		return a
	}
	// Untyped int adapts to the partner's type.
	if a == TypeUntypedInt {
		if isInteger(b) || isFloat(b) {
			return b
		}
		if b == TypeUntypedFloat {
			return b
		}
		return nil
	}
	if b == TypeUntypedInt {
		if isInteger(a) || isFloat(a) {
			return a
		}
		if a == TypeUntypedFloat {
			return a
		}
		return nil
	}
	// Untyped float adapts to concrete float.
	if a == TypeUntypedFloat {
		if isFloat(b) {
			return b
		}
		return nil
	}
	if b == TypeUntypedFloat {
		if isFloat(a) {
			return a
		}
		return nil
	}
	// Both concrete but different → mismatch.
	return nil
}

// ---------------------------------------------------------------------------
// Symbol
// ---------------------------------------------------------------------------

// SymbolKind describes what a symbol represents.
type SymbolKind int

const (
	SymVar     SymbolKind = iota // variable (let)
	SymFunc                      // user-declared function
	SymBuiltin                   // built-in intrinsic function
	SymReg                       // reserved CPU register
)

// Symbol records the declaration of a name in a scope.
type Symbol struct {
	Name        string
	MangledName string // for functions: the assembly-level name (may differ for overloads)
	Kind        SymbolKind
	Type        *Type // for variables: the declared type; for functions: the return type
	Pos         ast.Position

	// Function-only fields.
	Params       []*Type    // parameter types (in order); nil entries = skip type check
	Defaults     []ast.Expr // default value expressions (parallel to Params; nil = required)
	DefaultCount int        // number of trailing parameters that have defaults
	ReturnType   *Type      // same as Type for functions
}

// ---------------------------------------------------------------------------
// Scope
// ---------------------------------------------------------------------------

// Scope is a symbol table with an optional parent (lexical scoping).
type Scope struct {
	parent        *Scope
	symbols       map[string]*Symbol
	funcOverloads map[string][]*Symbol // original function name → all overloads
}

func newScope(parent *Scope) *Scope {
	return &Scope{
		parent:        parent,
		symbols:       make(map[string]*Symbol),
		funcOverloads: make(map[string][]*Symbol),
	}
}

// define adds a symbol to this scope (overwrites if already present).
func (s *Scope) define(sym *Symbol) {
	s.symbols[sym.Name] = sym
}

// defineFunc adds a function symbol to this scope, supporting overloading.
// The symbol is stored under its MangledName in the symbols map, and also
// added to the funcOverloads list for the original function name.
func (s *Scope) defineFunc(sym *Symbol) {
	key := sym.MangledName
	if key == "" {
		key = sym.Name
	}
	s.symbols[key] = sym
	s.funcOverloads[sym.Name] = append(s.funcOverloads[sym.Name], sym)
}

// lookupLocal returns the symbol with the given name in this scope only.
func (s *Scope) lookupLocal(name string) *Symbol {
	return s.symbols[name]
}

// lookupFuncOverloads returns all overloads of a function by its original
// name, searching up the scope chain.
func (s *Scope) lookupFuncOverloads(name string) []*Symbol {
	if overloads, ok := s.funcOverloads[name]; ok && len(overloads) > 0 {
		return overloads
	}
	if s.parent != nil {
		return s.parent.lookupFuncOverloads(name)
	}
	return nil
}

// lookupFunc finds the best matching function overload for the given name
// and argument types. Returns nil if no match is found.
func (s *Scope) lookupFunc(name string, argTypes []*Type) *Symbol {
	overloads := s.lookupFuncOverloads(name)
	if len(overloads) == 0 {
		return nil
	}
	// If there's only one overload, return it (arity check is done by caller).
	if len(overloads) == 1 {
		return overloads[0]
	}
	// Multiple overloads — find the one whose parameter types match.
	for _, sym := range overloads {
		minArgs := len(sym.Params) - sym.DefaultCount
		if len(argTypes) < minArgs || len(argTypes) > len(sym.Params) {
			continue
		}
		match := true
		for i := 0; i < len(argTypes); i++ {
			paramType := sym.Params[i]
			if paramType == nil || argTypes[i] == nil {
				continue // skip unchecked params (builtins)
			}
			if !isAssignableTo(paramType, argTypes[i]) {
				match = false
				break
			}
		}
		if match {
			return sym
		}
	}
	return nil
}

// lookup traverses the scope chain (current → parent → …) to find a symbol.
func (s *Scope) lookup(name string) *Symbol {
	if sym := s.symbols[name]; sym != nil {
		return sym
	}
	if s.parent != nil {
		return s.parent.lookup(name)
	}
	return nil
}

// ---------------------------------------------------------------------------
// Function name mangling
// ---------------------------------------------------------------------------

// funcMangle returns a mangled name for a function with the given name and
// parameter type names. Non-overloaded functions keep their original name.
// Overloaded functions get a suffix: "itoa.i32", "itoa.i64", etc.
func funcMangle(name string, paramTypeNames []string) string {
	if len(paramTypeNames) == 0 {
		return name
	}
	return name + "." + strings.Join(paramTypeNames, ".")
}

// ---------------------------------------------------------------------------
// Built-in intrinsic functions
// ---------------------------------------------------------------------------

type builtinInfo struct {
	Arity      int
	ReturnType *Type
}

// builtinFuncTable defines the low-level assembly intrinsics available in
// Novus.  Argument types are intentionally unchecked (nil params) because
// these functions accept registers, integers, strings, etc. freely.
//
// getreg and getflag return concrete data that can be used directly in the
// language (u64 and bool respectively).
var builtinFuncTable = map[string]builtinInfo{
	"push":     {Arity: 1, ReturnType: TypeVoid},  // push value onto stack
	"pop":      {Arity: 0, ReturnType: TypeU64},   // pop stack → u64 value
	"lea":      {Arity: 2, ReturnType: TypeVoid},  // load effective address
	"mov":      {Arity: 2, ReturnType: TypeVoid},  // move data
	"call":     {Arity: 1, ReturnType: TypeVoid},  // call function
	"ret":      {Arity: 0, ReturnType: TypeVoid},  // return from call
	"syscall":  {Arity: 0, ReturnType: TypeVoid},  // invoke system call
	"int":      {Arity: 1, ReturnType: TypeVoid},  // software interrupt
	"setreg":   {Arity: 2, ReturnType: TypeVoid},  // set register value
	"getreg":   {Arity: 1, ReturnType: TypeU64},   // read register → u64
	"nop":      {Arity: 0, ReturnType: TypeVoid},  // no operation
	"setflag":  {Arity: 2, ReturnType: TypeVoid},  // set CPU flag
	"getflag":  {Arity: 1, ReturnType: TypeBool},  // read CPU flag → bool
	"load8":    {Arity: 1, ReturnType: TypeI32},   // load byte from address → i32
	"load32":   {Arity: 1, ReturnType: TypeI32},   // load 32-bit int from address → i32
	"load64":   {Arity: 1, ReturnType: TypeI64},   // load 64-bit int from address → i64
	"len":      {Arity: 1, ReturnType: TypeI32},   // string length → i32
	"win_call": {Arity: -1, ReturnType: TypeVoid}, // call Windows API (variadic: first arg is function name string)
}

// ---------------------------------------------------------------------------
// Reserved CPU registers
// ---------------------------------------------------------------------------

var reservedRegisters = []string{
	// x86 32-bit general purpose
	"eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp",
	// x86 64-bit general purpose
	"rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp",
	"r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
	// x86 Instruction pointers
	"eip", "rip",
	// x86 Flags
	"eflags", "rflags",
	// x87 FPU
	"st0", "st1", "st2", "st3", "st4", "st5", "st6", "st7",
	// MMX
	"mm0", "mm1", "mm2", "mm3", "mm4", "mm5", "mm6", "mm7",
	// SSE
	"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
	"xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
	// ARM64 64-bit general purpose
	"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
	"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
	"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
	"x24", "x25", "x26", "x27", "x28", "x29", "x30",
	// ARM64 32-bit general purpose
	"w0", "w1", "w2", "w3", "w4", "w5", "w6", "w7",
	"w8", "w9", "w10", "w11", "w12", "w13", "w14", "w15",
	"w16", "w17", "w18", "w19", "w20", "w21", "w22", "w23",
	"w24", "w25", "w26", "w27", "w28", "w29", "w30",
	// ARM64 special registers
	"sp", "xzr", "wzr", "lr",
}

// ---------------------------------------------------------------------------
// Analyser
// ---------------------------------------------------------------------------

// ImportedFunc describes a function brought into scope by an import.
type ImportedFunc struct {
	Fn    *ast.FnDecl
	Alias string // "" for direct access, otherwise the namespace prefix
}

// Analyzer holds the state for a single semantic-analysis pass.
type Analyzer struct {
	diagnostics   []Diagnostic
	scope         *Scope
	currentFunc   *ast.FnDecl // the function body we are currently inside
	loopDepth     int         // > 0 when inside a loop
	importedFuncs []ImportedFunc
	importAliases map[string]bool      // set of known import aliases (for MemberExpr resolution)
	importedFnSet map[*ast.FnDecl]bool // FnDecl pointers registered via imports (skip in prog.Functions pass)
}

// Analyze runs semantic analysis on the given AST program and returns all
// diagnostics (errors and warnings).  The returned slice is empty when the
// program is semantically valid.
func Analyze(program *ast.Program) []Diagnostic {
	return AnalyzeWithImports(program, nil)
}

// AnalyzeWithImports runs semantic analysis with imported function metadata.
func AnalyzeWithImports(program *ast.Program, importedFuncs []ImportedFunc) []Diagnostic {
	a := &Analyzer{
		scope:         newScope(nil), // global scope
		importedFuncs: importedFuncs,
		importAliases: make(map[string]bool),
		importedFnSet: make(map[*ast.FnDecl]bool),
	}
	a.injectBuiltins()
	a.analyzeProgram(program)
	return a.diagnostics
}

// ---- helpers ----

func (a *Analyzer) error(pos ast.Position, msg string) {
	a.diagnostics = append(a.diagnostics, Diagnostic{
		Message:  msg,
		Pos:      pos,
		Severity: Error,
	})
}

func (a *Analyzer) warn(pos ast.Position, msg string) {
	a.diagnostics = append(a.diagnostics, Diagnostic{
		Message:  msg,
		Pos:      pos,
		Severity: Warning,
	})
}

func (a *Analyzer) pushScope() {
	a.scope = newScope(a.scope)
}

func (a *Analyzer) popScope() {
	a.scope = a.scope.parent
}

// ---------------------------------------------------------------------------
// Inject built-in symbols into global scope
// ---------------------------------------------------------------------------

func (a *Analyzer) injectBuiltins() {
	// Built-in intrinsic functions.
	for name, info := range builtinFuncTable {
		var params []*Type
		if info.Arity >= 0 {
			params = make([]*Type, info.Arity) // nil entries → skip arg type check
		}
		a.scope.define(&Symbol{
			Name:       name,
			Kind:       SymBuiltin,
			Type:       info.ReturnType,
			Params:     params,
			ReturnType: info.ReturnType,
		})
	}

	// Reserved CPU registers.
	for _, name := range reservedRegisters {
		a.scope.define(&Symbol{
			Name: name,
			Kind: SymReg,
			Type: TypeReg,
		})
	}
}

// ---------------------------------------------------------------------------
// Program analysis
// ---------------------------------------------------------------------------

func (a *Analyzer) analyzeProgram(prog *ast.Program) {
	// Register imported functions in scope.
	// Pre-scan: count imported function names to detect overloads.
	importNameCount := map[string]int{}
	for _, imp := range a.importedFuncs {
		importNameCount[imp.Fn.Name]++
	}

	for _, imp := range a.importedFuncs {
		fn := imp.Fn
		retType := a.resolveType(fn.ReturnType)
		paramTypes := make([]*Type, len(fn.Params))
		paramTypeNames := make([]string, len(fn.Params))
		defaults := make([]ast.Expr, len(fn.Params))
		defaultCount := 0
		for i, p := range fn.Params {
			paramTypes[i] = a.resolveType(p.Type)
			if p.Type != nil {
				paramTypeNames[i] = p.Type.Name
			} else {
				paramTypeNames[i] = "void"
			}
			if p.Default != nil {
				defaults[i] = p.Default
				defaultCount++
			}
		}

		mangledName := fn.MangledName
		if mangledName == "" {
			mangledName = fn.Name
		}
		// Apply overload mangling for imported functions with the same name.
		if importNameCount[fn.Name] > 1 {
			mangledName = funcMangle(fn.Name, paramTypeNames)
		}
		fn.MangledName = mangledName

		// Track this FnDecl so the prog.Functions pass skips it.
		a.importedFnSet[fn] = true

		if imp.Alias != "" {
			// Namespaced import: register under "alias.funcName" so MemberExpr can resolve it.
			a.importAliases[imp.Alias] = true
			qualifiedName := imp.Alias + "." + fn.Name
			// For overloaded functions, also store under the mangled qualified name.
			qualifiedMangled := imp.Alias + "." + mangledName
			sym := &Symbol{
				Name:         fn.Name,
				MangledName:  mangledName,
				Kind:         SymFunc,
				Type:         retType,
				Pos:          fn.Pos,
				Params:       paramTypes,
				Defaults:     defaults,
				DefaultCount: defaultCount,
				ReturnType:   retType,
			}
			a.scope.symbols[qualifiedMangled] = sym
			// Also store under the base qualified name for non-overloaded single lookups.
			if qualifiedMangled == qualifiedName {
				// Not overloaded — mangled == base, single entry is enough.
			} else {
				// Overloaded — keep the base name pointing to the last one for fallback.
				a.scope.symbols[qualifiedName] = sym
			}
			a.scope.funcOverloads[qualifiedName] = append(a.scope.funcOverloads[qualifiedName], sym)
		} else {
			// Direct import: register under the function's own name.
			sym := &Symbol{
				Name:         fn.Name,
				MangledName:  mangledName,
				Kind:         SymFunc,
				Type:         retType,
				Pos:          fn.Pos,
				Params:       paramTypes,
				Defaults:     defaults,
				DefaultCount: defaultCount,
				ReturnType:   retType,
			}
			a.scope.defineFunc(sym)
		}
	}

	// Pre-scan: detect which function names have multiple declarations
	// (candidates for overloading).
	nameCount := map[string]int{}
	for _, fn := range prog.Functions {
		nameCount[fn.Name]++
	}

	// First pass — register every top-level function so that they can call
	// each other (including recursion) regardless of source order.
	for _, fn := range prog.Functions {
		// Skip functions already registered via the imports pass.
		if a.importedFnSet[fn] {
			continue
		}

		// Check for collisions with builtins / registers.
		if existing := a.scope.lookupLocal(fn.Name); existing != nil {
			if existing.Kind == SymBuiltin {
				// Allow user functions to shadow builtins (the lowerer's
				// builtin handler will still take priority for the builtin name).
				a.warn(fn.Pos, fmt.Sprintf("function %q shadows built-in intrinsic", fn.Name))
			}
			if existing.Kind == SymReg {
				a.error(fn.Pos, fmt.Sprintf("cannot use reserved register name %q as function name", fn.Name))
				continue
			}
		}

		retType := a.resolveType(fn.ReturnType)
		paramTypes := make([]*Type, len(fn.Params))
		paramTypeNames := make([]string, len(fn.Params))
		defaults := make([]ast.Expr, len(fn.Params))
		defaultCount := 0
		seenDefault := false
		for i, p := range fn.Params {
			paramTypes[i] = a.resolveType(p.Type)
			if p.Type != nil {
				paramTypeNames[i] = p.Type.Name
			} else {
				paramTypeNames[i] = "void"
			}
			if p.Default != nil {
				seenDefault = true
				defaults[i] = p.Default
				defaultCount++
			} else if seenDefault {
				a.error(p.Pos, fmt.Sprintf("parameter %q without default follows parameter with default", p.Name))
			}
		}

		// Compute the mangled name. Only overloaded functions get a suffix.
		mangledName := fn.Name
		if nameCount[fn.Name] > 1 {
			mangledName = funcMangle(fn.Name, paramTypeNames)
		}

		// Check for duplicate with the same mangled signature.
		if existing := a.scope.lookupLocal(mangledName); existing != nil {
			if existing.Kind == SymBuiltin {
				// Shadowing a builtin — remove the builtin entry so the user function takes over.
				delete(a.scope.symbols, mangledName)
			} else {
				a.error(fn.Pos, fmt.Sprintf("function %q with signature (%s) already declared at %s",
					fn.Name, strings.Join(paramTypeNames, ", "), existing.Pos))
				continue
			}
		}

		// Store mangled name on the AST node for the lowerer.
		fn.MangledName = mangledName

		sym := &Symbol{
			Name:         fn.Name,
			MangledName:  mangledName,
			Kind:         SymFunc,
			Type:         retType,
			Pos:          fn.Pos,
			Params:       paramTypes,
			Defaults:     defaults,
			DefaultCount: defaultCount,
			ReturnType:   retType,
		}
		a.scope.defineFunc(sym)
	}

	// Second pass — analyse each function body.
	for _, fn := range prog.Functions {
		// Skip functions already registered via imports (they're analyzed in their own module context).
		if a.importedFnSet[fn] {
			continue
		}
		// Skip functions that failed registration (reserved names).
		mangledName := fn.MangledName
		if mangledName == "" {
			mangledName = fn.Name
		}
		if sym := a.scope.lookupLocal(mangledName); sym == nil || (sym.Kind != SymFunc) {
			continue
		}
		a.analyzeFunction(fn)
	}
}

func (a *Analyzer) analyzeFunction(fn *ast.FnDecl) {
	a.currentFunc = fn
	a.pushScope()

	// Register parameters in the function scope.
	for _, param := range fn.Params {
		pType := a.resolveType(param.Type)
		if pType == TypeVoid {
			a.error(param.Pos, fmt.Sprintf("parameter %q cannot have type void", param.Name))
		}

		// Prevent parameter names that collide with builtins/registers.
		if existing := a.scope.lookup(param.Name); existing != nil && (existing.Kind == SymBuiltin || existing.Kind == SymReg) {
			a.error(param.Pos, fmt.Sprintf("cannot use reserved name %q as parameter", param.Name))
			continue
		}

		if existing := a.scope.lookupLocal(param.Name); existing != nil {
			a.error(param.Pos, fmt.Sprintf("duplicate parameter %q", param.Name))
			continue
		}
		a.scope.define(&Symbol{
			Name: param.Name,
			Kind: SymVar,
			Type: pType,
			Pos:  param.Pos,
		})

		// Type-check the default value if present.
		if param.Default != nil {
			defType := a.analyzeExpr(param.Default)
			if defType != nil && pType != nil && !isAssignableTo(pType, defType) {
				a.error(param.Pos, fmt.Sprintf("default value for %q: expected %s, got %s",
					param.Name, pType.Name, defType.Name))
			}
		}
	}

	// Analyse the function body in a nested scope so that body-level
	// declarations shadow (warn) rather than duplicate (error) parameters.
	a.pushScope()
	a.analyzeBlock(fn.Body)
	a.popScope()

	// Non-void functions must return a value on every path.
	retType := a.resolveType(fn.ReturnType)
	if retType != nil && retType != TypeVoid && !a.blockReturns(fn.Body) {
		a.error(fn.Pos, fmt.Sprintf("function %q must return a value of type %s on all paths", fn.Name, retType.Name))
	}

	a.popScope()
	a.currentFunc = nil
}

func (a *Analyzer) analyzeBlock(block *ast.BlockStmt) {
	for _, stmt := range block.Stmts {
		a.analyzeStmt(stmt)
	}
}

// ---------------------------------------------------------------------------
// Statement analysis
// ---------------------------------------------------------------------------

func (a *Analyzer) analyzeStmt(stmt ast.Stmt) {
	switch s := stmt.(type) {
	case *ast.LetStmt:
		a.analyzeLetStmt(s)
	case *ast.ReturnStmt:
		a.analyzeReturnStmt(s)
	case *ast.BreakStmt:
		if a.loopDepth == 0 {
			a.error(s.Pos, "break statement outside of loop")
		}
	case *ast.ContinueStmt:
		if a.loopDepth == 0 {
			a.error(s.Pos, "continue statement outside of loop")
		}
	case *ast.IfStmt:
		a.analyzeIfStmt(s)
	case *ast.WhileStmt:
		a.analyzeWhileStmt(s)
	case *ast.ForStmt:
		a.analyzeForStmt(s)
	case *ast.ExprStmt:
		a.analyzeExpr(s.Expression)
	case *ast.AssignStmt:
		a.analyzeAssignStmt(s)
	case *ast.BlockStmt:
		a.pushScope()
		a.analyzeBlock(s)
		a.popScope()
	}
}

// ---- Let ----

func (a *Analyzer) analyzeLetStmt(s *ast.LetStmt) {
	declType := a.resolveType(s.Type)

	if declType == TypeVoid {
		a.error(s.Pos, fmt.Sprintf("variable %q cannot have type void", s.Name))
	}

	valType := a.analyzeExpr(s.Value)

	// Type-mismatch check (skip when either side is unknown or void).
	if declType != nil && declType != TypeVoid && valType != nil {
		if !isAssignableTo(declType, valType) {
			a.error(s.Pos, fmt.Sprintf("cannot assign %s value to variable %q of type %s", valType.Name, s.Name, declType.Name))
		}
	}

	// Prevent redeclaring built-in functions or registers.
	if existing := a.scope.lookup(s.Name); existing != nil && (existing.Kind == SymBuiltin || existing.Kind == SymReg) {
		a.error(s.Pos, fmt.Sprintf("cannot declare variable with reserved name %q", s.Name))
		return
	}

	// Shadowing: same-scope duplicate is an error; outer-scope shadow is a
	// warning (shadowing is always allowed, but we warn in the build).
	if existing := a.scope.lookupLocal(s.Name); existing != nil {
		a.error(s.Pos, fmt.Sprintf("variable %q already declared in this scope at %s", s.Name, existing.Pos))
	} else if existing := a.scope.lookup(s.Name); existing != nil {
		a.warn(s.Pos, fmt.Sprintf("variable %q shadows previous declaration at %s", s.Name, existing.Pos))
	}

	a.scope.define(&Symbol{
		Name: s.Name,
		Kind: SymVar,
		Type: declType,
		Pos:  s.Pos,
	})
}

// ---- Return ----

func (a *Analyzer) analyzeReturnStmt(s *ast.ReturnStmt) {
	if a.currentFunc == nil {
		a.error(s.Pos, "return statement outside of function")
		return
	}

	retType := a.resolveType(a.currentFunc.ReturnType)

	if s.Value == nil {
		// Bare  return;
		if retType != nil && retType != TypeVoid {
			a.error(s.Pos, fmt.Sprintf("function %q expects return type %s, got void", a.currentFunc.Name, retType.Name))
		}
		return
	}

	valType := a.analyzeExpr(s.Value)

	if retType == TypeVoid {
		a.error(s.Pos, fmt.Sprintf("void function %q should not return a value", a.currentFunc.Name))
	} else if valType != nil && retType != nil {
		if !isAssignableTo(retType, valType) {
			a.error(s.Pos, fmt.Sprintf("cannot return %s from function %q with return type %s", valType.Name, a.currentFunc.Name, retType.Name))
		}
	}
}

// ---- If ----

func (a *Analyzer) analyzeIfStmt(s *ast.IfStmt) {
	condType := a.analyzeExpr(s.Condition)
	if condType != nil && condType != TypeBool {
		a.error(s.Condition.GetPos(), fmt.Sprintf("if condition must be bool, got %s", condType.Name))
	}

	a.pushScope()
	a.analyzeBlock(s.Then)
	a.popScope()

	if s.Else != nil {
		switch e := s.Else.(type) {
		case *ast.BlockStmt:
			a.pushScope()
			a.analyzeBlock(e)
			a.popScope()
		case *ast.IfStmt:
			a.analyzeIfStmt(e)
		}
	}
}

// ---- While ----

func (a *Analyzer) analyzeWhileStmt(s *ast.WhileStmt) {
	condType := a.analyzeExpr(s.Condition)
	if condType != nil && condType != TypeBool {
		a.error(s.Condition.GetPos(), fmt.Sprintf("while condition must be bool, got %s", condType.Name))
	}

	a.loopDepth++
	a.pushScope()
	a.analyzeBlock(s.Body)
	a.popScope()
	a.loopDepth--
}

// ---- For ----

func (a *Analyzer) analyzeForStmt(s *ast.ForStmt) {
	a.pushScope() // for-loop scope (contains init variable)

	if s.Init != nil {
		a.analyzeStmt(s.Init)
	}

	condType := a.analyzeExpr(s.Condition)
	if condType != nil && condType != TypeBool {
		a.error(s.Condition.GetPos(), fmt.Sprintf("for condition must be bool, got %s", condType.Name))
	}

	if s.Update != nil {
		a.analyzeStmt(s.Update)
	}

	a.loopDepth++
	a.pushScope() // body scope
	a.analyzeBlock(s.Body)
	a.popScope()
	a.loopDepth--

	a.popScope() // for-loop scope
}

// ---- Assign ----

func (a *Analyzer) analyzeAssignStmt(s *ast.AssignStmt) {
	targetType := a.analyzeExpr(s.Target)
	valType := a.analyzeExpr(s.Value)

	// Only identifiers, member expressions, and index expressions are valid
	// assignment targets.
	switch s.Target.(type) {
	case *ast.IdentExpr, *ast.MemberExpr, *ast.IndexExpr:
		// OK
	default:
		a.error(s.Pos, "invalid assignment target")
	}

	if targetType != nil && valType != nil {
		if !isAssignableTo(targetType, valType) {
			a.error(s.Pos, fmt.Sprintf("cannot assign %s to %s", valType.Name, targetType.Name))
		}
	}
}

// ---------------------------------------------------------------------------
// Expression analysis — returns the resolved type (nil = unknown)
// ---------------------------------------------------------------------------

func (a *Analyzer) analyzeExpr(expr ast.Expr) *Type {
	if expr == nil {
		return nil
	}

	switch e := expr.(type) {
	case *ast.IdentExpr:
		return a.analyzeIdentExpr(e)
	case *ast.IntLitExpr:
		return TypeUntypedInt
	case *ast.FloatLitExpr:
		return TypeUntypedFloat
	case *ast.StringLitExpr:
		return TypeStr
	case *ast.BoolLitExpr:
		return TypeBool
	case *ast.UnaryExpr:
		return a.analyzeUnaryExpr(e)
	case *ast.BinaryExpr:
		return a.analyzeBinaryExpr(e)
	case *ast.CallExpr:
		return a.analyzeCallExpr(e)
	case *ast.MemberExpr:
		return a.analyzeMemberExpr(e)
	case *ast.IndexExpr:
		return a.analyzeIndexExpr(e)
	case *ast.GroupExpr:
		return a.analyzeExpr(e.Expression)
	case *ast.AddressOfExpr:
		a.analyzeExpr(e.Operand)
		return nil // pointer types not yet modelled
	case *ast.ArrayLitExpr:
		return a.analyzeArrayLitExpr(e)
	}

	return nil
}

func (a *Analyzer) analyzeIdentExpr(e *ast.IdentExpr) *Type {
	if e.Name == "<error>" {
		return nil // parser error-recovery placeholder
	}
	// Import alias names are valid identifiers (used as namespace prefixes).
	if a.importAliases[e.Name] {
		return nil
	}
	sym := a.scope.lookup(e.Name)
	if sym == nil {
		a.error(e.Pos, fmt.Sprintf("undefined identifier %q", e.Name))
		return nil
	}
	return sym.Type
}

// ---- Unary ----

func (a *Analyzer) analyzeUnaryExpr(e *ast.UnaryExpr) *Type {
	operandType := a.analyzeExpr(e.Operand)
	if operandType == nil {
		return nil
	}

	switch e.Op {
	case "!":
		if operandType != TypeBool {
			a.error(e.Pos, fmt.Sprintf("operator '!' requires bool operand, got %s", operandType.Name))
			return nil
		}
		return TypeBool
	case "-":
		if !isNumeric(operandType) {
			a.error(e.Pos, fmt.Sprintf("operator '-' requires numeric operand, got %s", operandType.Name))
			return nil
		}
		return operandType
	}

	return nil
}

// ---- Binary ----

func (a *Analyzer) analyzeBinaryExpr(e *ast.BinaryExpr) *Type {
	leftType := a.analyzeExpr(e.Left)
	rightType := a.analyzeExpr(e.Right)

	if leftType == nil || rightType == nil {
		return nil
	}

	switch e.Op {
	case "+":
		// String concatenation.
		if leftType == TypeStr && rightType == TypeStr {
			return TypeStr
		}
		if !isNumeric(leftType) || !isNumeric(rightType) {
			a.error(e.Pos, fmt.Sprintf("operator '+' requires numeric or string operands, got %s and %s", leftType.Name, rightType.Name))
			return nil
		}
		resolved := resolveNumericPair(leftType, rightType)
		if resolved == nil {
			a.error(e.Pos, fmt.Sprintf("mismatched types for '+': %s and %s", leftType.Name, rightType.Name))
			return nil
		}
		return resolved

	case "-", "*", "/", "%":
		if !isNumeric(leftType) || !isNumeric(rightType) {
			a.error(e.Pos, fmt.Sprintf("operator %q requires numeric operands, got %s and %s", e.Op, leftType.Name, rightType.Name))
			return nil
		}
		resolved := resolveNumericPair(leftType, rightType)
		if resolved == nil {
			a.error(e.Pos, fmt.Sprintf("mismatched types for %q: %s and %s", e.Op, leftType.Name, rightType.Name))
			return nil
		}
		return resolved

	case "==", "!=":
		if !isComparable(leftType, rightType) {
			a.error(e.Pos, fmt.Sprintf("mismatched types for %q: %s and %s", e.Op, leftType.Name, rightType.Name))
		}
		return TypeBool

	case "<", ">", "<=", ">=":
		if !isNumeric(leftType) || !isNumeric(rightType) {
			a.error(e.Pos, fmt.Sprintf("operator %q requires numeric operands, got %s and %s", e.Op, leftType.Name, rightType.Name))
			return TypeBool
		}
		if resolveNumericPair(leftType, rightType) == nil {
			a.error(e.Pos, fmt.Sprintf("mismatched types for %q: %s and %s", e.Op, leftType.Name, rightType.Name))
		}
		return TypeBool

	case "&&", "||":
		if leftType != TypeBool {
			a.error(e.Pos, fmt.Sprintf("operator %q requires bool operands, got %s", e.Op, leftType.Name))
		}
		if rightType != TypeBool {
			a.error(e.Pos, fmt.Sprintf("operator %q requires bool operands, got %s", e.Op, rightType.Name))
		}
		return TypeBool
	}

	return nil
}

// ---- Call ----

func (a *Analyzer) analyzeCallExpr(e *ast.CallExpr) *Type {
	switch callee := e.Callee.(type) {
	case *ast.IdentExpr:
		if callee.Name == "<error>" {
			for _, arg := range e.Args {
				a.analyzeExpr(arg)
			}
			return nil
		}

		// First, analyze all argument types so we can resolve overloads.
		argTypes := make([]*Type, len(e.Args))
		for i, arg := range e.Args {
			argTypes[i] = a.analyzeExpr(arg)
		}

		// Special handling for array builtins: array_append(arr, val), array_pop(arr).
		switch callee.Name {
		case "array_append":
			if len(e.Args) == 2 && len(argTypes) == 2 && argTypes[0] != nil && argTypes[0].IsArray {
				e.ResolvedCallee = "array_append"
				return TypeVoid
			}
		case "array_pop":
			if len(e.Args) == 1 && len(argTypes) == 1 && argTypes[0] != nil && argTypes[0].IsArray {
				e.ResolvedCallee = "array_pop"
				return argTypes[0].ElemType
			}
		}

		// Try to resolve as a builtin or non-overloaded symbol first.
		sym := a.scope.lookup(callee.Name)

		// If not found directly, or if found but it's not callable, try overloads.
		if sym == nil || (sym.Kind != SymFunc && sym.Kind != SymBuiltin) {
			// Try function overloads.
			overloadSym := a.scope.lookupFunc(callee.Name, argTypes)
			if overloadSym != nil {
				sym = overloadSym
			}
		} else if sym.Kind == SymFunc {
			// Found a function; check if there are overloads and pick the best match.
			overloads := a.scope.lookupFuncOverloads(callee.Name)
			if len(overloads) > 1 {
				overloadSym := a.scope.lookupFunc(callee.Name, argTypes)
				if overloadSym != nil {
					sym = overloadSym
				} else {
					// No exact match — report error with available overloads.
					a.error(e.Pos, fmt.Sprintf("no matching overload of %q for the given argument types", callee.Name))
					return nil
				}
			}
		}

		if sym == nil {
			a.error(callee.Pos, fmt.Sprintf("undefined function %q", callee.Name))
			return nil
		}

		// Only functions and builtins are callable.
		if sym.Kind != SymFunc && sym.Kind != SymBuiltin {
			a.error(callee.Pos, fmt.Sprintf("%q is not a function", callee.Name))
			return nil
		}

		// Arity check — allow fewer args when trailing defaults exist.
		// sym.Params is nil for variadic builtins (arity = -1), so skip check.
		if sym.Params != nil {
			minArgs := len(sym.Params) - sym.DefaultCount
			if len(e.Args) < minArgs || len(e.Args) > len(sym.Params) {
				a.error(e.Pos, fmt.Sprintf("function %q expects %d to %d arguments, got %d",
					callee.Name, minArgs, len(sym.Params), len(e.Args)))
			}

			// Fill in default values for omitted arguments.
			if len(e.Args) < len(sym.Params) && sym.DefaultCount > 0 {
				for i := len(e.Args); i < len(sym.Params); i++ {
					if sym.Defaults[i] != nil {
						e.Args = append(e.Args, sym.Defaults[i])
						// Analyse the default expression and add its type.
						defType := a.analyzeExpr(sym.Defaults[i])
						argTypes = append(argTypes, defType)
					}
				}
			}

			// Argument type checks (nil param entries = skip, used by builtins).
			for i := range e.Args {
				if i < len(sym.Params) && sym.Params[i] != nil && i < len(argTypes) && argTypes[i] != nil {
					if !isAssignableTo(sym.Params[i], argTypes[i]) {
						a.error(e.Args[i].GetPos(), fmt.Sprintf("argument %d of %q: expected %s, got %s", i+1, callee.Name, sym.Params[i].Name, argTypes[i].Name))
					}
				}
			}
		} else if len(e.Args) < 1 && callee.Name == "win_call" {
			a.error(e.Pos, fmt.Sprintf("function %q requires at least 1 argument (API function name)", callee.Name))
		}

		// Set the resolved callee for the lowerer (mangled name for overloads).
		if sym.MangledName != "" && sym.MangledName != sym.Name {
			e.ResolvedCallee = sym.MangledName
		} else {
			e.ResolvedCallee = sym.Name
		}

		return sym.ReturnType

	case *ast.MemberExpr:
		// Module-qualified calls (e.g. std.test()) — resolve via import aliases.
		if ident, ok := callee.Object.(*ast.IdentExpr); ok && a.importAliases[ident.Name] {
			alias := ident.Name
			funcName := callee.Field
			qualifiedName := alias + "." + funcName

			// Analyze argument types.
			argTypes := make([]*Type, len(e.Args))
			for i, arg := range e.Args {
				argTypes[i] = a.analyzeExpr(arg)
			}

			// Try overload resolution first (handles overloaded imported functions).
			overloads := a.scope.lookupFuncOverloads(qualifiedName)
			var sym *Symbol
			if len(overloads) > 1 {
				// Multiple overloads — pick the best match by arg types.
				sym = a.scope.lookupFunc(qualifiedName, argTypes)
				if sym == nil {
					// Build a readable signature list.
					a.error(callee.GetPos(), fmt.Sprintf("no matching overload for %q in import alias %q", funcName, alias))
					return nil
				}
			} else {
				sym = a.scope.lookup(qualifiedName)
			}
			if sym == nil {
				a.error(callee.GetPos(), fmt.Sprintf("undefined function %q in import alias %q", funcName, alias))
				return nil
			}

			// Arity check.
			minArgs := len(sym.Params) - sym.DefaultCount
			if len(e.Args) < minArgs || len(e.Args) > len(sym.Params) {
				a.error(e.Pos, fmt.Sprintf("function %q expects %d to %d arguments, got %d",
					funcName, minArgs, len(sym.Params), len(e.Args)))
			}

			// Fill in defaults.
			if len(e.Args) < len(sym.Params) && sym.DefaultCount > 0 {
				for i := len(e.Args); i < len(sym.Params); i++ {
					if sym.Defaults[i] != nil {
						e.Args = append(e.Args, sym.Defaults[i])
						argTypes = append(argTypes, a.analyzeExpr(sym.Defaults[i]))
					}
				}
			}

			// Argument type checks.
			for i := range e.Args {
				if i < len(sym.Params) && sym.Params[i] != nil && i < len(argTypes) && argTypes[i] != nil {
					if !isAssignableTo(sym.Params[i], argTypes[i]) {
						a.error(e.Args[i].GetPos(), fmt.Sprintf("argument %d of %q: expected %s, got %s",
							i+1, funcName, sym.Params[i].Name, argTypes[i].Name))
					}
				}
			}

			// Set the resolved callee to the mangled name.
			if sym.MangledName != "" {
				e.ResolvedCallee = sym.MangledName
			} else {
				e.ResolvedCallee = sym.Name
			}
			return sym.ReturnType
		}

		// Unknown module — fall through to generic handling.
		a.analyzeExpr(callee.Object)
		for _, arg := range e.Args {
			a.analyzeExpr(arg)
		}
		return nil

	default:
		a.analyzeExpr(e.Callee)
		for _, arg := range e.Args {
			a.analyzeExpr(arg)
		}
		return nil
	}
}

// ---- Member ----

func (a *Analyzer) analyzeMemberExpr(e *ast.MemberExpr) *Type {
	// For import aliases used in call expressions, the CallExpr handler does resolution.
	// For standalone member access (not in call position), check if it's a known alias.
	if ident, ok := e.Object.(*ast.IdentExpr); ok && a.importAliases[ident.Name] {
		// This is an aliased import reference — type will be resolved in call context.
		return nil
	}
	a.analyzeExpr(e.Object)
	// Member access on imported modules / structs cannot be resolved yet.
	return nil
}

// ---- Index ----

func (a *Analyzer) analyzeArrayLitExpr(e *ast.ArrayLitExpr) *Type {
	// Analyse each element expression. If non-empty, infer element type from first element.
	var elemType *Type
	for _, el := range e.Elems {
		t := a.analyzeExpr(el)
		if t == nil {
			continue
		}
		if elemType == nil {
			elemType = t
		}
	}
	// For empty array literals, the type will be resolved at the let-statement level
	// based on the declared type annotation.
	if elemType == nil {
		// Return a placeholder array type; the let-stmt will refine it.
		return &Type{Name: "[]<unknown>", IsArray: true, ElemType: TypeI64}
	}
	// Keep untyped element types so the let-stmt can coerce to the declared array type.
	arrTypeName := "[]" + elemType.Name
	return &Type{Name: arrTypeName, IsArray: true, ElemType: elemType}
}

func (a *Analyzer) analyzeIndexExpr(e *ast.IndexExpr) *Type {
	objType := a.analyzeExpr(e.Object)
	idxType := a.analyzeExpr(e.Index)

	if objType == nil || idxType == nil {
		return nil
	}

	// Index must be an integer type (concrete or untyped).
	if !isInteger(idxType) && idxType != TypeUntypedInt {
		a.error(e.Index.GetPos(), fmt.Sprintf("index must be an integer type, got %s", idxType.Name))
		return nil
	}

	// For now, the only indexable types are str and arrays.
	if objType == TypeStr {
		return TypeStr
	}
	if objType.IsArray {
		return objType.ElemType
	}

	a.error(e.Pos, fmt.Sprintf("type %s is not indexable", objType.Name))
	return nil
}

// ---------------------------------------------------------------------------
// Type resolution helper
// ---------------------------------------------------------------------------

func (a *Analyzer) resolveType(te *ast.TypeExpr) *Type {
	if te == nil {
		return TypeVoid
	}
	if te.Name == "<error>" {
		return nil // parser error-recovery placeholder
	}
	t := LookupType(te.Name)
	if t == nil {
		a.error(te.Pos, fmt.Sprintf("unknown type %q", te.Name))
		return nil
	}
	return t
}

// ---------------------------------------------------------------------------
// Return-path analysis
// ---------------------------------------------------------------------------

// blockReturns reports whether every execution path through the block ends
// with a return statement.
func (a *Analyzer) blockReturns(block *ast.BlockStmt) bool {
	if len(block.Stmts) == 0 {
		return false
	}
	last := block.Stmts[len(block.Stmts)-1]
	return a.stmtReturns(last)
}

// stmtReturns reports whether a statement unconditionally returns.
func (a *Analyzer) stmtReturns(stmt ast.Stmt) bool {
	switch s := stmt.(type) {
	case *ast.ReturnStmt:
		return true
	case *ast.IfStmt:
		if s.Else == nil {
			return false // no else → not guaranteed
		}
		thenReturns := a.blockReturns(s.Then)
		var elseReturns bool
		switch e := s.Else.(type) {
		case *ast.BlockStmt:
			elseReturns = a.blockReturns(e)
		case *ast.IfStmt:
			elseReturns = a.stmtReturns(e)
		}
		return thenReturns && elseReturns
	case *ast.BlockStmt:
		return a.blockReturns(s)
	}
	return false
}

package semantic

import (
	"fmt"
	"novus/internal/ast"
	"strings"
)

type classInfo struct {
	decl        *ast.ClassDecl
	typ         *Type
	fields      map[string]*fieldInfo
	methods     map[string][]*methodInfo
	constructor []*methodInfo
	slots       int
}

type fieldInfo struct {
	decl    *ast.ClassField
	typ     *Type
	storage ast.ResolvedField
}

type methodInfo struct {
	decl *ast.ClassMethod
	sym  *Symbol
}

func (a *Analyzer) classError(pos ast.Position, message string) {
	a.error(pos, message)
	a.diagnostics[len(a.diagnostics)-1].classRelated = true
}

func (a *Analyzer) collectClasses(prog *ast.Program) {
	for i, decl := range prog.Classes {
		class := &classInfo{
			decl:    decl,
			fields:  make(map[string]*fieldInfo),
			methods: make(map[string][]*methodInfo),
		}
		class.typ = &Type{Name: decl.Name, class: class, identity: fmt.Sprintf("_novus_type_%d", i)}
		a.classDecls[decl] = class
		a.bindClass(decl.SourceFile, decl.Name, class, decl.Pos)
	}
	for file, bindings := range prog.ClassScopes {
		for _, binding := range bindings {
			a.bindClass(file, binding.Name, a.classDecls[binding.Class], binding.Pos)
		}
	}
}

func (a *Analyzer) bindClass(file, name string, class *classInfo, pos ast.Position) {
	if a.classes[file] == nil {
		a.classes[file] = make(map[string]*classInfo)
	}
	if existing := a.classes[file][name]; existing != nil {
		if existing != class {
			a.classError(pos, fmt.Sprintf("class name %q is ambiguous or already declared", name))
		}
		return
	}
	if sym := a.scope.lookup(name); sym != nil && (sym.Kind == SymBuiltin || sym.Kind == SymReg) {
		a.classError(pos, fmt.Sprintf("cannot use reserved name %q as a class name", name))
	}
	a.classes[file][name] = class
}

func typeKey(t *Type, fallback string) string {
	if t == nil {
		return fallback
	}
	if t.class != nil {
		return t.identity
	}
	if t.IsArray {
		return "[]" + typeKey(t.ElemType, t.Name[2:])
	}
	return t.Name
}

func containsClass(t *Type) bool {
	return t != nil && (t.class != nil || containsClass(t.ElemType))
}

func (a *Analyzer) classType(name string) *classInfo {
	return a.classes[a.sourceFile][name]
}

func (a *Analyzer) collectClassMembers(prog *ast.Program) {
	for _, decl := range prog.Classes {
		a.sourceFile = decl.SourceFile
		class := a.classDecls[decl]
		prefix := class.typ.identity
		for i, field := range decl.Fields {
			if class.fields[field.Name] != nil || field.Name == "init" || field.Name == "self" {
				a.classError(field.Pos, fmt.Sprintf("invalid or duplicate member %q in class %s", field.Name, decl.Name))
				continue
			}
			typ := a.resolveType(field.Type)
			if typ == TypeVoid {
				a.classError(field.Pos, fmt.Sprintf("field %q cannot have type void", field.Name))
			}
			info := &fieldInfo{decl: field, typ: typ, storage: ast.ResolvedField{Type: field.Type.Name}}
			class.fields[field.Name] = info
			if field.Static {
				info.storage.Global = fmt.Sprintf("%s_field_%d", prefix, i)
				prog.ClassGlobals = append(prog.ClassGlobals, &ast.GlobalVar{Name: info.storage.Global, Type: field.Type, Pos: field.Pos})
			} else {
				info.storage.Slot = class.slots
				class.slots++
			}
		}
		methods := decl.Methods
		if !hasConstructor(methods) {
			pos := decl.Pos
			methods = append(append([]*ast.ClassMethod(nil), methods...), &ast.ClassMethod{
				Public:      true,
				Constructor: true,
				Fn:          &ast.FnDecl{Name: "init", SourceFile: decl.SourceFile, Pos: pos, ReturnType: &ast.TypeExpr{Name: "void", Pos: pos}, Body: &ast.BlockStmt{Pos: pos}},
			})
		}
		for i, method := range methods {
			fn := method.Fn
			if !method.Constructor && (fn.Name == "init" || fn.Name == "self" || class.fields[fn.Name] != nil) {
				a.classError(fn.Pos, fmt.Sprintf("invalid or duplicate member %q in class %s", fn.Name, decl.Name))
				continue
			}
			params := make([]*Type, len(fn.Params))
			defaults := make([]ast.Expr, len(fn.Params))
			defaultCount := 0
			for j, param := range fn.Params {
				params[j] = a.resolveType(param.Type)
				defaults[j] = param.Default
				if param.Default != nil {
					defaultCount++
				} else if defaultCount != 0 {
					a.classError(param.Pos, "required parameter follows a default parameter")
				}
			}
			sym := &Symbol{Name: fn.Name, MangledName: fmt.Sprintf("%s_method_%d", prefix, i), Kind: SymFunc,
				Params: params, Defaults: defaults, DefaultCount: defaultCount, ReturnType: a.resolveType(fn.ReturnType)}
			info := &methodInfo{decl: method, sym: sym}
			overloads := class.methods[fn.Name]
			if method.Constructor {
				overloads = class.constructor
			}
			for _, other := range overloads {
				if other.decl.Static != method.Static || sameParams(other.sym.Params, params) {
					a.classError(fn.Pos, fmt.Sprintf("conflicting declarations of %s.%s", decl.Name, fn.Name))
					break
				}
			}
			if method.Constructor {
				class.constructor = append(overloads, info)
			} else {
				class.methods[fn.Name] = append(overloads, info)
			}
			a.classMethods = append(a.classMethods, classMethodBody{class, info})
		}
	}
}

type classMethodBody struct {
	class  *classInfo
	method *methodInfo
}

func sameParams(left, right []*Type) bool {
	if len(left) != len(right) {
		return false
	}
	for i, param := range left {
		if typeKey(param, "") != typeKey(right[i], "") {
			return false
		}
	}
	return true
}

func hasConstructor(methods []*ast.ClassMethod) bool {
	for _, method := range methods {
		if method.Constructor {
			return true
		}
	}
	return false
}

func (a *Analyzer) analyzeClassBodies(prog *ast.Program) {
	for _, decl := range prog.Classes {
		class := a.classDecls[decl]
		a.sourceFile, a.currentClass = decl.SourceFile, class
		var instanceInits []ast.Stmt
		for _, field := range decl.Fields {
			info := class.fields[field.Name]
			if info == nil || info.decl != field {
				continue
			}
			value := field.Value
			if value == nil {
				switch {
				case info.typ == TypeStr:
					value = &ast.StringLitExpr{Value: `""`, Pos: field.Pos}
				case info.typ != nil && info.typ.IsArray:
					value = &ast.ArrayLitExpr{Pos: field.Pos}
				default:
					continue // instance storage and static storage start at zero
				}
			}
			a.pushScope()
			var object ast.Expr = &ast.IdentExpr{Name: decl.Name, Pos: field.Pos}
			if !field.Static {
				a.scope.define(&Symbol{Name: "self", Kind: SymVar, Type: class.typ, Pos: field.Pos})
				object = &ast.IdentExpr{Name: "self", Pos: field.Pos}
			}
			stmt := &ast.AssignStmt{Target: &ast.MemberExpr{Object: object, Field: field.Name, Pos: field.Pos}, Value: value, Pos: field.Pos}
			a.analyzeAssignStmt(stmt)
			a.popScope()
			if field.Static {
				prog.Startup = append(prog.Startup, stmt)
			} else {
				instanceInits = append(instanceInits, stmt)
			}
		}
		class.decl.Initializers = instanceInits
	}
	for _, body := range a.classMethods {
		class, method := body.class, body.method
		decl := method.decl
		a.sourceFile, a.currentClass = class.decl.SourceFile, class
		fn := *decl.Fn
		fn.Name = method.sym.MangledName
		fn.MangledName = method.sym.MangledName
		if !decl.Static {
			self := &ast.Param{Name: "self", Type: &ast.TypeExpr{Name: class.decl.Name, Pos: fn.Pos}, Pos: fn.Pos}
			fn.Params = append([]*ast.Param{self}, fn.Params...)
		}
		a.classInstance = !decl.Static
		a.analyzeFunction(decl.Fn)
		if decl.Constructor {
			block := *fn.Body
			block.Stmts = append(append([]ast.Stmt(nil), class.decl.Initializers...), block.Stmts...)
			fn.Body = &block
		}
		prog.ClassFunctions = append(prog.ClassFunctions, &fn)
	}
	a.currentClass, a.classInstance = nil, false
}

func qualifiedClassName(expr ast.Expr) string {
	switch e := expr.(type) {
	case *ast.IdentExpr:
		return e.Name
	case *ast.MemberExpr:
		if ident, ok := e.Object.(*ast.IdentExpr); ok {
			return ident.Name + "." + e.Field
		}
	}
	return ""
}

func (a *Analyzer) classReceiver(expr ast.Expr) (*classInfo, bool) {
	name := qualifiedClassName(expr)
	root, _, _ := strings.Cut(name, ".")
	if sym := a.scope.lookup(root); sym == nil || sym.Kind != SymVar {
		if class := a.classType(name); class != nil {
			return class, true
		}
	}
	if typ := a.analyzeExpr(expr); typ != nil {
		return typ.class, false
	}
	return nil, false
}

func (a *Analyzer) analyzeClassField(e *ast.MemberExpr, class *classInfo, static bool) *Type {
	field := class.fields[e.Field]
	if field == nil {
		a.classError(e.Pos, fmt.Sprintf("class %s has no field %q", class.decl.Name, e.Field))
		return nil
	}
	if field.decl.Static != static {
		a.classError(e.Pos, fmt.Sprintf("field %s.%s requires %s access", class.decl.Name, e.Field, memberAccess(field.decl.Static)))
		return nil
	}
	if !field.decl.Public && a.currentClass != class {
		a.classError(e.Pos, fmt.Sprintf("field %s.%s is private", class.decl.Name, e.Field))
		return nil
	}
	e.ResolvedField = &field.storage
	return field.typ
}

func memberAccess(static bool) string {
	if static {
		return "class"
	}
	return "instance"
}

func (a *Analyzer) analyzeClassCall(e *ast.CallExpr, callee *ast.MemberExpr, class *classInfo, static bool) *Type {
	methods := class.methods[callee.Field]
	name := class.decl.Name + "." + callee.Field
	if len(methods) == 0 {
		a.classError(e.Pos, fmt.Sprintf("class %s has no method %q", class.decl.Name, callee.Field))
		return nil
	}
	if methods[0].decl.Static != static {
		a.classError(e.Pos, fmt.Sprintf("method %s requires %s access", name, memberAccess(methods[0].decl.Static)))
		return nil
	}
	method, args := a.resolveClassCall(e.Pos, name, e.Args, methods, class)
	if method == nil {
		return nil
	}
	e.Args, e.ResolvedCallee = args, method.sym.MangledName
	if !static {
		e.Receiver = callee.Object
	}
	return method.sym.ReturnType
}

func (a *Analyzer) analyzeNewExpr(e *ast.NewExpr) *Type {
	class := a.classType(e.ClassName)
	if class == nil {
		a.classError(e.Pos, fmt.Sprintf("unknown class %q", e.ClassName))
		return nil
	}
	method, args := a.resolveClassCall(e.Pos, class.decl.Name+".init", e.Args, class.constructor, class)
	if method != nil {
		e.Args, e.Constructor, e.Slots = args, method.sym.MangledName, class.slots
	}
	return class.typ
}

func (a *Analyzer) resolveClassCall(pos ast.Position, name string, args []ast.Expr, methods []*methodInfo, class *classInfo) (*methodInfo, []ast.Expr) {
	argTypes := make([]*Type, len(args))
	for i, arg := range args {
		argTypes[i] = a.analyzeExpr(arg)
	}
	var match *methodInfo
	private := false
	for _, method := range methods {
		sym := method.sym
		if len(args) < len(sym.Params)-sym.DefaultCount || len(args) > len(sym.Params) {
			continue
		}
		ok := true
		for i, argType := range argTypes {
			if argType != nil && sym.Params[i] != nil && !isAssignableTo(sym.Params[i], argType) {
				ok = false
				break
			}
		}
		if !ok {
			continue
		}
		if !method.decl.Public && a.currentClass != class {
			private = true
			continue
		}
		if match != nil {
			a.classError(pos, fmt.Sprintf("ambiguous overload for %s", name))
			return nil, args
		}
		match = method
	}
	if match == nil {
		if private {
			a.classError(pos, fmt.Sprintf("%s is private", name))
		} else {
			a.classError(pos, fmt.Sprintf("no matching overload for %s", name))
		}
		return nil, args
	}
	for _, def := range match.sym.Defaults[len(args):] {
		args = append(args, def)
	}
	return match, args
}

func (a *Analyzer) checkClassConflict(alias, name string, pos ast.Position) {
	if alias != "" {
		name = alias + "." + name
	}
	if a.classType(name) != nil {
		a.classError(pos, fmt.Sprintf("declaration %q conflicts with a class name", name))
	}
}

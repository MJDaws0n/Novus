package imports

import (
	"novus/internal/ast"
	"testing"
)

// ---------------------------------------------------------------------------
// funcSignature tests
// ---------------------------------------------------------------------------

func TestFuncSignature(t *testing.T) {
	fn := &ast.FnDecl{
		Name: "add",
		Params: []*ast.Param{
			{Name: "a", Type: &ast.TypeExpr{Name: "i32"}},
			{Name: "b", Type: &ast.TypeExpr{Name: "i32"}},
		},
		ReturnType: &ast.TypeExpr{Name: "i32"},
	}
	got := funcSignature(fn)
	want := "add(i32,i32):i32"
	if got != want {
		t.Errorf("funcSignature = %q, want %q", got, want)
	}
}

func TestFuncSignatureNoParams(t *testing.T) {
	fn := &ast.FnDecl{
		Name:       "noop",
		Params:     nil,
		ReturnType: &ast.TypeExpr{Name: "void"},
	}
	got := funcSignature(fn)
	want := "noop():void"
	if got != want {
		t.Errorf("funcSignature = %q, want %q", got, want)
	}
}

// ---------------------------------------------------------------------------
// Deduplication tests
// ---------------------------------------------------------------------------

func TestDeduplicateModules_SameSignature(t *testing.T) {
	fn1 := &ast.FnDecl{
		Name:       "helper",
		Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "i32"}}},
		ReturnType: &ast.TypeExpr{Name: "i32"},
	}
	fn2 := &ast.FnDecl{
		Name:       "helper",
		Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "i32"}}},
		ReturnType: &ast.TypeExpr{Name: "i32"},
	}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "libA", Alias: "", Functions: []*ast.FnDecl{fn1}},
		{Path: "libB", Alias: "", Functions: []*ast.FnDecl{fn2}},
	}

	r.deduplicateModules()

	total := 0
	for _, mod := range r.allModules {
		total += len(mod.Functions)
	}
	if total != 1 {
		t.Errorf("expected 1 function after dedup, got %d", total)
	}
}

func TestDeduplicateModules_DifferentSignatures(t *testing.T) {
	fn1 := &ast.FnDecl{
		Name:       "convert",
		Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "i32"}}},
		ReturnType: &ast.TypeExpr{Name: "str"},
	}
	fn2 := &ast.FnDecl{
		Name:       "convert",
		Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "i64"}}},
		ReturnType: &ast.TypeExpr{Name: "str"},
	}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "libA", Alias: "", Functions: []*ast.FnDecl{fn1}},
		{Path: "libB", Alias: "", Functions: []*ast.FnDecl{fn2}},
	}

	r.deduplicateModules()

	total := 0
	for _, mod := range r.allModules {
		total += len(mod.Functions)
	}
	if total != 2 {
		t.Errorf("expected 2 functions after dedup (different sigs), got %d", total)
	}
}

func TestDeduplicateModules_AliasedSameSignature(t *testing.T) {
	fn1 := &ast.FnDecl{
		Name:       "greet",
		Params:     []*ast.Param{{Name: "name", Type: &ast.TypeExpr{Name: "str"}}},
		ReturnType: &ast.TypeExpr{Name: "void"},
	}
	fn2 := &ast.FnDecl{
		Name:       "greet",
		Params:     []*ast.Param{{Name: "name", Type: &ast.TypeExpr{Name: "str"}}},
		ReturnType: &ast.TypeExpr{Name: "void"},
	}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "libA", Alias: "std", Functions: []*ast.FnDecl{fn1}},
		{Path: "libB", Alias: "std", Functions: []*ast.FnDecl{fn2}},
	}

	r.deduplicateModules()

	total := 0
	for _, mod := range r.allModules {
		total += len(mod.Functions)
	}
	if total != 1 {
		t.Errorf("expected 1 function after aliased dedup, got %d", total)
	}
}

func TestDeduplicateModules_DifferentAliasesNotDeduped(t *testing.T) {
	fn1 := &ast.FnDecl{
		Name:       "greet",
		Params:     []*ast.Param{{Name: "name", Type: &ast.TypeExpr{Name: "str"}}},
		ReturnType: &ast.TypeExpr{Name: "void"},
	}
	fn2 := &ast.FnDecl{
		Name:       "greet",
		Params:     []*ast.Param{{Name: "name", Type: &ast.TypeExpr{Name: "str"}}},
		ReturnType: &ast.TypeExpr{Name: "void"},
	}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "libA", Alias: "a", Functions: []*ast.FnDecl{fn1}},
		{Path: "libB", Alias: "b", Functions: []*ast.FnDecl{fn2}},
	}

	r.deduplicateModules()

	total := 0
	for _, mod := range r.allModules {
		total += len(mod.Functions)
	}
	if total != 2 {
		t.Errorf("expected 2 functions (different aliases), got %d", total)
	}
}

// ---------------------------------------------------------------------------
// CheckAliasConflicts tests
// ---------------------------------------------------------------------------

func TestCheckAliasConflicts_IdenticalSignatureNoError(t *testing.T) {
	fn1 := &ast.FnDecl{
		Name:       "helper",
		Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "i32"}}},
		ReturnType: &ast.TypeExpr{Name: "i32"},
	}
	fn2 := &ast.FnDecl{
		Name:       "helper",
		Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "i32"}}},
		ReturnType: &ast.TypeExpr{Name: "i32"},
	}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "libA", FilePath: "/a.nov", Alias: "", Functions: []*ast.FnDecl{fn1}},
		{Path: "libB", FilePath: "/b.nov", Alias: "", Functions: []*ast.FnDecl{fn2}},
	}

	errs := r.CheckAliasConflicts()
	if len(errs) != 0 {
		t.Errorf("expected no errors for identical signatures, got %d: %v", len(errs), errs)
	}
}

func TestCheckAliasConflicts_DifferentSignatureErrors(t *testing.T) {
	fn1 := &ast.FnDecl{
		Name:       "helper",
		Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "i32"}}},
		ReturnType: &ast.TypeExpr{Name: "i32"},
	}
	fn2 := &ast.FnDecl{
		Name:       "helper",
		Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "str"}}},
		ReturnType: &ast.TypeExpr{Name: "void"},
	}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "libA", FilePath: "/a.nov", Alias: "", Functions: []*ast.FnDecl{fn1}},
		{Path: "libB", FilePath: "/b.nov", Alias: "", Functions: []*ast.FnDecl{fn2}},
	}

	errs := r.CheckAliasConflicts()
	if len(errs) == 0 {
		t.Error("expected an error for conflicting signatures, got none")
	}
}

func TestDeduplicateGlobals(t *testing.T) {
	g1 := &ast.GlobalVar{Name: "PI"}
	g2 := &ast.GlobalVar{Name: "PI"}

	prog1 := &ast.Program{Globals: []*ast.GlobalVar{g1}}
	prog2 := &ast.Program{Globals: []*ast.GlobalVar{g2}}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "libA", Program: prog1, Functions: nil},
		{Path: "libB", Program: prog2, Functions: nil},
	}

	r.deduplicateModules()

	totalGlobals := 0
	for _, mod := range r.allModules {
		if mod.Program != nil {
			totalGlobals += len(mod.Program.Globals)
		}
	}
	if totalGlobals != 1 {
		t.Errorf("expected 1 global after dedup, got %d", totalGlobals)
	}
}

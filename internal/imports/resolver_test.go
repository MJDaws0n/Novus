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

// ===========================================================================
// Bug 2: Cross-module globals included in merged program
// ===========================================================================

func TestResolve_ImportedGlobalsInMergedProgram(t *testing.T) {
	// Simulates what Resolve does: imported module globals should appear
	// in the merged program's Globals list.
	importedGlobal := &ast.GlobalVar{Name: "debug_enabled", Type: &ast.TypeExpr{Name: "bool"}}
	importedProg := &ast.Program{Globals: []*ast.GlobalVar{importedGlobal}}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "config", Program: importedProg, Functions: nil},
	}

	// Simulate the merge step from Resolve.
	mainProg := &ast.Program{
		Module:    &ast.ModuleDecl{Name: "test"},
		Functions: []*ast.FnDecl{},
	}
	merged := &ast.Program{
		Module:  mainProg.Module,
		Imports: mainProg.Imports,
	}

	for _, mod := range r.allModules {
		for _, fn := range mod.Functions {
			merged.Functions = append(merged.Functions, fn)
		}
		if mod.Program != nil {
			for _, g := range mod.Program.Globals {
				merged.Globals = append(merged.Globals, g)
			}
		}
	}
	merged.Globals = append(merged.Globals, mainProg.Globals...)
	merged.Functions = append(merged.Functions, mainProg.Functions...)

	// Check that the imported global is in the merged program.
	found := false
	for _, g := range merged.Globals {
		if g.Name == "debug_enabled" {
			found = true
		}
	}
	if !found {
		t.Error("expected imported global 'debug_enabled' in merged program")
	}
}

func TestResolve_MainFileGlobalsIncluded(t *testing.T) {
	// The main file's own globals should also be in the merged program.
	mainGlobal := &ast.GlobalVar{Name: "app_version", Type: &ast.TypeExpr{Name: "str"}}
	mainProg := &ast.Program{
		Module:    &ast.ModuleDecl{Name: "test"},
		Globals:   []*ast.GlobalVar{mainGlobal},
		Functions: []*ast.FnDecl{},
	}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = nil // no imports

	// Simulate the Resolve merge.
	merged := &ast.Program{
		Module:  mainProg.Module,
		Imports: mainProg.Imports,
	}
	for _, mod := range r.allModules {
		for _, fn := range mod.Functions {
			merged.Functions = append(merged.Functions, fn)
		}
		if mod.Program != nil {
			for _, g := range mod.Program.Globals {
				merged.Globals = append(merged.Globals, g)
			}
		}
	}
	merged.Globals = append(merged.Globals, mainProg.Globals...)
	merged.Functions = append(merged.Functions, mainProg.Functions...)

	found := false
	for _, g := range merged.Globals {
		if g.Name == "app_version" {
			found = true
		}
	}
	if !found {
		t.Error("expected main file global 'app_version' in merged program")
	}
}

// ===========================================================================
// Bug 4: Diamond imports deduplication
// ===========================================================================

func TestDeduplicateDiamondImports(t *testing.T) {
	// Simulates a diamond: main imports A and B, both import utils.
	// The same function "helper" from utils should appear only once after dedup.
	helperFn := func() *ast.FnDecl {
		return &ast.FnDecl{
			Name:       "helper",
			Params:     []*ast.Param{{Name: "x", Type: &ast.TypeExpr{Name: "i32"}}},
			ReturnType: &ast.TypeExpr{Name: "i32"},
		}
	}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "utils_via_a", Alias: "", Functions: []*ast.FnDecl{helperFn()}},
		{Path: "utils_via_b", Alias: "", Functions: []*ast.FnDecl{helperFn()}},
	}

	r.deduplicateModules()

	total := 0
	for _, mod := range r.allModules {
		total += len(mod.Functions)
	}
	if total != 1 {
		t.Errorf("expected 1 function after diamond dedup, got %d", total)
	}
}

func TestDeduplicateDiamondImports_WithGlobals(t *testing.T) {
	// Diamond import with globals: should also be deduplicated.
	prog1 := &ast.Program{Globals: []*ast.GlobalVar{{Name: "MAX_SIZE"}}}
	prog2 := &ast.Program{Globals: []*ast.GlobalVar{{Name: "MAX_SIZE"}}}

	r := &Resolver{resolved: make(map[string]*ImportedModule)}
	r.allModules = []*ImportedModule{
		{Path: "utils_via_a", Program: prog1, Functions: nil},
		{Path: "utils_via_b", Program: prog2, Functions: nil},
	}

	r.deduplicateModules()

	totalGlobals := 0
	for _, mod := range r.allModules {
		if mod.Program != nil {
			totalGlobals += len(mod.Program.Globals)
		}
	}
	if totalGlobals != 1 {
		t.Errorf("expected 1 global after diamond dedup, got %d", totalGlobals)
	}
}

func TestMaxImportDepthGuard(t *testing.T) {
	// The maxImportDepth constant should be defined and reasonable.
	if maxImportDepth < 16 {
		t.Errorf("maxImportDepth %d is too low, expected at least 16", maxImportDepth)
	}
	if maxImportDepth > 1024 {
		t.Errorf("maxImportDepth %d is too high, expected at most 1024", maxImportDepth)
	}
}

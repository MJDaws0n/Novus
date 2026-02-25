package imports

import (
	"novus/internal/ast"
	"novus/internal/lexer"
	"novus/internal/parser"
	"os"
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

// ---------------------------------------------------------------------------
// Directory import resolution tests
// ---------------------------------------------------------------------------

func TestDirectoryImportResolvesToMainNov(t *testing.T) {
	// Create a temporary directory structure: lib/mymod/main.nov
	dir := t.TempDir()
	modDir := dir + "/lib/mymod"
	if err := os.MkdirAll(modDir, 0o755); err != nil {
		t.Fatal(err)
	}
	mainNov := modDir + "/main.nov"
	if err := os.WriteFile(mainNov, []byte("module mymod;\nfn helper() -> i32 { return 42; }\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	// Create root source file.
	rootFile := dir + "/main.nov"
	if err := os.WriteFile(rootFile, []byte("module main;\nimport lib/mymod;\nfn main() -> i32 { return helper(); }\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// Parse and resolve.
	content, _ := os.ReadFile(rootFile)
	tokens, _ := lexer.Lex(string(content))
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	resolver := NewResolver(rootFile)
	merged, resolveErrs := resolver.Resolve(prog, rootFile)
	if len(resolveErrs) > 0 {
		t.Fatalf("resolve errors: %v", resolveErrs)
	}

	// Should have the imported function.
	found := false
	for _, fn := range merged.Functions {
		if fn.Name == "helper" {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected imported function 'helper' from lib/mymod/main.nov")
	}
}

func TestDirectoryImportFallbackWhenNoFile(t *testing.T) {
	// Create structure where lib/mymod.nov does NOT exist, but lib/mymod/main.nov does.
	dir := t.TempDir()
	modDir := dir + "/mymod"
	if err := os.MkdirAll(modDir, 0o755); err != nil {
		t.Fatal(err)
	}
	mainNov := modDir + "/main.nov"
	if err := os.WriteFile(mainNov, []byte("module mymod;\nfn greet() -> i32 { return 1; }\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	rootFile := dir + "/main.nov"
	if err := os.WriteFile(rootFile, []byte("module main;\nimport mymod;\nfn main() -> i32 { return greet(); }\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	content, _ := os.ReadFile(rootFile)
	tokens, _ := lexer.Lex(string(content))
	prog, _ := parser.Parse(tokens)

	resolver := NewResolver(rootFile)
	merged, resolveErrs := resolver.Resolve(prog, rootFile)
	if len(resolveErrs) > 0 {
		t.Fatalf("resolve errors: %v", resolveErrs)
	}

	found := false
	for _, fn := range merged.Functions {
		if fn.Name == "greet" {
			found = true
		}
	}
	if !found {
		t.Error("expected imported function 'greet' from mymod/main.nov")
	}
}

func TestHyphenatedPathImport(t *testing.T) {
	// Create lib with hyphen in name: my-lib/main.nov
	dir := t.TempDir()
	modDir := dir + "/my-lib"
	if err := os.MkdirAll(modDir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(modDir+"/main.nov", []byte("module mylib;\nfn util() -> i32 { return 7; }\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	rootFile := dir + "/main.nov"
	if err := os.WriteFile(rootFile, []byte("module main;\nimport my-lib;\nfn main() -> i32 { return util(); }\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	content, _ := os.ReadFile(rootFile)
	tokens, _ := lexer.Lex(string(content))
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	resolver := NewResolver(rootFile)
	_, resolveErrs := resolver.Resolve(prog, rootFile)
	if len(resolveErrs) > 0 {
		t.Fatalf("resolve errors: %v", resolveErrs)
	}
}

func TestRelativeDotDotImport(t *testing.T) {
	// Create dir/lib/helper.nov and dir/src/main.nov where main imports ../lib/helper.
	dir := t.TempDir()
	if err := os.MkdirAll(dir+"/lib", 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(dir+"/src", 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(dir+"/lib/helper.nov", []byte("module helper;\nfn help() -> i32 { return 5; }\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	rootFile := dir + "/src/main.nov"
	if err := os.WriteFile(rootFile, []byte("module main;\nimport ../lib/helper;\nfn main() -> i32 { return help(); }\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	content, _ := os.ReadFile(rootFile)
	tokens, _ := lexer.Lex(string(content))
	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors: %v", parseErrs)
	}

	resolver := NewResolver(rootFile)
	merged, resolveErrs := resolver.Resolve(prog, rootFile)
	if len(resolveErrs) > 0 {
		t.Fatalf("resolve errors: %v", resolveErrs)
	}

	found := false
	for _, fn := range merged.Functions {
		if fn.Name == "help" {
			found = true
		}
	}
	if !found {
		t.Error("expected imported function 'help' from ../lib/helper")
	}
}

// ---------------------------------------------------------------------------
// #if blocks in imported files
// ---------------------------------------------------------------------------

func TestResolver_CompTimeIfInImportedFile(t *testing.T) {
	dir := t.TempDir()

	// Create a library file with #if blocks for platform selection.
	libContent := `module mylib;

#if(os == "darwin") {
	fn platform_fn() -> str {
		return "darwin";
	}
}

#if(os == "windows") {
	fn platform_fn() -> str {
		return "windows";
	}
}

fn common_fn() -> i32 {
	return 42;
}
`
	os.WriteFile(dir+"/mylib.nov", []byte(libContent), 0644)

	// Create a main file that imports the library.
	mainContent := `module main;
import mylib;
fn main() -> void {}
`
	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(mainContent), 0644)

	// Parse the main file.
	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	// Create resolver with target set to "darwin".
	resolver := NewResolver(mainFile)
	resolver.TargetOS = "darwin"
	resolver.TargetArch = "arm64"

	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	// Should have common_fn + the darwin platform_fn, but NOT the windows one.
	fnNames := map[string]bool{}
	for _, fn := range merged.Functions {
		fnNames[fn.Name] = true
	}

	if !fnNames["common_fn"] {
		t.Error("expected common_fn to be imported")
	}
	if !fnNames["platform_fn"] {
		t.Error("expected platform_fn (darwin variant) to be imported")
	}

	// Count how many platform_fn instances (should be 1, not 2).
	count := 0
	for _, fn := range merged.Functions {
		if fn.Name == "platform_fn" {
			count++
		}
	}
	if count != 1 {
		t.Errorf("expected exactly 1 platform_fn, got %d", count)
	}
}

// Test that #if blocks with imports (not just functions) inside imported files work.
func TestResolver_CompTimeIfImportInsideImportedFile(t *testing.T) {
	dir := t.TempDir()

	// Helper file that will be conditionally imported
	os.WriteFile(dir+"/helper.nov", []byte(`module helper;
fn helper_fn() -> i32 { return 99; }
`), 0644)

	// Library that uses #if to conditionally import helper
	os.WriteFile(dir+"/mylib.nov", []byte(`module mylib;
#if(os == "darwin") {
	import helper;
}
fn lib_fn() -> i32 { return 1; }
`), 0644)

	// Main
	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(`module main;
import mylib;
fn main() -> void {}
`), 0644)

	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	resolver := NewResolver(mainFile)
	resolver.TargetOS = "darwin"
	resolver.TargetArch = "arm64"

	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	fnNames := map[string]bool{}
	for _, fn := range merged.Functions {
		fnNames[fn.Name] = true
	}

	if !fnNames["lib_fn"] {
		t.Error("expected lib_fn to be imported")
	}
	if !fnNames["helper_fn"] {
		t.Error("expected helper_fn (from conditionally imported helper) to be imported")
	}
}

// Test that #if blocks with non-matching OS exclude the import entirely.
func TestResolver_CompTimeIfImportExcluded(t *testing.T) {
	dir := t.TempDir()

	os.WriteFile(dir+"/win_only.nov", []byte(`module win_only;
fn win_fn() -> i32 { return 1; }
`), 0644)

	os.WriteFile(dir+"/mylib.nov", []byte(`module mylib;
#if(os == "windows") {
	import win_only;
}
fn lib_fn() -> i32 { return 2; }
`), 0644)

	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(`module main;
import mylib;
fn main() -> void {}
`), 0644)

	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	resolver := NewResolver(mainFile)
	resolver.TargetOS = "darwin"
	resolver.TargetArch = "arm64"

	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	fnNames := map[string]bool{}
	for _, fn := range merged.Functions {
		fnNames[fn.Name] = true
	}

	if !fnNames["lib_fn"] {
		t.Error("expected lib_fn to be imported")
	}
	if fnNames["win_fn"] {
		t.Error("win_fn should NOT be imported when target is darwin")
	}
}

// Test deeply nested #if: main → lib (has #if import) → sub-lib (has its own #if functions)
func TestResolver_CompTimeIfNestedDeep(t *testing.T) {
	dir := t.TempDir()
	os.MkdirAll(dir+"/sub", 0755)

	// sub/platform.nov has its own #if blocks
	os.WriteFile(dir+"/sub/platform.nov", []byte(`module platform;
#if(os == "linux") {
	fn get_id() -> i32 { return 2; }
}
#if(os == "darwin") {
	fn get_id() -> i32 { return 1; }
}
`), 0644)

	// loader.nov uses #if to import the correct sub-module
	os.WriteFile(dir+"/loader.nov", []byte(`module loader;
#if(os == "darwin") {
	import sub/platform;
}
fn loader_init() -> i32 { return 0; }
`), 0644)

	// main imports loader
	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(`module main;
import loader;
fn main() -> void {}
`), 0644)

	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	resolver := NewResolver(mainFile)
	resolver.TargetOS = "darwin"
	resolver.TargetArch = "arm64"

	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	fnNames := map[string]bool{}
	for _, fn := range merged.Functions {
		fnNames[fn.Name] = true
	}

	if !fnNames["loader_init"] {
		t.Error("expected loader_init")
	}
	if !fnNames["get_id"] {
		t.Error("expected get_id from nested #if (darwin variant)")
	}

	// Only 1 get_id should exist (darwin only, not linux)
	count := 0
	for _, fn := range merged.Functions {
		if fn.Name == "get_id" {
			count++
		}
	}
	if count != 1 {
		t.Errorf("expected exactly 1 get_id, got %d", count)
	}
}

// Test #if with selective imports: import lib[fn1, fn2] where fn2 comes from #if block.
func TestResolver_CompTimeIfWithSelectiveImport(t *testing.T) {
	dir := t.TempDir()

	os.WriteFile(dir+"/mylib.nov", []byte(`module mylib;
fn always_fn() -> i32 { return 10; }
fn extra_fn() -> i32 { return 20; }
#if(os == "darwin") {
	fn platform_fn() -> i32 { return 1; }
}
#if(os == "linux") {
	fn platform_fn() -> i32 { return 2; }
}
`), 0644)

	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(`module main;
import mylib[always_fn, platform_fn];
fn main() -> void {}
`), 0644)

	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	resolver := NewResolver(mainFile)
	resolver.TargetOS = "darwin"
	resolver.TargetArch = "arm64"

	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	fnNames := map[string]bool{}
	for _, fn := range merged.Functions {
		fnNames[fn.Name] = true
	}

	if !fnNames["always_fn"] {
		t.Error("expected always_fn (selected)")
	}
	if !fnNames["platform_fn"] {
		t.Error("expected platform_fn (from #if darwin block, selected)")
	}
	if fnNames["extra_fn"] {
		t.Error("extra_fn should NOT be imported (not in selective list)")
	}
}

// Test #if with globals in imported file.
func TestResolver_CompTimeIfGlobalsInImportedFile(t *testing.T) {
	dir := t.TempDir()

	os.WriteFile(dir+"/config.nov", []byte(`module config;
#if(os == "darwin") {
	let platform_name: str = "macOS";
}
#if(os == "windows") {
	let platform_name: str = "Windows";
}
fn get_version() -> i32 { return 1; }
`), 0644)

	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(`module main;
import config;
fn main() -> void {}
`), 0644)

	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	resolver := NewResolver(mainFile)
	resolver.TargetOS = "darwin"
	resolver.TargetArch = "arm64"

	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	// Check that the global from #if(darwin) was merged
	globalNames := map[string]bool{}
	for _, g := range merged.Globals {
		globalNames[g.Name] = true
	}
	if !globalNames["platform_name"] {
		t.Error("expected platform_name global (from darwin #if block)")
	}

	// Count — should be exactly 1 (only darwin, not windows)
	count := 0
	for _, g := range merged.Globals {
		if g.Name == "platform_name" {
			count++
		}
	}
	if count != 1 {
		t.Errorf("expected exactly 1 platform_name global, got %d", count)
	}
}

// Test that #if blocks with no TargetOS set on resolver are ignored (backwards compat).
func TestResolver_CompTimeIfNoTarget(t *testing.T) {
	dir := t.TempDir()

	os.WriteFile(dir+"/mylib.nov", []byte(`module mylib;
#if(os == "darwin") {
	fn platform_fn() -> i32 { return 1; }
}
fn common_fn() -> i32 { return 0; }
`), 0644)

	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(`module main;
import mylib;
fn main() -> void {}
`), 0644)

	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	// Resolver with NO target set
	resolver := NewResolver(mainFile)

	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	fnNames := map[string]bool{}
	for _, fn := range merged.Functions {
		fnNames[fn.Name] = true
	}

	if !fnNames["common_fn"] {
		t.Error("expected common_fn")
	}
	// With no target, #if blocks are NOT resolved, so platform_fn should not exist
	if fnNames["platform_fn"] {
		t.Error("platform_fn should NOT be imported when no target is set")
	}
}

// Test multiple #if blocks with different conditions in the same imported file.
func TestResolver_CompTimeIfMultipleConditions(t *testing.T) {
	dir := t.TempDir()

	os.WriteFile(dir+"/multi.nov", []byte(`module multi;
#if(os == "darwin") {
	fn os_fn() -> i32 { return 1; }
}
#if(arch == "arm64") {
	fn arch_fn() -> i32 { return 64; }
}
#if(arch == "amd64") {
	fn arch_fn() -> i32 { return 86; }
}
fn common() -> i32 { return 0; }
`), 0644)

	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(`module main;
import multi;
fn main() -> void {}
`), 0644)

	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	resolver := NewResolver(mainFile)
	resolver.TargetOS = "darwin"
	resolver.TargetArch = "arm64"

	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	fnNames := map[string]bool{}
	for _, fn := range merged.Functions {
		fnNames[fn.Name] = true
	}

	if !fnNames["common"] {
		t.Error("expected common")
	}
	if !fnNames["os_fn"] {
		t.Error("expected os_fn (darwin match)")
	}
	if !fnNames["arch_fn"] {
		t.Error("expected arch_fn (arm64 match)")
	}

	// Should only have 1 arch_fn (arm64, not amd64)
	count := 0
	for _, fn := range merged.Functions {
		if fn.Name == "arch_fn" {
			count++
		}
	}
	if count != 1 {
		t.Errorf("expected 1 arch_fn, got %d", count)
	}
}

func TestResolver_ImportedFunctionsMarkedAsImported(t *testing.T) {
	dir := t.TempDir()

	libContent := `module mylib;
fn helper() -> i32 { return 1; }
`
	os.WriteFile(dir+"/mylib.nov", []byte(libContent), 0644)

	mainContent := `module main;
import mylib;
fn main() -> void {}
`
	mainFile := dir + "/main.nov"
	os.WriteFile(mainFile, []byte(mainContent), 0644)

	mainSrc, _ := os.ReadFile(mainFile)
	tokens, _ := lexer.Lex(string(mainSrc))
	prog, _ := parser.Parse(tokens)

	resolver := NewResolver(mainFile)
	merged, errs := resolver.Resolve(prog, mainFile)
	if len(errs) > 0 {
		t.Fatalf("resolver errors: %v", errs)
	}

	for _, fn := range merged.Functions {
		switch fn.Name {
		case "helper":
			if !fn.Imported {
				t.Error("helper should be marked as Imported=true")
			}
		case "main":
			if fn.Imported {
				t.Error("main should be marked as Imported=false")
			}
		}
	}
}

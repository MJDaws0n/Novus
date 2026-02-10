package imports

import (
	"fmt"
	"novus/internal/ast"
	"novus/internal/lexer"
	"novus/internal/parser"
	"os"
	"path/filepath"
	"strings"
)

// ---------------------------------------------------------------------------
// ResolveError represents an error during import resolution.
// ---------------------------------------------------------------------------

type ResolveError struct {
	Message string
	Pos     ast.Position
	File    string
}

func (e *ResolveError) Error() string {
	if e.File != "" {
		return fmt.Sprintf("%s: line %d, col %d: %s", e.File, e.Pos.Line, e.Pos.Column, e.Message)
	}
	return fmt.Sprintf("line %d, col %d: %s", e.Pos.Line, e.Pos.Column, e.Message)
}

// ---------------------------------------------------------------------------
// ImportedModule holds the parsed result of an imported file.
// ---------------------------------------------------------------------------

type ImportedModule struct {
	Path      string        // original import path (e.g. "standard_lib")
	FilePath  string        // resolved absolute file path
	Alias     string        // alias for namespaced access ("" = direct access)
	SelectFns []string      // selective function imports (nil = import all)
	Program   *ast.Program  // parsed AST of the imported file
	Functions []*ast.FnDecl // functions available after filtering
}

// ---------------------------------------------------------------------------
// Resolver resolves all imports for a program, handling:
//   - File resolution (relative to the importing file's directory)
//   - Circular import detection
//   - No duplicate file imports
//   - Transitive imports (imported files can import other files)
//   - Selective imports
//   - Alias merging (same alias from different imports OK if no overlap)
// ---------------------------------------------------------------------------

type Resolver struct {
	// BaseDir is the directory of the root source file.
	BaseDir string

	// resolved tracks which absolute file paths have already been imported
	// to prevent duplicate imports of the same file.
	resolved map[string]*ImportedModule

	// importStack tracks the current chain of imports for circular detection.
	importStack []string

	// errors collects all resolution errors.
	errors []*ResolveError

	// allModules is the ordered list of all resolved modules (depth-first).
	allModules []*ImportedModule
}

// NewResolver creates a new import resolver rooted at the given source file path.
func NewResolver(sourceFilePath string) *Resolver {
	absPath, _ := filepath.Abs(sourceFilePath)
	return &Resolver{
		BaseDir:  filepath.Dir(absPath),
		resolved: make(map[string]*ImportedModule),
	}
}

// Resolve processes all imports in the given program and returns:
//   - A merged program with all imported functions prepended
//   - Any resolution errors
//
// The returned program has the same Module declaration and main functions,
// but imported functions are added before the original functions.
func (r *Resolver) Resolve(prog *ast.Program, sourceFile string) (*ast.Program, []*ResolveError) {
	absSource, _ := filepath.Abs(sourceFile)
	r.importStack = append(r.importStack, absSource)

	for _, imp := range prog.Imports {
		r.resolveImport(imp, r.BaseDir, absSource, "")
	}

	r.importStack = r.importStack[:len(r.importStack)-1]

	if len(r.errors) > 0 {
		return prog, r.errors
	}

	// Build the merged program: imported functions first, then original functions.
	merged := &ast.Program{
		Module:  prog.Module,
		Imports: prog.Imports,
		Pos:     prog.Pos,
	}

	// Add all imported functions (in import order, depth-first).
	for _, mod := range r.allModules {
		for _, fn := range mod.Functions {
			merged.Functions = append(merged.Functions, fn)
		}
	}

	// Add the original file's functions.
	merged.Functions = append(merged.Functions, prog.Functions...)

	return merged, nil
}

// resolveImport resolves a single import declaration.
// rootAlias is propagated from the top-level import so that transitive
// sub-imports inherit the caller's alias (e.g. import A standard; where A
// imports B — B's functions also land under the "standard" alias).
func (r *Resolver) resolveImport(imp *ast.ImportDecl, baseDir string, importerFile string, rootAlias string) {
	// Resolve the file path: import path maps to path.nov relative to baseDir.
	relPath := strings.ReplaceAll(imp.Path, "/", string(filepath.Separator))
	filePath := filepath.Join(baseDir, relPath+".nov")

	absPath, err := filepath.Abs(filePath)
	if err != nil {
		r.addError(imp.Pos, importerFile, fmt.Sprintf("cannot resolve import path %q: %v", imp.Path, err))
		return
	}

	// Check file exists.
	if _, err := os.Stat(absPath); os.IsNotExist(err) {
		r.addError(imp.Pos, importerFile, fmt.Sprintf("imported file not found: %s (resolved from %q)", absPath, imp.Path))
		return
	}

	// Check for circular imports.
	for _, stackPath := range r.importStack {
		if stackPath == absPath {
			chain := append(r.importStack, absPath)
			r.addError(imp.Pos, importerFile, fmt.Sprintf("circular import detected: %s", strings.Join(chain, " → ")))
			return
		}
	}

	// Check for duplicate file import.
	if existing, ok := r.resolved[absPath]; ok {
		// Same file already imported — that's OK, we just reuse the parsed module.
		// But we still create a new ImportedModule entry for this import's alias/selection.
		effectiveAlias := imp.Alias
		if effectiveAlias == "" && rootAlias != "" {
			effectiveAlias = rootAlias
		}
		mod := &ImportedModule{
			Path:      imp.Path,
			FilePath:  absPath,
			Alias:     effectiveAlias,
			SelectFns: imp.SelectFns,
			Program:   existing.Program,
		}
		mod.Functions = r.filterFunctions(existing.Program.Functions, imp, absPath)
		r.allModules = append(r.allModules, mod)
		return
	}

	// Read and parse the imported file.
	content, err := os.ReadFile(absPath)
	if err != nil {
		r.addError(imp.Pos, importerFile, fmt.Sprintf("cannot read imported file %s: %v", absPath, err))
		return
	}

	tokens, lexErrors := lexer.Lex(string(content))
	if len(lexErrors) > 0 {
		for _, e := range lexErrors {
			r.addError(imp.Pos, absPath, fmt.Sprintf("lex error in imported file: %s", e.Error()))
		}
		return
	}

	prog, parseErrors := parser.Parse(tokens)
	if len(parseErrors) > 0 {
		for _, e := range parseErrors {
			r.addError(imp.Pos, absPath, fmt.Sprintf("parse error in imported file: %s", e.Error()))
		}
		return
	}

	// Compute the effective alias: if this import has its own alias use it,
	// otherwise inherit the root alias from the parent import chain.
	effectiveAlias := imp.Alias
	if effectiveAlias == "" && rootAlias != "" {
		effectiveAlias = rootAlias
	}

	// Register this file as resolved before processing its own imports (for circular detection).
	mod := &ImportedModule{
		Path:      imp.Path,
		FilePath:  absPath,
		Alias:     effectiveAlias,
		SelectFns: imp.SelectFns,
		Program:   prog,
	}
	r.resolved[absPath] = mod

	// Process transitive imports (imports within the imported file).
	// Pass effectiveAlias so sub-imports inherit the alias from the root.
	importDir := filepath.Dir(absPath)
	r.importStack = append(r.importStack, absPath)
	for _, subImp := range prog.Imports {
		r.resolveImport(subImp, importDir, absPath, effectiveAlias)
	}
	r.importStack = r.importStack[:len(r.importStack)-1]

	// Filter functions based on selective imports.
	mod.Functions = r.filterFunctions(prog.Functions, imp, absPath)

	// Apply mangling for aliased imports: prefix function names with the alias
	// so that semantic analysis can resolve alias.func() calls.
	if effectiveAlias != "" {
		for _, fn := range mod.Functions {
			// Store the original name and set the mangled name for cross-module reference.
			if fn.MangledName == "" {
				fn.MangledName = fn.Name
			}
			// The function will be registered under "alias.name" in semantic analysis.
		}
	}

	r.allModules = append(r.allModules, mod)
}

// filterFunctions returns the subset of functions matching the import's selective list.
// If no selective list is specified, all functions are returned (excluding main).
func (r *Resolver) filterFunctions(fns []*ast.FnDecl, imp *ast.ImportDecl, absPath string) []*ast.FnDecl {
	var result []*ast.FnDecl

	if len(imp.SelectFns) == 0 {
		// Import all functions except main (main belongs to the importing file).
		for _, fn := range fns {
			if fn.Name != "main" {
				result = append(result, fn)
			}
		}
		return result
	}

	// Selective import: only include listed functions.
	wanted := make(map[string]bool)
	for _, name := range imp.SelectFns {
		wanted[name] = true
	}

	found := make(map[string]bool)
	for _, fn := range fns {
		if wanted[fn.Name] {
			result = append(result, fn)
			found[fn.Name] = true
		}
	}

	// Report errors for functions that were requested but not found.
	for _, name := range imp.SelectFns {
		if !found[name] {
			r.addError(imp.Pos, absPath, fmt.Sprintf("function %q not found in imported module %q", name, imp.Path))
		}
	}

	return result
}

// addError records a resolution error.
func (r *Resolver) addError(pos ast.Position, file string, msg string) {
	r.errors = append(r.errors, &ResolveError{
		Message: msg,
		Pos:     pos,
		File:    file,
	})
}

// GetModules returns all resolved imported modules in order.
func (r *Resolver) GetModules() []*ImportedModule {
	return r.allModules
}

// CheckAliasConflicts validates that no two imports with the same alias
// export overlapping function names.
func (r *Resolver) CheckAliasConflicts() []*ResolveError {
	// Group modules by alias.
	aliasFuncs := make(map[string]map[string]*ImportedModule) // alias → funcName → first module

	for _, mod := range r.allModules {
		alias := mod.Alias
		if alias == "" {
			alias = "" // direct imports go into the "" bucket
		}
		if aliasFuncs[alias] == nil {
			aliasFuncs[alias] = make(map[string]*ImportedModule)
		}
		for _, fn := range mod.Functions {
			if existing, ok := aliasFuncs[alias][fn.Name]; ok {
				// Same function name under the same alias from different files.
				if existing.FilePath != mod.FilePath {
					r.errors = append(r.errors, &ResolveError{
						Message: fmt.Sprintf("function %q conflicts: imported from both %q and %q under alias %q",
							fn.Name, existing.Path, mod.Path, alias),
						Pos:  fn.Pos,
						File: mod.FilePath,
					})
				}
			} else {
				aliasFuncs[alias][fn.Name] = mod
			}
		}
	}

	return r.errors
}

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
//
// A maximum import depth guard prevents runaway recursion or OOM when the
// same module is reachable through many different import paths (diamond
// imports).
// ---------------------------------------------------------------------------

const maxImportDepth = 64

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

	// TargetOS and TargetArch are used to resolve #if blocks in imported files.
	TargetOS   string
	TargetArch string
}

// NewResolver creates a new import resolver rooted at the given source file path.
func NewResolver(sourceFilePath string) *Resolver {
	absPath, _ := filepath.Abs(sourceFilePath)
	// Resolve symlinks to ensure consistent deduplication.
	if evaled, err := filepath.EvalSymlinks(absPath); err == nil {
		absPath = evaled
	}
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

	// Remove duplicate functions/globals that were imported multiple times
	// (e.g. the same library pulled in transitively from several files).
	r.deduplicateModules()

	// Build the merged program: imported functions first, then original functions.
	merged := &ast.Program{
		Module:  prog.Module,
		Imports: prog.Imports,
		Pos:     prog.Pos,
	}

	// Add all imported functions (in import order, depth-first).
	for _, mod := range r.allModules {
		for _, fn := range mod.Functions {
			fn.Imported = true
			merged.Functions = append(merged.Functions, fn)
		}
		// Also merge global variables from imported modules.
		if mod.Program != nil {
			for _, g := range mod.Program.Globals {
				merged.Globals = append(merged.Globals, g)
			}
		}
	}

	// Add the original file's globals.
	merged.Globals = append(merged.Globals, prog.Globals...)

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
	// Resolve symlinks so that the same file accessed via different relative
	// paths is correctly deduplicated (fixes diamond-import OOM).
	if evaled, err2 := filepath.EvalSymlinks(absPath); err2 == nil {
		absPath = evaled
	}

	// Check file exists. If not, try directory import: path/main.nov.
	if info, err := os.Stat(absPath); os.IsNotExist(err) {
		// Try resolving as a directory with main.nov inside.
		dirPath := filepath.Join(baseDir, relPath)
		absDirPath, _ := filepath.Abs(dirPath)
		mainPath := filepath.Join(absDirPath, "main.nov")
		if _, err2 := os.Stat(mainPath); err2 == nil {
			absPath = mainPath
			if evaled, err3 := filepath.EvalSymlinks(absPath); err3 == nil {
				absPath = evaled
			}
		} else {
			r.addError(imp.Pos, importerFile, fmt.Sprintf("imported file not found: %s (resolved from %q)", absPath, imp.Path))
			return
		}
	} else if err == nil && info.IsDir() {
		// The path itself is a directory — look for main.nov inside.
		mainPath := filepath.Join(absPath, "main.nov")
		if _, err2 := os.Stat(mainPath); err2 == nil {
			absPath = mainPath
			if evaled, err3 := filepath.EvalSymlinks(absPath); err3 == nil {
				absPath = evaled
			}
		} else {
			r.addError(imp.Pos, importerFile, fmt.Sprintf("imported directory %s has no main.nov (resolved from %q)", absPath, imp.Path))
			return
		}
	}

	// Check for circular imports.
	for _, stackPath := range r.importStack {
		if stackPath == absPath {
			chain := append(r.importStack, absPath)
			r.addError(imp.Pos, importerFile, fmt.Sprintf("circular import detected: %s", strings.Join(chain, " → ")))
			return
		}
	}

	// Guard against runaway import depth (diamond/fan-out graphs).
	if len(r.importStack) >= maxImportDepth {
		r.addError(imp.Pos, importerFile, fmt.Sprintf("import depth limit (%d) exceeded — possible diamond or recursive import", maxImportDepth))
		return
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

	// Resolve #if blocks in the imported file using the target OS/arch.
	if r.TargetOS != "" || r.TargetArch != "" {
		constants := map[string]string{
			"os":   r.TargetOS,
			"arch": r.TargetArch,
		}
		ast.ResolveCompTimeBlocks(prog, constants)
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

// funcSignature returns a deduplication key for a function: "name(paramType1,paramType2):returnType".
func funcSignature(fn *ast.FnDecl) string {
	var params []string
	for _, p := range fn.Params {
		if p.Type != nil {
			params = append(params, p.Type.Name)
		} else {
			params = append(params, "void")
		}
	}
	ret := "void"
	if fn.ReturnType != nil {
		ret = fn.ReturnType.Name
	}
	return fn.Name + "(" + strings.Join(params, ",") + "):" + ret
}

// deduplicateModules removes duplicate function entries across all resolved
// modules.  Two functions are considered duplicates if they share the same
// alias and signature (name + parameter types + return type).  Only the first
// occurrence is kept; subsequent identical imports are silently dropped.
// This also deduplicates global variables by name within each alias bucket.
func (r *Resolver) deduplicateModules() {
	// ---- deduplicate functions ----
	type fnKey struct {
		alias string
		sig   string
	}
	seenFns := make(map[fnKey]bool)

	for _, mod := range r.allModules {
		var deduped []*ast.FnDecl
		for _, fn := range mod.Functions {
			key := fnKey{alias: mod.Alias, sig: funcSignature(fn)}
			if !seenFns[key] {
				seenFns[key] = true
				deduped = append(deduped, fn)
			}
		}
		mod.Functions = deduped
	}

	// ---- deduplicate globals ----
	seenGlobals := make(map[string]bool)
	for _, mod := range r.allModules {
		if mod.Program == nil {
			continue
		}
		var dedupedGlobals []*ast.GlobalVar
		for _, g := range mod.Program.Globals {
			if !seenGlobals[g.Name] {
				seenGlobals[g.Name] = true
				dedupedGlobals = append(dedupedGlobals, g)
			}
		}
		mod.Program.Globals = dedupedGlobals
	}
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
// export overlapping function names with *different* signatures from
// *different* source files.  Functions that share the same name AND the same
// signature (parameter types + return type) are allowed — this supports the
// common pattern of the same library being transitively imported from
// multiple files.  Overloads (same name, different params) from the same
// source file are also allowed.
func (r *Resolver) CheckAliasConflicts() []*ResolveError {
	type sigOrigin struct {
		filePath string
		sig      string
		modPath  string
	}

	// alias → funcName → list of (sig, filePath) pairs we've seen
	aliasFuncs := make(map[string]map[string][]sigOrigin)

	for _, mod := range r.allModules {
		alias := mod.Alias
		if aliasFuncs[alias] == nil {
			aliasFuncs[alias] = make(map[string][]sigOrigin)
		}
		for _, fn := range mod.Functions {
			sig := funcSignature(fn)
			origins := aliasFuncs[alias][fn.Name]

			// Check whether we already have this exact signature from another file.
			duplicate := false
			for _, o := range origins {
				if o.sig == sig {
					// Same signature — accepted (duplicate import).
					duplicate = true
					break
				}
				if o.filePath != mod.FilePath {
					// Different signature from a different file — conflict.
					r.errors = append(r.errors, &ResolveError{
						Message: fmt.Sprintf(
							"function %q conflicts: imported from %q (sig %s) and %q (sig %s) under alias %q",
							fn.Name, o.modPath, o.sig, mod.Path, sig, alias),
						Pos:  fn.Pos,
						File: mod.FilePath,
					})
					duplicate = true
					break
				}
				// Different signature from the same file — overload, OK.
			}
			if !duplicate {
				aliasFuncs[alias][fn.Name] = append(origins, sigOrigin{
					filePath: mod.FilePath,
					sig:      sig,
					modPath:  mod.Path,
				})
			}
		}
	}

	return r.errors
}

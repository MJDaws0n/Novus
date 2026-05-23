package imports

import (
	"novus/internal/ast"
	"novus/internal/lexer"
	"novus/internal/parser"
	"novus/internal/semantic"
	"os"
	"path/filepath"
	"testing"
)

func parseResolveAnalyzeExample(t *testing.T, relPath string, targetOS string, targetArch string) {
	t.Helper()

	absPath, err := filepath.Abs(relPath)
	if err != nil {
		t.Fatalf("resolve path %q: %v", relPath, err)
	}

	content, err := os.ReadFile(absPath)
	if err != nil {
		t.Fatalf("read %s: %v", absPath, err)
	}

	tokens, lexErrs := lexer.Lex(string(content))
	if len(lexErrs) > 0 {
		t.Fatalf("lex errors in %s: %v", relPath, lexErrs)
	}

	prog, parseErrs := parser.Parse(tokens)
	if len(parseErrs) > 0 {
		t.Fatalf("parse errors in %s: %v", relPath, parseErrs)
	}

	ast.ResolveCompTimeBlocks(prog, map[string]string{
		"os":   targetOS,
		"arch": targetArch,
	})

	resolver := NewResolver(absPath)
	resolver.TargetOS = targetOS
	resolver.TargetArch = targetArch

	merged, resolveErrs := resolver.Resolve(prog, absPath)
	if len(resolveErrs) > 0 {
		t.Fatalf("resolve errors in %s (%s/%s): %v", relPath, targetOS, targetArch, resolveErrs)
	}

	if conflictErrs := resolver.CheckAliasConflicts(); len(conflictErrs) > 0 {
		t.Fatalf("alias conflicts in %s (%s/%s): %v", relPath, targetOS, targetArch, conflictErrs)
	}

	var importedFuncs []semantic.ImportedFunc
	for _, mod := range resolver.GetModules() {
		for _, fn := range mod.Functions {
			importedFuncs = append(importedFuncs, semantic.ImportedFunc{
				Fn:    fn,
				Alias: mod.Alias,
			})
		}
	}

	diagnostics := semantic.AnalyzeWithImports(merged, importedFuncs)
	for _, d := range diagnostics {
		if d.Severity == semantic.Error {
			t.Fatalf("semantic error in %s (%s/%s): %s", relPath, targetOS, targetArch, d.Error())
		}
	}
}

func TestExampleAppsResolveAndAnalyze_LinuxAMD64(t *testing.T) {
	files := []string{
		"../../examples/apps/hello-matrix/main.nov",
		"../../examples/apps/dice-duel/main.nov",
		"../../examples/apps/string-lab/main.nov",
		"../../examples/apps/portable-sanity/main.nov",
	}
	for _, f := range files {
		parseResolveAnalyzeExample(t, f, "linux", "amd64")
	}
}

func TestPortableSanityResolveAndAnalyze_CrossTargets(t *testing.T) {
	targets := []struct {
		os   string
		arch string
	}{
		{os: "linux", arch: "arm64"},
		{os: "darwin", arch: "arm64"},
		{os: "windows", arch: "amd64"},
	}
	for _, target := range targets {
		parseResolveAnalyzeExample(t, "../../examples/apps/portable-sanity/main.nov", target.os, target.arch)
	}
}

func TestHelloMatrixResolveAndAnalyze_WindowsAMD64(t *testing.T) {
	parseResolveAnalyzeExample(t, "../../examples/apps/hello-matrix/main.nov", "windows", "amd64")
}

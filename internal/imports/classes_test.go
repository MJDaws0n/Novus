package imports

import (
	"novus/internal/lexer"
	"novus/internal/parser"
	"novus/internal/semantic"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestClassImportAndAccessErrors(t *testing.T) {
	for _, test := range []struct {
		name, imports, declarations, body, want string
	}{
		{"private field", "import first;", "", "let item: Box = new Box(); item.hidden = 3;", "Box.hidden is private"},
		{"private method", "import first a;", "", "let item: a.Box = new a.Box(); item.secret();", "Box.secret is private"},
		{"private constructor", "import first;", "", "let item: Locked = new Locked();", "Locked.init is private"},
		{"private static field", "import first;", "", "Box.total += 1;", "Box.total is private"},
		{"static via instance", "import first;", "", "let item: Box = new Box(); item.make();", "requires class access"},
		{"instance via class", "import first;", "", "Box.secret();", "requires instance access"},
		{"constructor without new", "import first a;", "", "a.Box();", "use new a.Box"},
		{"direct collision", "import first; import second;", "", "", "class name \"Box\" is ambiguous"},
		{"alias collision", "import first a; import second a;", "", "", "class name \"a.Box\" is ambiguous"},
		{"function collision", "import first;", "fn Box() -> void {}", "", "conflicts with a class name"},
		{"duplicate class", "", "class Box {} class Box {}", "", "class name \"Box\" is ambiguous"},
		{"selection hides class", "import first[Locked];", "", "new Box();", "unknown class \"Box\""},
		{"invalid selector", "import first[Missing];", "", "", "\"Missing\" not found"},
		{"different class identities", "import first a; import second b;", "", "let item: a.Box = new b.Box();", "cannot assign"},
		{"different array element types", "import first a; import second b;", "", "let items: []a.Box = [new a.Box(), new b.Box()];", "array element has type"},
		{"static has no self", "", "class Box { pub static fn test() -> i32 { return self.value; } }", "", "self is available only on an instance"},
	} {
		t.Run(test.name, func(t *testing.T) {
			dir := t.TempDir()
			writeTestFile(t, dir, "first.nov", `module first;
class Box {
	let hidden: i32;
	priv static let total: i32;
	pub init() {}
	fn secret() -> void {}
	pub static fn make() -> Box { return new Box(); }
}
class Locked { init() {} }
`)
			writeTestFile(t, dir, "second.nov", "module second; class Box {}")
			source := "module app; " + test.imports + " " + test.declarations + " fn main() -> i32 { " + test.body + " return 0; }"
			tokens, lexErrors := lexer.Lex(source)
			if len(lexErrors) != 0 {
				t.Fatal(lexErrors)
			}
			program, parseErrors := parser.Parse(tokens)
			if len(parseErrors) != 0 {
				t.Fatal(parseErrors)
			}
			path := filepath.Join(dir, "main.nov")
			resolver := NewResolver(path)
			merged, resolveErrors := resolver.Resolve(program, path)
			for _, err := range resolveErrors {
				if strings.Contains(err.Error(), test.want) {
					return
				}
			}
			if len(resolveErrors) != 0 {
				t.Fatalf("unexpected import errors: %v", resolveErrors)
			}
			var imported []semantic.ImportedFunc
			for _, module := range resolver.GetModules() {
				for _, fn := range module.Functions {
					imported = append(imported, semantic.ImportedFunc{Fn: fn, Alias: module.Alias})
				}
			}
			diagnostics := semantic.AnalyzeWithImports(merged, imported)
			for _, diagnostic := range diagnostics {
				if diagnostic.Severity == semantic.Error && strings.Contains(diagnostic.Message, test.want) {
					return
				}
			}
			t.Fatalf("expected %q, got %v", test.want, diagnostics)
		})
	}
}

func writeTestFile(t *testing.T, dir, name, source string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte(source), 0644); err != nil {
		t.Fatal(err)
	}
}

package main

import (
	"fmt"
	"novus/internal/ast"
	"novus/internal/codegen"
	"novus/internal/lexer"
	"novus/internal/parser"
	"novus/internal/semantic"
	"os"
	"time"
)

const VERSION = "0.1.0"

func main() {
	start := time.Now()
	exitCode := run()
	if exitCode == 0 {
		fmt.Printf("Compile time: %s\n", time.Since(start))
	}
	os.Exit(exitCode)
}

func run() int {
	// Inititial print statements
	fmt.Println("Novus Compiler V" + VERSION)
	fmt.Println("Made by MJDawson. https://mjdawson.net & https://github.com/MJDaws0n")
	printDebug("Using debug mode.")

	if len(os.Args) < 2 {
		fmt.Println("Usage: novus [flags] <file>")
		return 1
	}

	// Find the source file (first non-flag argument).
	var filePath string
	for _, arg := range os.Args[1:] {
		if len(arg) > 0 && arg[0] != '-' {
			filePath = arg
			break
		}
	}
	if filePath == "" {
		fmt.Println("Usage: novus [flags] <file>")
		return 1
	}
	printDebug("Building using: " + filePath)

	// Check file exists
	if !fileExists(filePath) {
		fmt.Println("Error: File does not exist.")
		return 1
	}

	printDebug("File found, building: " + filePath)

	// Get file content
	var fileContent string
	var fileContentError error
	fileContent, fileContentError = getFileContent(filePath)

	if fileContentError != nil {
		fmt.Println("Error: Could not read file.")
		fmt.Println("Error details: " + fileContentError.Error())
		return 1
	}

	printDebug("Starting lexing process...")
	tokens, lexErrors := lexer.Lex(fileContent)
	if len(lexErrors) > 0 {
		fmt.Println("Lexing errors:")
		for _, e := range lexErrors {
			fmt.Printf("  %s\n", e.Error())
		}
		return 1
	}
	printDebug(fmt.Sprintf("Lexing complete. %d tokens produced.", len(tokens)))
	printTokens(tokens)

	// --- Parsing ---
	printDebug("Starting parsing process...")
	program, parseErrors := parser.Parse(tokens)
	if len(parseErrors) > 0 {
		fmt.Println("Parse errors:")
		for _, e := range parseErrors {
			fmt.Printf("  %s\n", e.Error())
		}
		return 1
	}
	printDebug("Parsing complete. No errors.")
	printDebug("--- AST ---")
	fmt.Print(ast.DebugString(program))
	printDebug("--- End AST ---")

	// --- Semantic analysis ---
	printDebug("Starting semantic analysis...")
	diagnostics := semantic.Analyze(program)

	// Separate warnings and errors.
	var semWarnings, semErrors []semantic.Diagnostic
	for _, d := range diagnostics {
		if d.Severity == semantic.Warning {
			semWarnings = append(semWarnings, d)
		} else {
			semErrors = append(semErrors, d)
		}
	}

	// Always print warnings.
	if len(semWarnings) > 0 {
		fmt.Println("Warnings:")
		for _, w := range semWarnings {
			fmt.Printf("  %s\n", w.Error())
		}
	}

	// Print errors and exit.
	if len(semErrors) > 0 {
		fmt.Println("Semantic errors:")
		for _, e := range semErrors {
			fmt.Printf("  %s\n", e.Error())
		}
		return 1
	}
	printDebug("Semantic analysis complete. No errors.")

	// --- Code generation ---
	printDebug("Starting code generation...")

	codegenOpts := codegen.DefaultOptions()
	codegenOpts.Verbose = true

	// Check for --asm-only flag.
	for _, arg := range os.Args[1:] {
		switch arg {
		case "--asm-only":
			codegenOpts.AsmOnly = true
		case "--skip-link":
			codegenOpts.SkipLink = true
		}
	}

	// Check for --target=os/arch flag.
	for _, arg := range os.Args[1:] {
		if len(arg) > 9 && arg[:9] == "--target=" {
			parts := splitTarget(arg[9:])
			if len(parts) == 2 {
				target, err := codegen.ResolveTarget(parts[0], parts[1])
				if err != nil {
					fmt.Printf("Error: %s\n", err)
					return 1
				}
				codegenOpts.Target = target
			} else {
				fmt.Printf("Error: invalid target format %q (expected os/arch, e.g. linux/amd64)\n", arg[9:])
				return 1
			}
		}
	}

	result, err := codegen.Generate(program, codegenOpts)
	if err != nil {
		fmt.Printf("Codegen error: %s\n", err)
		return 1
	}

	fmt.Println("Build artifacts:")
	if result.AsmFile != "" {
		fmt.Printf("  Assembly: %s\n", result.AsmFile)
	}
	if result.ObjFile != "" {
		fmt.Printf("  Object:   %s\n", result.ObjFile)
	}
	if result.ExeFile != "" {
		fmt.Printf("  Binary:   %s\n", result.ExeFile)
	}

	printDebug("Compilation pipeline finished successfully.")
	return 0
}

func splitTarget(s string) []string {
	for i, c := range s {
		if c == '/' {
			return []string{s[:i], s[i+1:]}
		}
	}
	return []string{s}
}

/**
* Prints a debug message to the console.
* @param message The message to print.
 */
func printDebug(message string) {
	fmt.Println("[DEBUG] " + message)
}
func printTokens(tokens []lexer.Token) {
	for _, token := range tokens {
		fmt.Printf("[DEBUG] Token: %s, Value: %s, Line: %d, Column: %d\n", token.Type, token.Value, token.Line, token.Column)
	}
}

/**
* Checks if a file exists at the given path.
* @param filePath The path to the file to check.
* @return true if the file exists, false otherwise.
 */
func fileExists(filePath string) bool {
	if _, err := os.Stat(filePath); os.IsNotExist(err) {
		return false
	}
	return true
}

/**
* Gets content of a file at the given path.
* @param filePath The path to the file to read.
* @return The content of the file as a string, or an error if the file cannot be read.
 */
func getFileContent(filePath string) (string, error) {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

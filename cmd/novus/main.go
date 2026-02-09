package main

import (
	"fmt"
	"novus/internal/lexer"
	"os"
	// "novus/internal/codegen"
	// "novus/internal/parser"
	// "novus/internal/semantic"
	// "novus/internal/ast"
)

const VERSION = "0.1.0"

func main() {
	// Inititial print statements
	fmt.Println("Novus Compiler V" + VERSION)
	fmt.Println("Made by MJDawson. https://mjdawson.net & https://github.com/MJDaws0n")
	printDebug("Using debug mode.")

	if len(os.Args) < 2 {
		fmt.Println("Usage: novus <file>")
		os.Exit(1)
	}

	// Get file to compile
	var filePath string = os.Args[1]
	printDebug("Building using: " + filePath)

	// Check file exists
	if !fileExists(filePath) {
		fmt.Println("Error: File does not exist.")
		os.Exit(1)
	}

	printDebug("File found, building: " + filePath)

	// Get file content
	var fileContent string
	var fileContentError error
	fileContent, fileContentError = getFileContent(filePath)

	if fileContentError != nil {
		fmt.Println("Error: Could not read file.")
		fmt.Println("Error details: " + fileContentError.Error())
		os.Exit(1)
	}

	printDebug("Starting lexing process...")
	tokens, lexErrors := lexer.Lex(fileContent)
	if len(lexErrors) > 0 {
		fmt.Println("Lexing errors:")
		for _, e := range lexErrors {
			fmt.Printf("  %s\n", e.Error())
		}
		os.Exit(1)
	}
	printTokens(tokens)
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

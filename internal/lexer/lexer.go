package lexer

type Token struct {
	Type   string
	Value  string
	Line   int
	Column int
}

func Lex(input string) []Token {
	var tokens []Token

	var inString bool = false
	var lineNumber int = 1
	var columnNumber int = 1
	var currentType string = "UNKNOWN"
	var soFar = ""

	for i := 0; i < len(input); i++ {
		var char byte = input[i]
		var charBefore byte
		if i == 0 {
			charBefore = 0
		} else {
			charBefore = input[i-1]
		}

		if isStringDelimiter(char, charBefore) {
			inString = !inString
		}

		if char == ';' && !inString && !isEscapeCharacter(charBefore) {
			tokens = append(tokens, Token{
				Type:   currentType,
				Value:  soFar,
				Line:   lineNumber,
				Column: columnNumber,
			})
			soFar = ""
		}

		// Final Bits
		soFar += string(char)
		columnNumber++
		if isNewline(char) {
			columnNumber = 1
			lineNumber++
		}
	}
	return tokens
}

func isWhitespace(char byte) bool {
	return char == ' '
}
func isStringDelimiter(char byte, charBefore byte) bool {
	return (char == '"' || char == '\'') && !isEscapeCharacter(charBefore)
}
func isEscapeCharacter(char byte) bool {
	return char == '\\'
}
func isNewline(char byte) bool {
	return char == '\n'
}

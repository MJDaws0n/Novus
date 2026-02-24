package lexer

import "fmt"

const (
	// Special
	EOF     = "EOF"
	ILLEGAL = "ILLEGAL"

	// Literals
	IDENT  = "IDENT"  // identifiers: msg, win32, push, eax, …
	INT    = "INT"    // integer literals: 0, 42, 0xFF, …
	FLOAT  = "FLOAT"  // float literals: 3.14, 0.5, 1.0e10, …
	STRING = "STRING" // string literals: "hello", '\r\n', …

	// Keywords
	MODULE   = "MODULE"
	IMPORT   = "IMPORT"
	FN       = "FN"
	LET      = "LET"
	RETURN   = "RETURN"
	TRUE     = "TRUE"
	FALSE    = "FALSE"
	IF       = "IF"
	ELSE     = "ELSE"
	FOR      = "FOR"
	WHILE    = "WHILE"
	BREAK    = "BREAK"
	CONTINUE = "CONTINUE"

	// Type keywords
	VOID = "VOID"
	BOOL = "BOOL"
	STR  = "STR"
	U8   = "U8"
	U16  = "U16"
	U32  = "U32"
	U64  = "U64"
	I8   = "I8"
	I16  = "I16"
	I32  = "I32"
	I64  = "I64"
	F32  = "F32"
	F64  = "F64"

	// Delimiters
	LPAREN    = "LPAREN"    // (
	RPAREN    = "RPAREN"    // )
	LBRACE    = "LBRACE"    // {
	RBRACE    = "RBRACE"    // }
	LBRACKET  = "LBRACKET"  // [
	RBRACKET  = "RBRACKET"  // ]
	SEMICOLON = "SEMICOLON" // ;
	COLON     = "COLON"     // :
	COMMA     = "COMMA"     // ,
	DOT       = "DOT"       // .
	HASH      = "HASH"      // #

	// Operators
	ARROW     = "ARROW"     // ->
	ASSIGN    = "ASSIGN"    // =
	PLUS      = "PLUS"      // +
	MINUS     = "MINUS"     // -
	STAR      = "STAR"      // *
	SLASH     = "SLASH"     // /
	PERCENT   = "PERCENT"   // %
	AMPERSAND = "AMPERSAND" // &
	BANG      = "BANG"      // !
	PIPE      = "PIPE"      // |
	CARET     = "CARET"     // ^
	TILDE     = "TILDE"     // ~
	SHL       = "SHL"       // <<
	SHR       = "SHR"       // >>

	// Comparison operators
	EQ  = "EQ"  // ==
	NEQ = "NEQ" // !=
	LT  = "LT"  // <
	GT  = "GT"  // >
	LTE = "LTE" // <=
	GTE = "GTE" // >=

	// Logical operators
	AND = "AND" // &&
	OR  = "OR"  // ||
)

// keywords maps reserved words to their token types.
var keywords = map[string]string{
	"module":   MODULE,
	"import":   IMPORT,
	"fn":       FN,
	"let":      LET,
	"return":   RETURN,
	"true":     TRUE,
	"false":    FALSE,
	"if":       IF,
	"else":     ELSE,
	"for":      FOR,
	"while":    WHILE,
	"break":    BREAK,
	"continue": CONTINUE,
	"void":     VOID,
	"bool":     BOOL,
	"str":      STR,
	"u8":       U8,
	"u16":      U16,
	"u32":      U32,
	"u64":      U64,
	"i8":       I8,
	"i16":      I16,
	"i32":      I32,
	"i64":      I64,
	"f32":      F32,
	"f64":      F64,
}

// Token represents a single lexical token produced by the lexer.
type Token struct {
	Type   string
	Value  string
	Line   int
	Column int
}

// LexError represents a recoverable error encountered during lexing.
type LexError struct {
	Message string
	Lexeme  string
	Line    int
	Column  int
}

func (e LexError) Error() string {
	return fmt.Sprintf("line %d, col %d: %s (got %q)", e.Line, e.Column, e.Message, e.Lexeme)
}

/**
* Lexes the given input string into a slice of Tokens. Also returns a slice of LexErrors for any recoverable errors encountered during lexing (e.g. unterminated strings).
* @param input The source code to lex.
* @return A slice of Tokens and a slice of LexErrors.
 */
func Lex(input string) ([]Token, []LexError) {
	var tokens []Token
	var errors []LexError
	line, col, i := 1, 1, 0

	for i < len(input) {
		ch := input[i]
		if isWhitespace(ch) {
			if ch == '\n' {
				line++
				col = 1
			} else if ch != '\r' {
				col++
			}
			i++
			continue
		}

		// Ignore comments
		if ch == '/' && i+1 < len(input) {
			// Single-line comment: // …
			if input[i+1] == '/' {
				i, col = skipLineComment(input, i, col)
				continue
			}
			// Multi-line comment: /* … */
			if input[i+1] == '*' {
				var err *LexError
				i, line, col, err = skipBlockComment(input, i, line, col)
				if err != nil {
					errors = append(errors, *err)
				}
				continue
			}
		}

		// Strings
		if ch == '"' || ch == '\'' {
			tok, errs, newI, newLine, newCol := lexString(input, i, line, col)
			i, line, col = newI, newLine, newCol
			errors = append(errors, errs...)
			if tok != nil {
				tokens = append(tokens, *tok)
			}
			continue
		}

		// Intigetrs
		if isDigit(ch) {
			tok, newI, newCol := lexNumber(input, i, line, col)
			tokens = append(tokens, tok)
			i, col = newI, newCol
			continue
		}

		// Keywords and identifiers
		if isIdentStart(ch) {
			tok, newI, newCol := lexIdentifier(input, i, line, col)
			tokens = append(tokens, tok)
			i, col = newI, newCol
			continue
		}

		// Multi-character and single-character operators / delimiters
		if tok, width := lexOperatorOrDelimiter(input, i, line, col); width > 0 {
			tokens = append(tokens, tok)
			i += width
			col += width
			continue
		}

		// Unknown characters
		errors = append(errors, LexError{
			Message: "unexpected character",
			Lexeme:  string(ch),
			Line:    line,
			Column:  col,
		})
		i++
		col++
	}

	tokens = append(tokens, Token{EOF, "", line, col})
	return tokens, errors
}

func skipLineComment(input string, i int, col int) (int, int) {
	for i < len(input) && input[i] != '\n' {
		i++
		col++
	}
	return i, col
}

func skipBlockComment(input string, i int, line int, col int) (int, int, int, *LexError) {
	startLine, startCol := line, col
	// Skip the opening /*
	i += 2
	col += 2

	for i < len(input) {
		if input[i] == '*' && i+1 < len(input) && input[i+1] == '/' {
			i += 2
			col += 2
			return i, line, col, nil
		}
		if input[i] == '\n' {
			line++
			col = 1
		} else if input[i] != '\r' {
			col++
		}
		i++
	}

	return i, line, col, &LexError{
		Message: "unterminated block comment",
		Lexeme:  "/*",
		Line:    startLine,
		Column:  startCol,
	}
}

func lexString(input string, start int, line int, col int) (*Token, []LexError, int, int, int) {
	quote := input[start]
	startLine, startCol := line, col
	var errs []LexError
	i := start + 1
	col++

	for i < len(input) {
		ch := input[i]

		// Newline inside a string → unterminated.
		if ch == '\n' || ch == '\r' {
			errs = append(errs, LexError{
				Message: "unterminated string literal (newline in string)",
				Lexeme:  input[start:i],
				Line:    startLine,
				Column:  startCol,
			})
			return nil, errs, i, line, col
		}

		// Escape sequence: validate and skip both characters.
		if ch == '\\' {
			if i+1 >= len(input) {
				errs = append(errs, LexError{
					Message: "unterminated escape sequence at end of input",
					Lexeme:  "\\",
					Line:    line,
					Column:  col,
				})
				return nil, errs, i + 1, line, col + 1
			}
			next := input[i+1]
			if !isValidEscape(next) {
				errs = append(errs, LexError{
					Message: fmt.Sprintf("invalid escape sequence '\\%c'", next),
					Lexeme:  string([]byte{'\\', next}),
					Line:    line,
					Column:  col,
				})
				// Continue scanning—the string may still be valid overall.
			}
			i += 2
			col += 2
			continue
		}

		// Closing quote.
		if ch == quote {
			tok := Token{
				Type:   STRING,
				Value:  input[start : i+1],
				Line:   startLine,
				Column: startCol,
			}
			i++
			col++
			return &tok, errs, i, line, col
		}

		i++
		col++
	}

	// Reached end of input without a closing quote.
	errs = append(errs, LexError{
		Message: "unterminated string literal (reached end of input)",
		Lexeme:  input[start:],
		Line:    startLine,
		Column:  startCol,
	})
	return nil, errs, i, line, col
}

// lexNumber scans an integer or float literal.
// Supports: decimal (42), hexadecimal (0xFF), float (3.14), and
// scientific notation (1.5e10, 2.0E-3).
// A trailing dot is only consumed as part of a float if followed by a digit,
// so that `obj.method` after an integer is not mis-parsed.
func lexNumber(input string, start int, line int, col int) (Token, int, int) {
	i := start
	startCol := col
	isFloat := false

	// Hexadecimal: 0x… / 0X…
	if input[i] == '0' && i+1 < len(input) && (input[i+1] == 'x' || input[i+1] == 'X') {
		i += 2
		col += 2
		for i < len(input) && isHexDigit(input[i]) {
			i++
			col++
		}
		return Token{INT, input[start:i], line, startCol}, i, col
	}

	// Decimal integer part
	for i < len(input) && isDigit(input[i]) {
		i++
		col++
	}

	// Fractional part: only if '.' is followed by a digit (avoids eating
	// the dot in "42.method()" or field access).
	if i < len(input) && input[i] == '.' && i+1 < len(input) && isDigit(input[i+1]) {
		isFloat = true
		i++ // consume '.'
		col++
		for i < len(input) && isDigit(input[i]) {
			i++
			col++
		}
	}

	// Exponent part: e/E followed by optional +/- and digits.
	if i < len(input) && (input[i] == 'e' || input[i] == 'E') {
		isFloat = true
		i++ // consume 'e'
		col++
		if i < len(input) && (input[i] == '+' || input[i] == '-') {
			i++
			col++
		}
		for i < len(input) && isDigit(input[i]) {
			i++
			col++
		}
	}

	tokType := INT
	if isFloat {
		tokType = FLOAT
	}
	return Token{tokType, input[start:i], line, startCol}, i, col
}

func lexIdentifier(input string, start int, line int, col int) (Token, int, int) {
	i := start
	startCol := col
	for i < len(input) && isIdentPart(input[i]) {
		i++
		col++
	}
	word := input[start:i]
	tokType := IDENT
	if kw, ok := keywords[word]; ok {
		tokType = kw
	}
	return Token{tokType, word, line, startCol}, i, col
}

// lexOperatorOrDelimiter tries to match a 1- or 2-character operator or
// delimiter starting at input[i]. Returns the token and the number of
// characters consumed (0 if nothing matched).
func lexOperatorOrDelimiter(input string, i int, line int, col int) (Token, int) {
	ch := input[i]
	var next byte
	if i+1 < len(input) {
		next = input[i+1]
	}

	// Two-character tokens
	switch ch {
	case '-':
		if next == '>' {
			return Token{ARROW, "->", line, col}, 2
		}
		return Token{MINUS, "-", line, col}, 1
	case '=':
		if next == '=' {
			return Token{EQ, "==", line, col}, 2
		}
		return Token{ASSIGN, "=", line, col}, 1
	case '!':
		if next == '=' {
			return Token{NEQ, "!=", line, col}, 2
		}
		return Token{BANG, "!", line, col}, 1
	case '<':
		if next == '=' {
			return Token{LTE, "<=", line, col}, 2
		}
		if next == '<' {
			return Token{SHL, "<<", line, col}, 2
		}
		return Token{LT, "<", line, col}, 1
	case '>':
		if next == '=' {
			return Token{GTE, ">=", line, col}, 2
		}
		if next == '>' {
			return Token{SHR, ">>", line, col}, 2
		}
		return Token{GT, ">", line, col}, 1
	case '&':
		if next == '&' {
			return Token{AND, "&&", line, col}, 2
		}
		return Token{AMPERSAND, "&", line, col}, 1
	case '|':
		if next == '|' {
			return Token{OR, "||", line, col}, 2
		}
		return Token{PIPE, "|", line, col}, 1
	}

	// Single-character tokens
	switch ch {
	case '(':
		return Token{LPAREN, "(", line, col}, 1
	case ')':
		return Token{RPAREN, ")", line, col}, 1
	case '{':
		return Token{LBRACE, "{", line, col}, 1
	case '}':
		return Token{RBRACE, "}", line, col}, 1
	case '[':
		return Token{LBRACKET, "[", line, col}, 1
	case ']':
		return Token{RBRACKET, "]", line, col}, 1
	case ';':
		return Token{SEMICOLON, ";", line, col}, 1
	case ':':
		return Token{COLON, ":", line, col}, 1
	case ',':
		return Token{COMMA, ",", line, col}, 1
	case '.':
		return Token{DOT, ".", line, col}, 1
	case '+':
		return Token{PLUS, "+", line, col}, 1
	case '*':
		return Token{STAR, "*", line, col}, 1
	case '/':
		return Token{SLASH, "/", line, col}, 1
	case '%':
		return Token{PERCENT, "%", line, col}, 1
	case '^':
		return Token{CARET, "^", line, col}, 1
	case '~':
		return Token{TILDE, "~", line, col}, 1
	case '#':
		return Token{HASH, "#", line, col}, 1
	}

	return Token{}, 0
}

func isWhitespace(ch byte) bool {
	return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

func isHexDigit(ch byte) bool {
	return isDigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
}

func isLetter(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
}

func isIdentStart(ch byte) bool {
	return isLetter(ch) || ch == '_'
}

func isIdentPart(ch byte) bool {
	return isLetter(ch) || isDigit(ch) || ch == '_'
}

func isValidEscape(ch byte) bool {
	switch ch {
	case 'n', 'r', 't', '\\', '\'', '"', '0':
		return true
	default:
		return false
	}
}

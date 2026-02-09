# Novus — Supported Syntax (Current)

What the current lexer recognizes as valid tokens.

## Keywords

### Modules
- `module`
- `import`

### Declarations
- `fn`
- `let`
- `return`

### Control flow
- `if`
- `else`
- `for`
- `while`
- `break`
- `continue`

### Boolean literals
- `true`
- `false`

## Data types (type keywords)

The lexer treats these as reserved words:

### Integer types
- Unsigned: `u8`, `u16`, `u32`, `u64`
- Signed: `i8`, `i16`, `i32`, `i64`

### Floating-point types
- `f32`, `f64`

### Other types
- `bool`
- `str`
- `void`

## Literals

### Identifiers
- `IDENT`: names like `msg`, `hello_win32`, `GetStdHandle`, `eax`
- Allowed characters: letters, digits, underscore
- Must start with a letter or underscore

### Integers
- `INT`: decimal digits, e.g. `0`, `42`
- `INT`: hex literals, e.g. `0xFF`, `0X1A`

### Floats
- `FLOAT`: decimals with a fractional part, e.g. `3.14`, `0.5`, `1.0`
- `FLOAT`: scientific notation, e.g. `1.5e10`, `2.0E-3`, `3e4`, `5E+2`

`42.method` lexes as `INT` `.` `IDENT` (the `.` is only part of a float when followed by a digit).

### Strings
- `STRING`: double-quoted and single-quoted strings
  - Examples: `"Hello"`, `'hi'`, `'\r\n'`
- Supported escapes (validated by the lexer):
  - `\\` `\"` `\'` `\n` `\r` `\t` `\0`
- Strings cannot span newlines (newline before closing quote is a lex error).

## Comments

- Single-line: `// comment until end of line`
- Multi-line: `/* comment */` (can span multiple lines)
- Unterminated block comments produce a lexing error.

## Delimiters / punctuation
- Parentheses: `(` `)`
- Braces: `{` `}`
- Separators: `;` `:` `,`
- Member access / dot: `.`

## Operators

### Assignment
- `=`

### Arithmetic
- `+` `-` `*` `/` `%`

### Addressing / misc
- `&`
- `->` (used in function return types)

### Comparison
- `==` `!=` `<` `>` `<=` `>=`

### Logic
- `!` `&&` `||`

## Errors (lexing)

The lexer reports recoverable errors with line and column:
- Unexpected/unknown character
- Unterminated string literal (newline or end-of-input)
- Invalid escape sequence in a string (string token still emitted)
- Unterminated block comment

## Notes / current constraints

- Because type names like `str`, `i32`, etc. are tokenized as keywords, they are effectively reserved words at the lexing stage.
- Anything not listed above (additional keywords, additional operators, character literals, multiline strings, etc.) is not currently defined by the lexer.

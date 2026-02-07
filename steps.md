# Lexical analysis (Lexer)
Take raw code, split into tokens.

# Parsing (Parser)
Tokens get put togeather into a structured tree. Then checks correct syntax, then create an abstract syntax tree. This is an AST.

# Semantic analysis
Ensures variables exists, functions have correct arguments, do anything that insn't just syntax based errors.

# Code generation
AST gets turned into assembly instructions for target machine.

# Linking and assembly
Converts assembly to machine code after linking anything it needs.


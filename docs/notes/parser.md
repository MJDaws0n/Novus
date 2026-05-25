# How to build
- Gets your list of tokens from the lexer.
- Reads them in order and groups them based on your language’s syntax rules.
- Builds a tree (AST) showing the structure. 

# Example
print(6 + 7) might look like:
```
PrintStatement
  └── AddExpression
        ├── Number(6)
        └── Number(7)
```
x = 67 might look like:
```
Assignment
  ├── Identifier(x)
  └── Number(67)
```
# How to build
- Code as a string.
- Loop through checking characters and matching it to a rule to map out what the current chunk is, a string, variable, operation etc.
- When a chunk is complete such as `67` or `"67 is the greatest number"` it gets added to a list of tokens.
- Ignore white spaces usually and comments (whole line, or up to comment end point).
- Keep going till the end is reached.`

# Example
print("67") might look like:
 - Token(type='IDENTIFIER', value='print')
 - Token(type='LPAREN', value='(')
 - Token(type='STRING', value='67')
 - Token(type='RPAREN', value=')')

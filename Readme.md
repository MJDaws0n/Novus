# Novus (notes)

Novus is a compiled language that sits between a low-level assembly and a modern language.

It aims to feel like “structured assembly”:
- **High-level structure**: variables, blocks, `if`/`else`, `while`, functions, modules/imports, strings.
- **Low-level primitives**: `mov`, `add`, `sub`, `cmp`, `jmp`/`jcc`, `call`, `ret`, `push`/`pop`, and an explicit boundary to the OS (`extern`/`syscall`).
- **Small core**: you can write “pure assembly style” if you want; importing a library just saves you repetition.

## What makes it "not fake"
Instructions like `mov eax, 123` are **not ordinary functions**.
They are **keywords / statements** in the grammar.

When you compile, those instructions become either:
- Real target assembly (e.g. x86) that gets assembled and linked into a real executable, or
- (optional later) machine code bytes directly.

So `mov` changes *real* registers in the final program because it becomes real machine instructions.

## Compilation pipeline (typical)
1. **Lexing**: turn source text into tokens.
2. **Parsing**: build an AST (functions, statements, expressions, instruction statements).
3. **Semantic checks**: names/scopes, types (even if minimal), constant folding.
4. **Lowering**:
   - Variables → stack slots (or registers when possible).
   - `if`/`while` → labels + `cmp` + conditional jumps.
   - Function calls → calling convention + stack frame.
   - Strings → a data section label (`.rdata`/`.rodata`) + references to that label.
5. **Codegen**: emit target assembly (recommended for sanity).
6. **Assemble + link**: run an external assembler/linker to produce an executable.

## Core language sketch
### Statements
- Variable declarations: `let x: i32 = 0`
- Assignments: `x = x + 1`
- Control flow: `if (...) { ... } else { ... }`, `while (...) { ... }`
- Functions: `fn main() -> i32 { ... }`
- Instruction statements: `mov eax, 1`, `add eax, ebx`, `cmp eax, 0`, `je label`

### Operands (idea)
- Registers: `eax`, `ebx`, `esp`, … (target-defined)
- Immediates: `123`, `0x7B`
- Variables: `x`
- Memory forms (optional): `[ebp-4]`, `[eax+8]`
- **String literals**: `"Hello"`

String literals are supported by lowering:
- The compiler puts the bytes into a data section with a label (e.g. `str_0`).
- An instruction like `mov eax, "Hello"` becomes “load the address of `str_0` into `eax`” (e.g. `lea eax, [str_0]`).

## Imports / libraries
`import essentials` can provide convenience wrappers like:
- `essentials.print("Hello")`
- `essentials.println(...)`
- `essentials.exit(code)`

Libraries can be written in the same language (preferred) and compiled together.

## Windows (Win32) calling idea
On 32-bit Windows, most Win32 APIs use `stdcall`:
- Arguments are pushed **right-to-left** on the stack.
- The callee typically cleans up the stack.
- Return value is in `eax`.

Your language can support a tiny FFI surface:
- `extern "stdcall" fn WriteFile(...) -> bool` (compiler uses correct call sequence)
- or a low-level approach: manually `push` args and `call` an extern symbol.

See the example in [language/examples/hello_win32.midasm](language/examples/hello_win32.midasm).

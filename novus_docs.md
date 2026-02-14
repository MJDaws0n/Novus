# Novus Language Documentation

This document is a complete reference for the **Novus** programming language syntax and all compiler built-in features. It covers every construct the compiler understands natively — standard library functions are **not** included here.

---

## Table of Contents

1. [Program Structure](#program-structure)
2. [Modules](#modules)
3. [Imports](#imports)
4. [Comments](#comments)
5. [Types](#types)
6. [Literals](#literals)
7. [Variables](#variables)
8. [Functions](#functions)
9. [Operators](#operators)
10. [Control Flow](#control-flow)
11. [Arrays](#arrays)
12. [Strings](#strings)
13. [Built-in Intrinsic Functions](#built-in-intrinsic-functions)
14. [CPU Registers](#cpu-registers)
15. [Address-of Operator](#address-of-operator)
16. [Escape Sequences](#escape-sequences)
17. [Function Overloading](#function-overloading)
18. [Global Variables](#global-variables)
19. [Target Platforms](#target-platforms)

---

## Program Structure

A Novus source file (`.nov`) has the following top-level structure, in order:

1. An optional **module declaration**
2. Zero or more **import declarations**
3. Zero or more **global variable declarations** and **function declarations** (in any order)

The entry point of a Novus program is the `main` function.

```novus
module my_program;

import ../lib/standard_lib_macos_silicon;

let global_val: i32 = 42;

fn main() -> i32 {
    // program starts here
    return 0;
}
```

Every statement in Novus is terminated with a semicolon (`;`).

---

## Modules

A module declaration names the current source file. It must appear before any imports or declarations.

```novus
module my_module;
```

The module name is a single identifier. It is used as the default output name for the compiled binary.

---

## Imports

Import declarations bring functions from other `.nov` source files into scope. Imports must appear after the module declaration and before any function or variable declarations.

### Basic import

```novus
import ../lib/standard_lib_macos_silicon;
```

The path is relative to the current file, uses `/` as the separator, and omits the `.nov` extension. All public functions from the imported file become available directly.

### Import with alias (namespaced)

```novus
import ../lib/maths maths;
```

Functions from the imported file are accessed via the alias:

```novus
maths.is_even(42);
```

### Selective import

Import only specific functions:

```novus
import ../lib/standard_lib[print, exit];
```

### Selective import with alias

```novus
import ../lib/maths[is_even, is_prime] maths;
```

### Multi-segment paths

Paths can include multiple segments and `..` for parent directories:

```novus
import ../lib/standard_lib;
import foo/bar/baz;
```

---

## Comments

Novus supports two styles of comments:

### Single-line comments

```novus
// This is a single-line comment
```

### Multi-line comments

```novus
/* This is a
   multi-line comment */
```

---

## Types

Novus is statically typed. Every variable and function parameter must have an explicit type annotation.

### Void

| Type   | Description            |
|--------|------------------------|
| `void` | No value (for function return types only) |

### Boolean

| Type   | Description         |
|--------|---------------------|
| `bool` | Boolean (`true` or `false`) |

### String

| Type  | Description                        |
|-------|------------------------------------|
| `str` | A null-terminated byte string      |

### Unsigned integers

| Type  | Size    | Range                    |
|-------|---------|--------------------------|
| `u8`  | 8-bit   | 0 to 255                 |
| `u16` | 16-bit  | 0 to 65,535              |
| `u32` | 32-bit  | 0 to 4,294,967,295       |
| `u64` | 64-bit  | 0 to 18,446,744,073,709,551,615 |

### Signed integers

| Type  | Size    | Range                                    |
|-------|---------|------------------------------------------|
| `i8`  | 8-bit   | -128 to 127                              |
| `i16` | 16-bit  | -32,768 to 32,767                        |
| `i32` | 32-bit  | -2,147,483,648 to 2,147,483,647          |
| `i64` | 64-bit  | -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807 |

### Floating-point

| Type  | Size    | Description                     |
|-------|---------|---------------------------------|
| `f32` | 32-bit  | Single-precision IEEE 754 float |
| `f64` | 64-bit  | Double-precision IEEE 754 float |

### Array types

Array types are written with `[]` before the element type:

```novus
[]i32    // array of i32
[]str    // array of strings
[]u64    // array of u64
```

### Untyped literals

Integer and float literals have special **untyped** status in the type system:

- An **untyped integer literal** (e.g. `42`) is automatically compatible with any integer type (`i8`–`i64`, `u8`–`u64`) and any float type (`f32`, `f64`). No explicit cast is needed.
- An **untyped float literal** (e.g. `3.14`) is automatically compatible with any float type (`f32`, `f64`).

This means you can write:

```novus
let x: i64 = 100;    // 100 adapts to i64
let y: f64 = 3.14;   // 3.14 adapts to f64
let z: u8 = 255;     // 255 adapts to u8
```

---

## Literals

### Integer literals

Novus supports decimal and hexadecimal integer notation:

```novus
42          // decimal
0xFF        // hexadecimal
0x2000004   // hexadecimal (commonly used for macOS syscall numbers)
```

### Float literals

Standard decimal notation with optional scientific notation:

```novus
3.14
0.5
1.0e10
2.0E-3
```

A trailing dot is only interpreted as a float if followed by a digit, so `obj.method` after an integer is parsed correctly.

### String literals

Strings can use either double or single quotes:

```novus
"Hello, World!"
'Goodbye, World!'
```

Both produce `str` values. A single-quoted literal that decodes to exactly one byte is treated as an **immediate byte value** (useful for character comparisons):

```novus
'\0'    // null byte (value 0)
'\n'    // newline (value 10)
'A'     // byte value 65
```

### Boolean literals

```novus
true
false
```

### Array literals

```novus
[10, 20, 30]       // array with 3 elements
[]                  // empty array
[1, 2, 3,]         // trailing comma is allowed
```

---

## Variables

Variables are declared with `let` and require a type annotation and an initialiser:

```novus
let name: type = value;
```

### Examples

```novus
let x: i32 = 0;
let msg: str = "hello";
let flag: bool = true;
let nums: []i32 = [1, 2, 3];
```

### Assignment

Variables can be reassigned after declaration:

```novus
let count: i32 = 0;
count = count + 1;
```

Valid assignment targets are:
- Identifiers (`x = 5;`)
- Array index expressions (`nums[0] = 99;`)
- String index expressions (`str[0] = 'A';` — writes a single byte)

### Shadowing

Declaring a variable with the same name in an inner scope shadows the outer variable. The compiler produces a **warning** for shadows. Redeclaring a variable in the **same** scope is an **error**.

---

## Functions

Functions are declared with the `fn` keyword, a parameter list, a return type after `->`, and a body:

```novus
fn name(param1: type1, param2: type2) -> return_type {
    // body
}
```

### Void functions

Functions that return nothing use `void`:

```novus
fn greet() -> void {
    // no return value
}
```

### Return statements

```novus
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

A bare `return;` is allowed in void functions. Non-void functions must return a value on all code paths.

### Default parameter values

Parameters can have default values. Default parameters must come after all required parameters:

```novus
fn input(prompt: str, buf_len: i32 = 128) -> str {
    // buf_len defaults to 128 if not provided
}
```

Calling with or without the defaulted argument:

```novus
input("Name: ");        // buf_len = 128
input("Name: ", 256);   // buf_len = 256
```

### Function calls

```novus
let result: i32 = add(1, 2);
print("hello");
```

---

## Operators

### Arithmetic operators

| Operator | Description    | Operands             |
|----------|----------------|----------------------|
| `+`      | Addition       | Numeric, or `str + str` (concatenation) |
| `-`      | Subtraction    | Numeric              |
| `*`      | Multiplication | Numeric              |
| `/`      | Division       | Numeric (signed)     |
| `%`      | Modulo         | Numeric (signed)     |

### Comparison operators

All comparison operators return `bool`:

| Operator | Description        |
|----------|--------------------|
| `==`     | Equal              |
| `!=`     | Not equal          |
| `<`      | Less than          |
| `>`      | Greater than       |
| `<=`     | Less than or equal |
| `>=`     | Greater than or equal |

`==` and `!=` work on any comparable types (numeric, bool, str). The relational operators (`<`, `>`, `<=`, `>=`) require numeric operands.

### Logical operators

| Operator | Description | Operands |
|----------|-------------|----------|
| `&&`     | Logical AND | `bool`   |
| `||`     | Logical OR  | `bool`   |

### Unary operators

| Operator | Description       | Operand  |
|----------|-------------------|----------|
| `-`      | Negation          | Numeric  |
| `!`      | Logical NOT       | `bool`   |
| `&`      | Address-of        | Variable |

### Operator precedence (highest to lowest)

| Level | Operators          |
|-------|--------------------|
| 7     | `()` `.` `[]` (call, member access, index) |
| 6     | `!` `-` `&` (unary) |
| 5     | `*` `/` `%`         |
| 4     | `+` `-`             |
| 3     | `<` `>` `<=` `>=`   |
| 2     | `==` `!=`           |
| 1     | `&&`                |
| 0     | `||`                |

Binary operators are **left-associative**. Parentheses `()` can be used to override precedence:

```novus
let x: i32 = (a + b) * c;
```

---

## Control Flow

### If / Else If / Else

Conditions must be enclosed in parentheses and must evaluate to `bool`. The body is always a block `{}`:

```novus
if (x > 0) {
    // ...
} else if (x == 0) {
    // ...
} else {
    // ...
}
```

### While loop

```novus
while (condition) {
    // ...
}
```

### For loop

The classic three-part for loop:

```novus
for (let i: i32 = 0; i < 10; i = i + 1) {
    // ...
}
```

The init clause can be a `let` statement or an assignment/expression. The update clause does **not** have a trailing semicolon. Both the init and update clauses are required syntactically (use an expression statement if either is not needed).

### Break and Continue

Inside loops, `break` exits the loop immediately and `continue` skips to the next iteration:

```novus
while (true) {
    if (done) {
        break;
    }
    if (skip) {
        continue;
    }
}
```

Using `break` or `continue` outside of a loop is a compile-time error.

---

## Arrays

Arrays are heap-allocated, dynamically-sized collections of elements with the same type.

### Creating arrays

```novus
let nums: []i32 = [10, 20, 30];
let empty: []i32 = [];
```

### Indexing

```novus
let first: i32 = nums[0];
nums[2] = 999;
```

Array indices must be integer types.

### Built-in array operations

The compiler has these array operations built in (they are **not** standard library functions):

| Function | Signature | Description |
|----------|-----------|-------------|
| `array_append` | `array_append(arr, value) -> void` | Append a value to the end of the array |
| `array_pop` | `array_pop(arr) -> element_type` | Remove and return the last element |

```novus
let nums: []i32 = [10, 20, 30];
array_append(nums, 40);           // nums is now [10, 20, 30, 40]
let last: i32 = array_pop(nums);  // last = 40, nums is now [10, 20, 30]
```

### Array memory layout

Arrays are stored on the heap with the layout: `[data_ptr (8 bytes), len (8 bytes), cap (8 bytes)]`. The data pointer points to a contiguous block of elements. You can read the length field manually using the low-level memory intrinsics:

```novus
fn array_len(arr: []i32) -> i32 {
    mov(x9, arr);
    let ptr: u64 = getreg(x9);
    return load32(ptr + 8);
}
```

---

## Strings

Strings in Novus are null-terminated byte strings (C-style).

### Concatenation

Use the `+` operator:

```novus
let greeting: str = "Hello, " + "World!";
```

Concatenation works at both compile time (when both operands are literal strings) and runtime.

When a byte value (from string indexing) is concatenated with a string using `+`, it is automatically promoted to a single-character string.

### Indexing

```novus
let ch: str = msg[0];        // returns the character as a single-byte value
msg[0] = 'A';                // write a single byte
```

String indexing returns a byte value. Comparing with single-quoted characters works naturally:

```novus
if (msg[i] == '\0') {
    // end of string
}
```

### Length

Use the built-in `len()` function:

```novus
let n: i32 = len(msg);
```

---

## Built-in Intrinsic Functions

These are **compiler built-in** functions that map directly to assembly instructions or low-level operations. They are always available without any import. Arguments marked with `*` accept any type (register, integer, string, etc.).

### Data movement

| Function | Signature | Description |
|----------|-----------|-------------|
| `mov` | `mov(dst*, src*) -> void` | Move data — copies `src` into `dst`. Commonly used to load values into CPU registers. |
| `lea` | `lea(dst*, src*) -> void` | Load effective address — loads the memory address of `src` into `dst`. |
| `push` | `push(value*) -> void` | Push a value onto the CPU stack. |
| `pop` | `pop() -> u64` | Pop a value from the CPU stack. |

```novus
mov(x0, 1);           // load 1 into register x0
mov(x1, msg);         // load the address of msg into x1
push(rax);            // save rax on the stack
let val: u64 = pop(); // restore from stack
```

### Register access

| Function | Signature | Description |
|----------|-----------|-------------|
| `setreg` | `setreg(reg*, value*) -> void` | Set a CPU register to a value. |
| `getreg` | `getreg(reg*) -> u64` | Read the current value of a CPU register. |

```novus
setreg(x0, 42);
let val: u64 = getreg(x0);
```

### Flag access

| Function | Signature | Description |
|----------|-----------|-------------|
| `setflag` | `setflag(flag*, value*) -> void` | Set a CPU flag. |
| `getflag` | `getflag(flag*) -> bool` | Read the state of a CPU flag. |

### System calls

| Function | Signature | Description |
|----------|-----------|-------------|
| `syscall` | `syscall() -> void` | Invoke a system call. Registers must be set up beforehand using `mov` or `setreg`. |
| `int` | `int(number*) -> void` | Trigger a software interrupt (e.g. `int(0x80)` on x86 Linux). |

```novus
// macOS ARM64: write "Hello\n" to stdout
mov(x0, 1);              // fd = stdout
mov(x1, msg);            // buffer
mov(x2, len(msg));       // length
mov(x16, 0x2000004);     // syscall number (write)
syscall();
```

### Control flow

| Function | Signature | Description |
|----------|-----------|-------------|
| `call` | `call(label*) -> void` | Call a function by label (low-level, raw assembly call). |
| `ret` | `ret() -> void` | Return from a function (low-level, raw assembly return). |
| `nop` | `nop() -> void` | No operation — does nothing. |

### Memory loads

| Function | Signature | Description |
|----------|-----------|-------------|
| `load8` | `load8(addr*) -> i32` | Load an unsigned byte from a memory address (zero-extended to i32). |
| `load32` | `load32(addr*) -> i32` | Load a 32-bit integer from a memory address. |
| `load64` | `load64(addr*) -> i64` | Load a 64-bit integer from a memory address. |

```novus
let test_str: str = "ABCD";
mov(x9, test_str);
let addr: u64 = getreg(x9);
let byte_a: i32 = load8(addr);       // 65 ('A')
let byte_b: i32 = load8(addr + 1);   // 66 ('B')
```

### String operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `len` | `len(s: str) -> i32` | Returns the length of a string (counts bytes until null terminator). |

Note: `len` is a compiler built-in that emits inline code to compute the string length without a function call.

### Windows API calls

| Function | Signature | Description |
|----------|-----------|-------------|
| `win_call` | `win_call(api_name: str, args...) -> void` | Call a Windows API function by name. Variadic — the first argument is the API function name as a string literal, remaining arguments are passed via Windows x64 calling convention. |

```novus
// Windows: get stdout handle and write to console
win_call("GetStdHandle", -11);
let out_handle: u64 = getreg(rax);
win_call("WriteConsoleA", out_handle, msg, msg_len, 0, 0);
```

---

## CPU Registers

Novus exposes CPU registers as reserved identifiers. They can be used directly in expressions, passed to built-in functions, and assigned to.

### x86 32-bit registers

`eax`, `ebx`, `ecx`, `edx`, `esi`, `edi`, `ebp`, `esp`

### x86 64-bit registers

`rax`, `rbx`, `rcx`, `rdx`, `rsi`, `rdi`, `rbp`, `rsp`, `r8`–`r15`

### x86 special registers

`eip`, `rip`, `eflags`, `rflags`

### x87 FPU registers

`st0`–`st7`

### MMX registers

`mm0`–`mm7`

### SSE registers

`xmm0`–`xmm15`

### ARM64 64-bit registers

`x0`–`x30`

### ARM64 32-bit registers

`w0`–`w30`

### ARM64 special registers

`sp` (stack pointer), `xzr` (zero register, 64-bit), `wzr` (zero register, 32-bit), `lr` (link register)

### Register usage notes

- Register names are **reserved** — you cannot use them as variable or function names.
- Registers have the type `register` in the type system.
- When targeting ARM64, using x86 register names will generate a compiler warning (the register will be mapped to an ARM64 equivalent automatically). The reverse (ARM64 registers on x86) generates a warning that the register is unavailable.

### Common pattern: reading a register value

```novus
mov(x9, some_value);
let result: u64 = getreg(x9);
```

---

## Address-of Operator

The `&` operator returns the memory address of a variable's stack slot:

```novus
let x: i32 = 42;
let addr: u64 = &x;   // addr holds the memory address of x
```

This is useful for passing pointers to system calls or other low-level operations.

---

## Escape Sequences

String and character literals support the following escape sequences:

| Escape | Character           |
|--------|---------------------|
| `\n`   | Newline (LF)        |
| `\r`   | Carriage return (CR)|
| `\t`   | Tab                 |
| `\\`   | Backslash           |
| `\'`   | Single quote        |
| `\"`   | Double quote        |
| `\0`   | Null byte           |

---

## Function Overloading

Novus supports **function overloading** — multiple functions can share the same name as long as they have different parameter type signatures.

```novus
fn itoa(n: i32) -> str {
    // convert i32 to string
}

fn itoa(n: i64) -> str {
    // convert i64 to string
}
```

The compiler resolves which overload to call based on the argument types at the call site:

```novus
let a: i32 = 42;
let b: i64 = 100;
itoa(a);  // calls itoa(i32)
itoa(b);  // calls itoa(i64)
```

Overloaded functions are **name-mangled** internally (e.g. `itoa.i32`, `itoa.i64`) to produce unique assembly labels.

---

## Global Variables

Variables declared at the top level (outside any function) are **global**:

```novus
let max_size: i32 = 1024;
let app_name: str = "MyApp";
let running: bool = true;
```

Global variables:
- Must have a type annotation and an initialiser.
- Can be initialised with compile-time constant expressions (integer literals, boolean literals, or string literals).
- Are accessible from any function in the module.
- Are stored in the data section of the binary.

---

## Target Platforms

Novus compiles to native assembly for the following target platforms:

| OS      | Architecture | Assembly syntax | Notes |
|---------|-------------|-----------------|-------|
| macOS   | ARM64 (Apple Silicon) | GAS (AT&T) | Mach-O object format, `_` symbol prefix |
| macOS   | x86_64 | GAS (AT&T) | Mach-O object format, `_` symbol prefix, `0x2000000` syscall offset |
| Linux   | x86_64 | GAS (AT&T) | ELF object format |
| Linux   | x86 (32-bit) | GAS (AT&T) | ELF object format |
| Linux   | ARM64 | GAS (AT&T) | ELF object format |
| Windows | x86_64 | NASM (Intel) | PE/COFF object format |

Cross-compilation is supported via the `--target` flag:

```
novus --target=linux/amd64 myfile.nov
novus --target=darwin/arm64 myfile.nov
novus --target=windows/amd64 myfile.nov
```

### Compiler flags

| Flag | Description |
|------|-------------|
| `--target=os/arch` | Cross-compile for the specified target (e.g. `linux/amd64`, `darwin/arm64`, `windows/amd64`) |
| `--asm-only` | Stop after emitting the assembly file (do not assemble or link) |
| `--skip-link` | Stop after assembling (produce `.o` object file but do not link) |
| `--debug` | Enable verbose debug output during compilation |

### Build output

Build artifacts are placed in `build/<os>_<arch>/`:

```
build/darwin_arm64/my_program      # executable
build/darwin_arm64/my_program.s    # assembly source
```

---

## Complete Example (No Standard Library)

This example demonstrates the core language features using only compiler built-in functions, targeting macOS Apple Silicon:

```novus
module hello;

// String length — walks bytes until the null terminator
fn len(s: str) -> i32 {
    let count: i32 = 0;
    let i: i32 = 0;
    while (s[i] != '\0') {
        count = count + 1;
        i = i + 1;
    }
    return count;
}

// Print a string to stdout using macOS ARM64 syscall
fn print(msg: str) -> void {
    msg = msg + "\n";
    mov(x0, 1);              // fd = 1 (stdout)
    mov(x1, msg);            // buffer pointer
    mov(x2, len(msg));       // buffer length
    mov(x16, 0x2000004);     // write syscall
    syscall();
}

// Exit the process
fn exit(code: i32) -> void {
    mov(x0, code);
    mov(x16, 0x2000001);     // exit syscall
    syscall();
}

fn main() -> i32 {
    print("Hello, World!");

    let count: i32 = 0;
    for (let i: i32 = 0; i < 5; i = i + 1) {
        count = count + 1;
    }

    // Array demo
    let nums: []i32 = [10, 20, 30];
    array_append(nums, 40);
    let last: i32 = array_pop(nums);

    // Memory demo
    let test_str: str = "ABCD";
    mov(x9, test_str);
    let addr: u64 = getreg(x9);
    let byte_val: i32 = load8(addr);  // 65 ('A')

    exit(0);
    return 0;
}
```

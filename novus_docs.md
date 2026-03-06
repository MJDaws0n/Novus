# Novus Language Documentation

A complete reference for the **Novus** systems programming language — a compiled systems language that produces native binaries. This guide covers everything from first install to low-level intrinsics. For standard library documentation, see the [Nox](https://github.com/mjdaws0n/nox) package manager, which ships the official standard libraries.

---

## Table of Contents

1. [Overview](#1-overview)
2. [Getting Started](#2-getting-started)
3. [Module System](#3-module-system)
4. [Types](#4-types)
5. [Variables](#5-variables)
6. [Functions](#6-functions)
7. [Operators](#7-operators)
8. [Control Flow](#8-control-flow)
9. [Strings](#9-strings)
10. [Arrays](#10-arrays)
11. [Global Variables](#11-global-variables)
12. [Conditional Compilation (#if)](#12-conditional-compilation-if)
13. [Garbage Collection](#13-garbage-collection)
14. [Low-Level Intrinsics](#14-low-level-intrinsics)
15. [Float Operations](#15-float-operations)
16. [Windows API Integration](#16-windows-api-integration)
17. [Target Platforms](#17-target-platforms)
18. [Nox Package Manager](#18-nox-package-manager)
19. [Standard Library Functions (via Nox)](#19-standard-library-functions-via-nox)
20. [Real-World Patterns](#20-real-world-patterns)
21. [Project Structure](#21-project-structure)
22. [Complete Example: CLI Tool](#22-complete-example-cli-tool)
23. [Compiler Flags](#23-compiler-flags)

---

## 1. Overview

Novus is a compiled systems programming language designed to produce fast, standalone native binaries with no runtime dependencies. It compiles directly to assembly (ARM64 or x86/x86_64 NASM), which is then assembled and linked into a platform-native executable.

**Key characteristics:**

- **Compiled to native code** — no interpreter, no VM, no bytecode. Novus produces real machine code.
- **Multi-platform** — targets macOS (ARM64), Linux (ARM64, x86_64, x86 32-bit), and Windows (x86_64).
- **Simple, explicit syntax** — no hidden control flow, no implicit conversions, no exceptions.
- **Built-in garbage collection** — a conservative mark-sweep GC handles heap memory automatically.
- **Low-level access** — inline intrinsics let you manipulate CPU registers, invoke syscalls, and work at the hardware level when needed.
- **Conditional compilation** — `#if` blocks let you write platform-specific code that is resolved at compile time.

### Compiling a Program

To compile a Novus source file:

```bash
novus main.nov
```

The compiler produces a native binary in the `build/<target>/` directory. For example, on macOS ARM64 the output would be placed in `build/darwin_arm64/`.

### Package Management

For dependency management, project scaffolding, and library installation, use the **Nox** package manager:

- Repository: [https://github.com/mjdaws0n/nox](https://github.com/mjdaws0n/nox)

Nox provides the official standard libraries (`std`, `file_io`, `process`, `net`, `http`, `maths`, `time`, `env`, `window`) and handles pulling third-party Novus libraries from Git repositories.

---

## 2. Getting Started

### Installation

Novus is written in Go. To build and install the compiler from source:

```bash
# Clone the repository
git clone https://github.com/mjdaws0n/novus.git
cd novus

# Build the compiler
go build -o novus cmd/novus/main.go

# Install system-wide
sudo mv novus /usr/local/bin/
```

After installation, verify the compiler is available:

```bash
novus --help
```

### Hello World

Create a file called `main.nov`:

```novus
module hello;

import std;

fn main() -> i32 {
    print("Hello, World!\n");
    return 0;
}
```

### Compiling and Running

```bash
# Compile
novus main.nov

# Run the resulting binary (macOS ARM64 example)
./build/darwin_arm64/hello
```

The compiler determines the output directory based on your target platform. On Apple Silicon Macs, the default target is `darwin_arm64`.

### Using Nox (Recommended)

Nox is the recommended way to manage Novus projects. It handles library paths, dependencies, and build configuration.

```bash
# Install Nox
go install github.com/mjdaws0n/nox@latest

# Create a new project
nox init myproject
cd myproject

# Pull the standard library
nox pull github.com/mjdaws0n/std

# Pull additional libraries
nox pull github.com/user/lib

# Build the project
nox build
```

Nox creates a `nox.json` file that tracks dependencies and build settings. When you run `nox build`, it resolves all library paths and invokes the Novus compiler with the correct configuration.

### Updating Dependencies

```bash
# Update all pulled libraries to their latest versions
nox update
```

---

## 3. Module System

Every Novus source file must begin with a module declaration. The module system controls how files find and reference each other.

### Module Declaration

The first statement in every `.nov` file must be a module declaration:

```novus
module my_program;
```

The module name is an identifier used to organize code. It does not need to match the filename, but keeping them consistent is recommended.

### Basic Import

Use `import` to bring another module into scope:

```novus
import path/to/module;
```

The compiler resolves the import path relative to the current file's directory and appends the `.nov` extension automatically. For example, `import lib/utils;` looks for `lib/utils.nov`.

```novus
module main;

import lib/helpers;

fn main() -> i32 {
    do_something();  // calls a function defined in lib/helpers.nov
    return 0;
}
```

### Import with Alias

You can give an imported module an alias to namespace its exports:

```novus
import lib/maths maths;
```

Then access its functions through the alias:

```novus
module main;

import lib/maths maths;

fn main() -> i32 {
    let result: i32 = maths.add(10, 20);
    return 0;
}
```

### Selective Imports

Import only specific symbols from a module using bracket syntax:

```novus
import lib/std[print, exit];
```

This brings only `print` and `exit` into scope, leaving other symbols from `std` inaccessible. This is useful for avoiding name collisions and making dependencies explicit.

```novus
module main;

import lib/std[print, exit];

fn main() -> i32 {
    print("Selective import example\n");
    exit(0);
    return 0;
}
```

### Directory Imports

When you import a directory path (rather than a file), the compiler looks for a `main.nov` file inside that directory:

```novus
import lib/maths;
```

This resolves to `lib/maths/main.nov`. This convention allows libraries to have a single entry point while organizing implementation across multiple files internally.

### Relative Path Imports

Use `../` to navigate up directories:

```novus
import ../shared/utils;
```

This is useful when your project has a shared library directory that multiple sub-modules need to access.

```novus
module handler;

import ../shared/utils;
import ../shared/config;

fn handle_request() -> i32 {
    let cfg: str = load_config();
    return 0;
}
```

### Hyphenated Names

Module paths can contain hyphens, which is common for multi-word library names:

```novus
import my-lib/http-client;
```

This resolves to `my-lib/http-client.nov`.

---

## 4. Types

Novus has a small, explicit type system. Every variable and function parameter must have a declared type. There is no implicit type coercion — you must use conversion functions to change between types.

### Primitive Types

| Type   | Description                        | Size           |
|--------|------------------------------------|----------------|
| `i32`  | 32-bit signed integer              | 4 bytes        |
| `i64`  | 64-bit signed integer              | 8 bytes        |
| `u64`  | 64-bit unsigned integer            | 8 bytes        |
| `f32`  | 32-bit floating-point number       | 4 bytes        |
| `f64`  | 64-bit floating-point number       | 8 bytes        |
| `str`  | String (pointer + length)          | 16 bytes (64-bit) |
| `bool` | Boolean (`true` or `false`)        | 1 byte (stored as integer) |
| `void` | No value (used for return types)   | 0 bytes        |

### Array Types

Array types are written as `[]` followed by the element type:

| Type      | Description               |
|-----------|---------------------------|
| `[]i32`   | Array of 32-bit integers  |
| `[]i64`   | Array of 64-bit integers  |
| `[]u64`   | Array of unsigned 64-bit integers |
| `[]str`   | Array of strings          |
| `[]bool`  | Array of booleans         |
| `[]f64`   | Array of 64-bit floats    |

### Type Requirements

- Every variable declaration requires a type annotation.
- Every function parameter requires a type annotation.
- Every function must declare its return type (or omit it for `void`).
- There is no type inference — you must always write the type explicitly.

```novus
// Correct — types are explicit
let count: i32 = 10;
let name: str = "Alice";
let values: []i32 = [1, 2, 3];

fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

### No Implicit Coercion

Novus does not automatically convert between types. To convert an `i32` to a `str`, you must call a conversion function:

```novus
let n: i32 = 42;
let s: str = i32_to_str(n);  // explicit conversion via std library
```

---

## 5. Variables

Variables in Novus are declared with the `let` keyword and must always include a type annotation and an initial value.

### Declaration Syntax

```
let name: type = value;
```

### Properties

- **All variables are mutable.** There is no `const` or `final` keyword — any variable can be reassigned after declaration.
- **Variables must be initialized.** You cannot declare a variable without assigning a value.
- **Block scoping.** Variables are scoped to the block `{ }` in which they are declared.

### Examples

```novus
// Integer types
let count: i32 = 0;
let big_number: i64 = 9999999999;
let address: u64 = 0;

// Floating-point types
let pi: f64 = 3.14159;
let temp: f32 = 98.6;

// String
let greeting: str = "Hello, Novus!";

// Boolean
let is_ready: bool = true;
let is_done: bool = false;

// Arrays
let numbers: []i32 = [10, 20, 30];
let names: []str = ["Alice", "Bob", "Charlie"];
let empty: []i32 = [];
```

### Reassignment

Variables can be reassigned at any time:

```novus
let x: i32 = 10;
x = 20;
x = x + 5;  // x is now 25
```

### Scope

Variables are only visible within the block where they are declared:

```novus
fn example() -> void {
    let outer: i32 = 1;

    if (outer == 1) {
        let inner: i32 = 2;
        // both outer and inner are accessible here
    }

    // inner is NOT accessible here
    // outer is still accessible
}
```

---

## 6. Functions

Functions are declared with the `fn` keyword. Every Novus program must have a `main` function as its entry point.

### Syntax

```
fn name(param1: type1, param2: type2) -> return_type {
    // body
}
```

### Basic Functions

```novus
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn greet(name: str) -> void {
    print("Hello, " + name + "!\n");
}

fn main() -> i32 {
    let sum: i32 = add(3, 7);
    greet("World");
    return 0;
}
```

### Void Functions

Functions that return nothing use the `void` return type. You can also omit the return type entirely for void functions:

```novus
// Explicit void return type
fn log_message(msg: str) -> void {
    print("[LOG] " + msg + "\n");
}

// Omitted return type (implicitly void)
fn log_warning(msg: str) {
    print("[WARN] " + msg + "\n");
}
```

### Return Values

Use `return` to return a value from a function. For functions with a non-void return type, every code path should have a `return` statement:

```novus
fn max(a: i32, b: i32) -> i32 {
    if (a > b) {
        return a;
    }
    return b;
}

fn is_positive(n: i32) -> bool {
    if (n > 0) {
        return true;
    }
    return false;
}
```

### Function Overloading

Novus supports function overloading — you can define multiple functions with the same name as long as they have different parameter types:

```novus
fn to_string(n: i32) -> str {
    return i32_to_str(n);
}

fn to_string(n: i64) -> str {
    return i64_to_str(n);
}

fn to_string(b: bool) -> str {
    return bool_to_str(b);
}

fn main() -> i32 {
    let a: str = to_string(42);       // calls i32 version
    let b: str = to_string(true);     // calls bool version
    print(a + "\n");
    print(b + "\n");
    return 0;
}
```

The compiler selects the correct overload based on the argument types at the call site.

### Calling Functions

Functions are called by name with arguments in parentheses:

```novus
let result: i32 = add(10, 20);
greet("Novus");
log_message("Application started");
```

---

## 7. Operators

Novus provides a standard set of operators for arithmetic, comparison, logic, bitwise manipulation, and assignment.

### Arithmetic Operators

| Operator | Description       | Example        |
|----------|-------------------|----------------|
| `+`      | Addition          | `a + b`        |
| `-`      | Subtraction       | `a - b`        |
| `*`      | Multiplication    | `a * b`        |
| `/`      | Division          | `a / b`        |
| `%`      | Modulo (remainder)| `a % b`        |

```novus
let a: i32 = 17;
let b: i32 = 5;

let sum: i32 = a + b;       // 22
let diff: i32 = a - b;      // 12
let prod: i32 = a * b;      // 85
let quot: i32 = a / b;      // 3 (integer division)
let rem: i32 = a % b;       // 2
```

### Comparison Operators

| Operator | Description           | Example        |
|----------|-----------------------|----------------|
| `==`     | Equal to              | `a == b`       |
| `!=`     | Not equal to          | `a != b`       |
| `<`      | Less than             | `a < b`        |
| `>`      | Greater than          | `a > b`        |
| `<=`     | Less than or equal    | `a <= b`       |
| `>=`     | Greater than or equal | `a >= b`       |

```novus
let x: i32 = 10;
let y: i32 = 20;

if (x < y) {
    print("x is less than y\n");
}

if (x != y) {
    print("x and y are different\n");
}
```

### Logical Operators

| Operator | Description | Example           |
|----------|-------------|-------------------|
| `&&`     | Logical AND | `a && b`          |
| `\|\|`   | Logical OR  | `a \|\| b`        |
| `!`      | Logical NOT | `!a`              |

```novus
let a: bool = true;
let b: bool = false;

if (a && !b) {
    print("a is true and b is false\n");
}

if (a || b) {
    print("at least one is true\n");
}
```

### Bitwise Operators

| Operator | Description      | Example        |
|----------|------------------|----------------|
| `&`      | Bitwise AND      | `a & b`        |
| `\|`     | Bitwise OR       | `a \| b`       |
| `^`      | Bitwise XOR      | `a ^ b`        |
| `<<`     | Left shift       | `a << n`       |
| `>>`     | Right shift      | `a >> n`       |

```novus
let flags: i32 = 0;
flags = flags | 4;       // set bit 2
flags = flags & 4;       // isolate bit 2
let shifted: i32 = 1 << 3;  // 8
```

### Assignment Operator

| Operator | Description | Example      |
|----------|-------------|--------------|
| `=`      | Assignment  | `x = 10;`   |

Novus uses `=` for assignment. There are no compound assignment operators (`+=`, `-=`, etc.) — use the expanded form instead:

```novus
let x: i32 = 10;
x = x + 5;   // instead of x += 5
x = x * 2;   // instead of x *= 2
```

### String Concatenation

The `+` operator is overloaded for strings to perform concatenation:

```novus
let first: str = "Hello";
let second: str = " World";
let full: str = first + second;  // "Hello World"
print(full + "!\n");             // "Hello World!"
```

---

## 8. Control Flow

Novus provides `if`/`else if`/`else` for branching, `while` loops, `for` loops, and `break`/`continue` for loop control.

### If / Else If / Else

```novus
fn classify(n: i32) -> str {
    if (n > 0) {
        return "positive";
    } else if (n < 0) {
        return "negative";
    } else {
        return "zero";
    }
}
```

Conditions must be enclosed in parentheses. The braces `{ }` are required.

```novus
let age: i32 = 25;

if (age >= 18) {
    print("Adult\n");
} else {
    print("Minor\n");
}
```

### Nested Conditions

```novus
fn check_credentials(user: str, role: str) -> bool {
    if (user == "admin") {
        if (role == "superuser") {
            return true;
        }
    }
    return false;
}
```

### While Loops

```novus
let i: i32 = 0;
while (i < 10) {
    print(i32_to_str(i) + "\n");
    i = i + 1;
}
```

While loops repeat as long as the condition is true. Here is an example that sums numbers:

```novus
fn sum_to(n: i32) -> i32 {
    let total: i32 = 0;
    let i: i32 = 1;
    while (i <= n) {
        total = total + i;
        i = i + 1;
    }
    return total;
}
```

### For Loops

Novus uses C-style for loops with three clauses — initialization, condition, and update:

```
for (init; condition; update) {
    // body
}
```

```novus
for (let i: i32 = 0; i < 10; i = i + 1) {
    print(i32_to_str(i) + "\n");
}
```

Note: there is no `++` or `--` operator. Use `i = i + 1` or `i = i - 1`.

```novus
// Print even numbers from 0 to 20
for (let i: i32 = 0; i <= 20; i = i + 2) {
    print(i32_to_str(i) + " ");
}
print("\n");
```

### Break and Continue

`break` exits the innermost loop immediately. `continue` skips the rest of the loop body and moves to the next iteration.

```novus
// Find the first number divisible by 7
for (let i: i32 = 1; i < 100; i = i + 1) {
    if (i % 7 == 0) {
        print("Found: " + i32_to_str(i) + "\n");
        break;
    }
}
```

```novus
// Print only odd numbers
for (let i: i32 = 0; i < 20; i = i + 1) {
    if (i % 2 == 0) {
        continue;
    }
    print(i32_to_str(i) + " ");
}
print("\n");
```

### Note on Switch/Match

Novus does not currently have a `switch` or `match` statement. Use `if`/`else if`/`else` chains instead:

```novus
fn day_name(day: i32) -> str {
    if (day == 0) {
        return "Sunday";
    } else if (day == 1) {
        return "Monday";
    } else if (day == 2) {
        return "Tuesday";
    } else if (day == 3) {
        return "Wednesday";
    } else if (day == 4) {
        return "Thursday";
    } else if (day == 5) {
        return "Friday";
    } else if (day == 6) {
        return "Saturday";
    } else {
        return "Unknown";
    }
}
```

---

## 9. Strings

Strings in Novus are sequences of bytes. They are stored as a pointer and a length, and are heap-allocated when created via concatenation.

### String Literals

String literals are enclosed in double quotes:

```novus
let greeting: str = "Hello, World!";
let empty: str = "";
let multiword: str = "Novus is a systems language";
```

### Escape Sequences

The following escape sequences are supported inside string literals:

| Escape   | Meaning             |
|----------|---------------------|
| `\n`     | Newline             |
| `\t`     | Tab                 |
| `\\`     | Backslash           |
| `\"`     | Double quote        |
| `\0`     | Null byte           |

```novus
print("Line one\nLine two\n");
print("Column1\tColumn2\n");
print("She said \"hello\"\n");
print("Path: C:\\Users\\novus\n");
```

### String Concatenation

Use the `+` operator to concatenate strings:

```novus
let first: str = "Hello";
let second: str = ", World!";
let full: str = first + second;
print(full + "\n");  // Hello, World!
```

Concatenation creates a new string on the heap. The garbage collector handles deallocation.

```novus
// Building a string in a loop
let result: str = "";
for (let i: i32 = 0; i < 5; i = i + 1) {
    result = result + i32_to_str(i) + " ";
}
print(result + "\n");  // 0 1 2 3 4
```

### String Indexing

You can index into a string with `s[i]`. This returns the byte value at position `i` as an `i32`:

```novus
let s: str = "ABC";
let first_byte: i32 = s[0];   // 65 (ASCII 'A')
let second_byte: i32 = s[1];  // 66 (ASCII 'B')
```

### String Length

Use the built-in `len()` function to get the length of a string in bytes:

```novus
let s: str = "Hello";
let length: i32 = len(s);  // 5
```

### String Comparison

Strings can be compared with `==` and `!=`:

```novus
let a: str = "hello";
let b: str = "hello";
let c: str = "world";

if (a == b) {
    print("a and b are equal\n");
}

if (a != c) {
    print("a and c are different\n");
}
```

### Working with Characters

Since string indexing returns byte values, you can compare individual characters by their ASCII codes or by using helper functions:

```novus
let s: str = "Hello";
let first: i32 = s[0];

if (first == 72) {  // ASCII 'H'
    print("Starts with H\n");
}
```

---

## 10. Arrays

Arrays in Novus are dynamically-sized, heap-allocated collections of elements of a single type.

### Array Literals

Create an array with initial values:

```novus
let numbers: []i32 = [1, 2, 3, 4, 5];
let names: []str = ["Alice", "Bob", "Charlie"];
let flags: []bool = [true, false, true];
```

### Empty Arrays

Create an empty array (with no initial elements):

```novus
let items: []i32 = [];
let words: []str = [];
```

### Indexing

Access elements by index (zero-based). Use the same syntax for reading and writing:

```novus
let arr: []i32 = [10, 20, 30];

// Read
let first: i32 = arr[0];   // 10
let second: i32 = arr[1];  // 20

// Write
arr[2] = 99;  // arr is now [10, 20, 99]
```

### Append

Add an element to the end of an array with `array_append`:

```novus
let items: []i32 = [];
array_append(items, 10);
array_append(items, 20);
array_append(items, 30);
// items is now [10, 20, 30]
```

### Pop

Remove and return the last element with `array_pop`:

```novus
let stack: []i32 = [1, 2, 3];
let top: i32 = array_pop(stack);  // top = 3, stack = [1, 2]
```

### Length

Use the built-in `len()` function to get the number of elements:

```novus
let arr: []i32 = [10, 20, 30];
let size: i32 = len(arr);  // 3
```

### Iterating Over Arrays

```novus
let names: []str = ["Alice", "Bob", "Charlie"];
for (let i: i32 = 0; i < len(names); i = i + 1) {
    print(names[i] + "\n");
}
```

### Array as Function Parameters

Arrays can be passed to functions:

```novus
fn sum(arr: []i32) -> i32 {
    let total: i32 = 0;
    for (let i: i32 = 0; i < len(arr); i = i + 1) {
        total = total + arr[i];
    }
    return total;
}

fn main() -> i32 {
    let numbers: []i32 = [1, 2, 3, 4, 5];
    let result: i32 = sum(numbers);
    print("Sum: " + i32_to_str(result) + "\n");  // Sum: 15
    return 0;
}
```

### Memory Layout

Arrays are represented in memory as a three-field structure:

```
[data_ptr | len | cap]
```

| Field      | Description                          | Size (64-bit) | Size (32-bit) |
|------------|--------------------------------------|---------------|---------------|
| `data_ptr` | Pointer to the element data on the heap | 8 bytes    | 4 bytes       |
| `len`      | Current number of elements           | 8 bytes       | 4 bytes       |
| `cap`      | Allocated capacity                   | 8 bytes       | 4 bytes       |

- **Total size:** 24 bytes on 64-bit platforms, 12 bytes on 32-bit platforms.
- When `len` exceeds `cap`, the array is reallocated with a larger capacity (typically doubled).
- The data pointer is tracked by the garbage collector.

---

## 11. Global Variables

Variables declared outside of any function are global variables. They are accessible from all functions within the same module.

### Declaration

```novus
module counter;

import std;

let count: i32 = 0;
let app_name: str = "MyApp";

fn increment() -> void {
    count = count + 1;
}

fn get_count() -> i32 {
    return count;
}

fn main() -> i32 {
    increment();
    increment();
    increment();
    print(app_name + " count: " + i32_to_str(get_count()) + "\n");
    return 0;
}
```

### Initialization

Global variables can be initialized with:

- **Integer literals:** `let max_size: i32 = 1024;`
- **String literals:** `let version: str = "1.0.0";`
- **Boolean literals:** `let debug: bool = false;`

```novus
let max_retries: i32 = 3;
let api_url: str = "https://example.com/api";
let verbose: bool = false;
```

### Use Cases

Global variables are useful for:

- Configuration constants
- Shared state across functions
- Counters and accumulators
- Module-level metadata (version strings, names)

```novus
module config;

let version: str = "2.1.0";
let max_connections: i32 = 100;
let debug_mode: bool = false;

fn print_version() -> void {
    print("Version: " + version + "\n");
}
```

---

## 12. Conditional Compilation (#if)

Novus supports compile-time conditional compilation using `#if` blocks. These are evaluated at compile time — code inside a block whose condition is false is completely excluded from the generated assembly.

### Syntax

```novus
#if(os == "value") {
    // code included only when compiling for this OS
}
```

### Supported Conditions

Currently, the only supported condition is `os`, which matches the target operating system:

| Value       | Platform         |
|-------------|------------------|
| `"darwin"`  | macOS            |
| `"windows"` | Windows          |
| `"linux"`   | Linux            |

### What Can Appear Inside #if

`#if` blocks can contain:

- Import statements
- Function definitions
- Global variable declarations

### Platform-Specific Function Implementations

A common pattern is to provide different implementations of the same function for each platform:

```novus
module paths;

#if(os == "darwin") {
    fn get_config_path() -> str {
        return "/Users/Shared/myapp/config.json";
    }

    fn get_temp_dir() -> str {
        return "/tmp";
    }
}

#if(os == "windows") {
    fn get_config_path() -> str {
        return "C:\\ProgramData\\myapp\\config.json";
    }

    fn get_temp_dir() -> str {
        return "C:\\Temp";
    }
}

#if(os == "linux") {
    fn get_config_path() -> str {
        return "/etc/myapp/config.json";
    }

    fn get_temp_dir() -> str {
        return "/tmp";
    }
}
```

This approach ensures that only one version of each function is compiled into the final binary.

### Library Loader Pattern

Libraries that have platform-specific assembly implementations use `#if` to load the correct backend:

```novus
module file_io;

#if(os == "darwin") {
    import darwin_arm64;
}

#if(os == "windows") {
    import windows_amd64;
}

#if(os == "linux") {
    import linux;
}
```

Each imported file (`darwin_arm64.nov`, `windows_amd64.nov`, `linux.nov`) contains the same set of function signatures but with platform-specific implementations using intrinsics and syscalls.

### Platform-Specific Imports

```novus
module my_app;

import std;

#if(os == "darwin") {
    import lib/macos_helpers;
}

#if(os == "linux") {
    import lib/linux_helpers;
}

fn main() -> i32 {
    setup();  // defined in platform-specific import
    print("Application started\n");
    return 0;
}
```

### Conditional Global Variables

```novus
#if(os == "darwin") {
    let line_ending: str = "\n";
}

#if(os == "windows") {
    let line_ending: str = "\r\n";
}
```

---

## 13. Garbage Collection

Novus includes a built-in conservative mark-sweep garbage collector that automatically manages heap memory. You do not need to manually free memory in most cases.

### How It Works

1. **Allocation tracking:** Every heap allocation (arrays, string concatenation results) is registered with the GC.
2. **Mark phase:** When GC runs, it scans the stack and global variables conservatively, looking for values that could be pointers to tracked allocations. Every reachable allocation is marked as "in use."
3. **Sweep phase:** Any allocation not marked as reachable is freed. Its memory is added to a free list for reuse.
4. **Free list reuse:** When new allocations are requested, the GC first checks the free list for a suitably sized block before requesting new memory from the OS.

### When GC Runs

The garbage collector runs automatically when allocation pressure reaches an internal threshold. You do not need to configure this — it happens transparently during normal program execution.

### Manual GC Trigger

You can manually trigger a garbage collection cycle using the built-in `gc_collect()` function:

```novus
fn process_data() -> void {
    // ... create many temporary strings and arrays ...
    gc_collect();  // force cleanup of unreachable allocations
}
```

This is rarely needed, but can be useful in long-running programs or after processing large batches of temporary data.

### Conservative Scanning

The GC is "conservative" — it scans raw memory values on the stack and in globals, treating any value that looks like a valid heap pointer as a potential reference. This means:

- **No pointer tagging required.** The GC works without any special annotations or type information at runtime.
- **False positives are possible** (an integer that happens to look like a heap address may keep an allocation alive longer than necessary), but this is harmless — it only affects when memory is freed, not correctness.

### What Is GC-Managed

- **String concatenation results:** `let c: str = a + b;` — the new string is heap-allocated and GC-tracked.
- **Arrays:** All array data is heap-allocated and GC-tracked.
- **Values created by intrinsics** that allocate heap memory.

### What Is NOT GC-Managed

- **Stack variables:** Automatically freed when the function returns.
- **String literals:** Stored in the data section of the binary, never freed.
- **Global variables:** Live for the entire program duration.

```novus
fn example() -> void {
    // These string concatenations create heap allocations tracked by GC
    let msg: str = "Hello" + " " + "World";
    let repeated: str = "";
    for (let i: i32 = 0; i < 1000; i = i + 1) {
        repeated = repeated + "x";
    }
    // When 'repeated' and 'msg' go out of scope and are no longer
    // reachable, GC will eventually reclaim their memory.
}
```

---

## 14. Low-Level Intrinsics

Novus provides a set of built-in intrinsic functions that give direct access to CPU registers, memory, and system calls. These are used primarily by standard library implementations and for systems programming tasks.

### Register Operations

#### `mov(reg, value)` — Move Value into Register

Loads a value into the specified CPU register:

```novus
mov(x0, 1);       // ARM64: move 1 into x0
mov(rax, 60);     // x86_64: move 60 into rax
```

#### `setreg(reg, value)` — Set Register

Alternative way to set a register value:

```novus
setreg(x0, 42);
```

#### `getreg(reg)` → u64 — Read Register Value

Reads the current value of a CPU register:

```novus
let val: u64 = getreg(x0);
let stack_ptr: u64 = getreg(sp);
```

### Stack Operations

#### `push(value)` — Push onto Hardware Stack

Pushes a value onto the hardware stack:

```novus
push(42);
push(getreg(x0));
```

#### `pop()` → u64 — Pop from Hardware Stack

Pops a value from the hardware stack:

```novus
let val: u64 = pop();
```

### System Calls

#### `syscall()` — Invoke System Call

Triggers a system call. The syscall number and arguments must be placed in the appropriate registers beforehand:

```novus
// ARM64 macOS: write(1, buf, len)
mov(x16, 4);        // syscall number for write on macOS ARM64
mov(x0, 1);         // fd = stdout
mov(x1, ptr(msg));  // buffer address
mov(x2, len(msg));  // buffer length
syscall();
```

#### `int(n)` — Software Interrupt (x86)

Triggers a software interrupt, used on x86 for legacy system calls:

```novus
// x86 Linux: int 0x80 for system call
mov(eax, 4);     // sys_write
mov(ebx, 1);     // stdout
int(0x80);
```

### CPU Flags

#### `setflag(flag, value)` — Set CPU Flag

Sets a CPU flag:

```novus
setflag(zero, true);
setflag(carry, false);
```

#### `getflag(flag)` → bool — Read CPU Flag

Reads the current state of a CPU flag:

```novus
let is_zero: bool = getflag(zero);
let has_carry: bool = getflag(carry);
```

### Memory Operations

#### `load8(addr)` → i32 — Load Byte

Loads a single byte from a memory address:

```novus
let byte_val: i32 = load8(some_address);
```

#### `load32(addr)` → i32 — Load 32-bit Value

Loads a 32-bit value from a memory address:

```novus
let word: i32 = load32(some_address);
```

#### `load64(addr)` → i64 — Load 64-bit Value

Loads a 64-bit value from a memory address:

```novus
let qword: i64 = load64(some_address);
```

#### `lea(dst, src)` — Load Effective Address

Computes an address and stores it in a register without accessing memory:

```novus
lea(x0, some_label);
```

### Pointer Operations

#### `ptr(value)` → u64 — Get Address

Returns the memory address of a value. Works with strings and arrays:

```novus
let msg: str = "Hello";
let addr: u64 = ptr(msg);   // address of the string data
```

### Control Flow Intrinsics

#### `call(label)` — Call Assembly Label

Calls a named assembly label:

```novus
call(my_asm_function);
```

#### `ret()` — Return from Call

Returns from the current function at the assembly level:

```novus
ret();
```

#### `nop()` — No Operation

Does nothing. Useful for alignment or as a placeholder:

```novus
nop();
```

### GC Intrinsic

#### `gc_collect()` — Manual Garbage Collection

Manually triggers a garbage collection cycle:

```novus
gc_collect();
```

### Windows-Specific

#### `win_call(func_name, args...)` — Call Windows API

Calls a Windows API function by name (see [Section 16](#16-windows-api-integration) for details):

```novus
win_call("ExitProcess", 0);
```

### Register Names

**ARM64 registers:**

| Register    | Description              |
|-------------|--------------------------|
| `x0` – `x28` | General-purpose registers |
| `sp`        | Stack pointer            |
| `lr`        | Link register (return address) |
| `xzr`       | Zero register (always 0) |

**x86_64 registers:**

| Register    | Description              |
|-------------|--------------------------|
| `rax`       | Accumulator              |
| `rbx`       | Base                     |
| `rcx`       | Counter                  |
| `rdx`       | Data                     |
| `rsi`       | Source index             |
| `rdi`       | Destination index        |
| `rsp`       | Stack pointer            |
| `rbp`       | Base pointer             |
| `r8` – `r15` | Additional general-purpose |

### CPU Flags

| Flag       | Description                                  |
|------------|----------------------------------------------|
| `zero`     | Set when the result of an operation is zero   |
| `carry`    | Set on unsigned overflow                      |
| `negative` | Set when the result is negative               |
| `overflow` | Set on signed overflow                        |

### Example: Low-Level String Output (ARM64 macOS)

```novus
module raw_io;

fn raw_print(msg: str) -> void {
    mov(x16, 4);           // macOS ARM64 write syscall
    mov(x0, 1);            // stdout
    mov(x1, ptr(msg));     // string data pointer
    mov(x2, len(msg));     // string length
    syscall();
}

fn main() -> i32 {
    raw_print("Hello from raw syscall!\n");
    return 0;
}
```

### Example: Reading a Byte from a String

```novus
fn first_byte(s: str) -> i32 {
    let addr: u64 = ptr(s);
    let byte_val: i32 = load8(addr);
    return byte_val;
}

fn main() -> i32 {
    let ch: i32 = first_byte("ABC");  // 65 (ASCII 'A')
    return 0;
}
```

---

## 15. Float Operations

Novus provides built-in functions for working with floating-point numbers, including conversion between integer and float types, and bit-level reinterpretation.

### Conversion Functions

#### `i64_to_f64(n)` → f64 — Integer to Float

Converts a 64-bit integer to a 64-bit float:

```novus
let n: i64 = 42;
let f: f64 = i64_to_f64(n);  // 42.0
```

#### `f64_to_i64(f)` → i64 — Float to Integer (Truncate)

Converts a 64-bit float to a 64-bit integer by truncating toward zero:

```novus
let f: f64 = 3.99;
let n: i64 = f64_to_i64(f);  // 3 (truncated, not rounded)
```

### Bit-Level Reinterpretation

#### `f64_bits(f)` → i64 — Float Bits as Integer

Reinterprets the raw IEEE 754 bits of a float as an integer (no conversion — the bits are preserved):

```novus
let f: f64 = 1.0;
let bits: i64 = f64_bits(f);  // 4607182418800017408 (0x3FF0000000000000)
```

#### `f64_from_bits(n)` → f64 — Integer Bits as Float

Reinterprets an integer's bits as a float:

```novus
let bits: i64 = 4607182418800017408;
let f: f64 = f64_from_bits(bits);  // 1.0
```

### Float Arithmetic

Float values support the standard arithmetic operators:

```novus
let a: f64 = 3.14;
let b: f64 = 2.0;

let sum: f64 = a + b;    // 5.14
let diff: f64 = a - b;   // 1.14
let prod: f64 = a * b;   // 6.28
let quot: f64 = a / b;   // 1.57
```

### Example: Computing Circle Area

```novus
module circle;

import std;

fn circle_area(radius: f64) -> f64 {
    let pi: f64 = 3.14159265358979;
    return pi * radius * radius;
}

fn main() -> i32 {
    let r: f64 = i64_to_f64(5);
    let area: f64 = circle_area(r);
    // area is approximately 78.5398
    return 0;
}
```

### Example: Temperature Conversion

```novus
fn celsius_to_fahrenheit(c: f64) -> f64 {
    return c * 1.8 + 32.0;
}

fn fahrenheit_to_celsius(f: f64) -> f64 {
    return (f - 32.0) / 1.8;
}
```

---

## 16. Windows API Integration

On Windows targets, Novus provides the `win_call` intrinsic for calling Windows API (Win32) functions directly from Novus code.

### Syntax

```novus
win_call("FunctionName", arg1, arg2, ...);
```

The first argument is a string literal naming the Windows API function. Subsequent arguments are passed as the function's parameters.

### Example: Message Box

```novus
module win_demo;

fn main() -> i32 {
    let msg: str = "Hello from Novus!";
    let title: str = "Novus";
    win_call("MessageBoxA", 0, ptr(msg), ptr(title), 0);
    return 0;
}
```

### Example: Exit Process

```novus
win_call("ExitProcess", 0);
```

### How It Works

- The Novus compiler emits NASM assembly that declares external Win32 functions using the `extern` directive.
- Arguments are placed in the correct registers following the Windows x64 calling convention (RCX, RDX, R8, R9, then stack).
- The linker (GoLink) resolves these external symbols against Windows system DLLs (`kernel32.dll`, `user32.dll`, etc.).

### Entry Point on Windows

On Windows, `fn main()` is wrapped by the compiler. The runtime obtains `argc` and `argv` automatically via the `__getmainargs` function from the C runtime, so command-line argument handling works transparently.

### External Functions

When the compiler encounters `win_call`, it generates `extern` declarations in the NASM output. For example:

```nasm
extern MessageBoxA
extern ExitProcess
extern __getmainargs
```

These are resolved at link time by GoLink against the appropriate Windows DLLs.

### Common Windows API Functions

| Function         | DLL          | Description                    |
|------------------|--------------|--------------------------------|
| `MessageBoxA`    | user32.dll   | Display a message box          |
| `ExitProcess`    | kernel32.dll | Terminate the process          |
| `GetStdHandle`   | kernel32.dll | Get standard I/O handle        |
| `WriteFile`      | kernel32.dll | Write to a file or console     |
| `ReadFile`       | kernel32.dll | Read from a file or console    |
| `CreateFileA`    | kernel32.dll | Open or create a file          |
| `CloseHandle`    | kernel32.dll | Close a handle                 |
| `GetLastError`   | kernel32.dll | Get the last error code        |

---

## 17. Target Platforms

Novus supports cross-compilation to multiple operating systems and CPU architectures from a single machine.

### Supported Targets

| OS      | Architecture | Flag Values                       | Assembler | Linker     |
|---------|-------------|-----------------------------------|-----------|------------|
| macOS   | ARM64       | `--os darwin --arch arm64`        | as        | ld         |
| Linux   | ARM64       | `--os linux --arch arm64`         | as        | ld         |
| Linux   | x86_64      | `--os linux --arch amd64`         | nasm      | ld         |
| Linux   | x86 (32-bit)| `--os linux --arch 386`           | nasm      | ld         |
| Windows | x86_64      | `--os windows --arch amd64`       | nasm      | GoLink     |

### Default Target

When no flags are specified, the compiler targets the host platform. On an Apple Silicon Mac, the default is `darwin arm64`. On a Linux x86_64 machine, the default is `linux amd64`.

### Cross-Compilation

To cross-compile, specify the target OS and architecture:

```bash
# Compile for Linux x86_64 from any platform
novus --os linux --arch amd64 main.nov

# Compile for Windows x86_64
novus --os windows --arch amd64 main.nov

# Compile for Linux ARM64
novus --os linux --arch arm64 main.nov

# Compile for Linux 32-bit x86
novus --os linux --arch 386 main.nov
```

### Output Directory

The compiled binary is placed in a directory named after the target:

```
build/<os>_<arch>/<binary_name>
```

Examples:

```
build/darwin_arm64/myapp
build/linux_amd64/myapp
build/windows_amd64/myapp.exe
build/linux_386/myapp
build/linux_arm64/myapp
```

### Platform-Specific Considerations

**macOS ARM64:**
- Uses the system assembler (`as`) and linker (`ld`).
- Generates ARM64 assembly.
- Default target on Apple Silicon Macs.

**Linux x86_64 / x86:**
- Uses NASM as the assembler.
- Generates NASM-syntax x86/x86_64 assembly.
- x86 (386) target produces 32-bit ELF binaries.

**Linux ARM64:**
- Uses the system assembler (`as`) and linker (`ld`).
- Generates ARM64 assembly (same instruction set as macOS ARM64, different syscall conventions).

**Windows x86_64:**
- Uses NASM as the assembler and GoLink as the linker.
- Generates NASM-syntax x86_64 assembly with Windows calling conventions.
- Produces PE executables (`.exe`).

---

## 18. Nox Package Manager

**Nox** is the official package manager for Novus. It handles project initialization, dependency management, building, and library distribution.

- **Repository:** [https://github.com/mjdaws0n/nox](https://github.com/mjdaws0n/nox)

### Installation

```bash
go install github.com/mjdaws0n/nox@latest
```

### Initialize a Project

```bash
nox init myproject
cd myproject
```

This creates a project directory with a `nox.json` configuration file and a starter `main.nov`.

### Install a Library

```bash
nox pull github.com/user/library
```

This clones the library into the project's `lib/` directory and records it in `nox.json`.

### Build

```bash
nox build
```

Nox resolves all library paths and invokes the Novus compiler with the correct configuration. The output binary is placed in the `build/` directory.

### Update Dependencies

```bash
nox update
```

Updates all pulled libraries to their latest versions from their Git repositories.

### Standard Libraries Available via Nox

The following official libraries are available:

| Library    | Description                                  |
|------------|----------------------------------------------|
| `std`      | Core I/O, type conversions, string utilities |
| `file_io`  | File reading, writing, and operations        |
| `process`  | Subprocess execution, command running        |
| `net`      | TCP sockets and networking                   |
| `http`     | HTTP client and server                       |
| `maths`    | Mathematical functions                       |
| `time`     | Time operations and measurement              |
| `env`      | Environment variable access                  |
| `window`   | GUI window creation (macOS and Windows)      |

### Example Workflow

```bash
# Create a new project
nox init webapp
cd webapp

# Pull required libraries
nox pull github.com/mjdaws0n/std
nox pull github.com/mjdaws0n/http
nox pull github.com/mjdaws0n/file_io

# Edit main.nov
# ...

# Build and run
nox build
./build/darwin_arm64/webapp
```

---

## 19. Standard Library Functions (via Nox)

The `std` library (installed via `nox pull`) provides essential I/O, type conversion, and string manipulation functions. Import it with:

```novus
import std;
```

### I/O Functions

#### `print(msg: str) -> void`

Prints a string to standard output (stdout). Does not add a newline — include `\n` in the string if needed:

```novus
print("Hello, World!\n");
print("No newline here");
```

#### `print_raw(msg: str) -> void`

Prints a string to standard output without any processing:

```novus
print_raw("raw output");
```

#### `input(prompt: str) -> str`

Displays a prompt and reads a line of input from the user:

```novus
let name: str = input("Enter your name: ");
print("Hello, " + name + "!\n");
```

#### `exit(code: i32) -> void`

Terminates the program with the given exit code:

```novus
if (error_occurred) {
    print("Fatal error\n");
    exit(1);
}
```

### Type Conversion Functions

#### `str_to_i32(s: str) -> i32`

Parses a string as a 32-bit integer:

```novus
let n: i32 = str_to_i32("42");  // 42
```

#### `i32_to_str(n: i32) -> str`

Converts a 32-bit integer to its string representation:

```novus
let s: str = i32_to_str(42);  // "42"
```

#### `i64_to_str(n: i64) -> str`

Converts a 64-bit integer to its string representation:

```novus
let s: str = i64_to_str(9999999999);  // "9999999999"
```

#### `bool_to_str(b: bool) -> str`

Converts a boolean to `"true"` or `"false"`:

```novus
let s: str = bool_to_str(true);   // "true"
let t: str = bool_to_str(false);  // "false"
```

### String Utility Functions

#### `starts_with(s: str, prefix: str) -> bool`

Checks if a string starts with the given prefix:

```novus
let result: bool = starts_with("hello world", "hello");  // true
let other: bool = starts_with("hello world", "world");   // false
```

#### `ends_with(s: str, suffix: str) -> bool`

Checks if a string ends with the given suffix:

```novus
let result: bool = ends_with("hello.nov", ".nov");  // true
```

#### `str_contains(s: str, needle: str) -> bool`

Checks if a string contains a substring:

```novus
let found: bool = str_contains("hello world", "world");  // true
let nope: bool = str_contains("hello world", "xyz");      // false
```

#### `str_find(s: str, needle: str) -> i32`

Returns the index of the first occurrence of `needle` in `s`, or `-1` if not found:

```novus
let idx: i32 = str_find("hello world", "world");  // 6
let none: i32 = str_find("hello world", "xyz");   // -1
```

#### `substr(s: str, start: i32) -> str`

Returns a substring from `start` to the end of the string:

```novus
let tail: str = substr("hello world", 6);  // "world"
```

#### `substr_len(s: str, start: i32, count: i32) -> str`

Returns a substring of `count` characters starting at `start`:

```novus
let part: str = substr_len("hello world", 0, 5);  // "hello"
```

#### `str_replace(s: str, old: str, new_val: str) -> str`

Replaces all occurrences of `old` with `new_val`:

```novus
let result: str = str_replace("hello world", "world", "novus");
// result is "hello novus"
```

#### `str_trim(s: str) -> str`

Removes leading and trailing whitespace:

```novus
let trimmed: str = str_trim("  hello  ");  // "hello"
```

#### `str_upper(s: str) -> str`

Converts a string to uppercase:

```novus
let upper: str = str_upper("hello");  // "HELLO"
```

#### `str_lower(s: str) -> str`

Converts a string to lowercase:

```novus
let lower: str = str_lower("HELLO");  // "hello"
```

---

## 20. Real-World Patterns

This section demonstrates practical patterns used in real Novus projects, including the Nox package manager itself.

### Platform-Specific Implementations with #if

When building cross-platform libraries, use `#if` blocks with a loader module:

```novus
// lib/mylib/main.nov — the loader
module mylib;

#if(os == "darwin") {
    import darwin_arm64;
}

#if(os == "windows") {
    import windows_amd64;
}

#if(os == "linux") {
    import linux_amd64;
}
```

Each platform file exports the same set of functions with platform-specific implementations. The calling code simply imports `lib/mylib` and calls functions by name.

### String Parsing

Parse structured text (CSV, config files) using loops and character comparison:

```novus
fn parse_csv_line(line: str) -> []str {
    let fields: []str = [];
    let current: str = "";
    let i: i32 = 0;

    while (i < len(line)) {
        let ch: i32 = line[i];
        if (ch == 44) {  // ASCII comma
            array_append(fields, current);
            current = "";
        } else {
            current = current + substr_len(line, i, 1);
        }
        i = i + 1;
    }
    array_append(fields, current);
    return fields;
}
```

### Command-Line Argument Processing

Process `argv` to handle flags and positional arguments:

```novus
module cli;

import std;

fn main() -> i32 {
    let args: []str = argv();
    let verbose: bool = false;
    let filename: str = "";

    for (let i: i32 = 1; i < len(args); i = i + 1) {
        if (args[i] == "--verbose") {
            verbose = true;
        } else if (args[i] == "--file") {
            i = i + 1;
            if (i < len(args)) {
                filename = args[i];
            }
        } else {
            print("Unknown argument: " + args[i] + "\n");
            exit(1);
        }
    }

    if (filename == "") {
        print("Usage: cli --file <path> [--verbose]\n");
        exit(1);
    }

    if (verbose) {
        print("Processing: " + filename + "\n");
    }

    return 0;
}
```

### Building Argument Arrays for Subprocess Calls

When using the `process` library to run external commands, build argument arrays with `ptr()` for passing to system calls:

```novus
fn run_compiler(source: str, output: str) -> void {
    let args: []str = [];
    array_append(args, "gcc");
    array_append(args, "-o");
    array_append(args, output);
    array_append(args, source);
    run_cmd(args);
}
```

### Error Handling with Status Codes

Novus does not have exceptions. Use return codes and early returns for error handling:

```novus
fn parse_port(s: str) -> i32 {
    let port: i32 = str_to_i32(s);
    if (port < 1) {
        print("Error: invalid port number\n");
        return -1;
    }
    if (port > 65535) {
        print("Error: port out of range\n");
        return -1;
    }
    return port;
}

fn main() -> i32 {
    let port: i32 = parse_port("8080");
    if (port == -1) {
        exit(1);
    }
    print("Using port: " + i32_to_str(port) + "\n");
    return 0;
}
```

### Version String Parsing

Parse a semantic version string like `"1.2.3"`:

```novus
fn parse_major_version(version: str) -> i32 {
    let dot_pos: i32 = str_find(version, ".");
    if (dot_pos == -1) {
        return str_to_i32(version);
    }
    let major_str: str = substr_len(version, 0, dot_pos);
    return str_to_i32(major_str);
}

fn is_compatible(required: str, actual: str) -> bool {
    let req_major: i32 = parse_major_version(required);
    let act_major: i32 = parse_major_version(actual);
    return req_major == act_major;
}
```

---

## 21. Project Structure

### Recommended Layout

```
myproject/
├── main.nov            # Entry point
├── nox.json            # Nox package manager configuration
└── lib/                # Libraries (pulled via nox or local)
    ├── std/            # Standard library
    │   ├── main.nov    # Loader (uses #if for platform dispatch)
    │   ├── darwin_arm64.nov
    │   ├── windows_amd64.nov
    │   └── linux.nov
    ├── file_io/
    │   ├── main.nov
    │   ├── darwin_arm64.nov
    │   ├── windows_amd64.nov
    │   └── linux.nov
    └── mylib/          # Your own library
        ├── main.nov
        ├── darwin_arm64.nov
        ├── windows_amd64.nov
        └── linux.nov
```

### Library Structure

Each library follows the same pattern:

1. **`main.nov`** — the entry point / loader that uses `#if` to import the correct platform implementation.
2. **Platform files** — one file per supported platform containing the actual function implementations.

```novus
// lib/mylib/main.nov
module mylib;

#if(os == "darwin") {
    import darwin_arm64;
}

#if(os == "windows") {
    import windows_amd64;
}

#if(os == "linux") {
    import linux;
}
```

### Entry Point

The `main.nov` file at the project root is the program entry point. It should contain the `main` function:

```novus
module myproject;

import lib/std std;
import lib/mylib mylib;

fn main() -> i32 {
    std.print("Starting myproject\n");
    mylib.do_work();
    return 0;
}
```

### Nox Configuration

The `nox.json` file tracks dependencies and build settings:

```json
{
    "name": "myproject",
    "version": "1.0.0",
    "entry": "main.nov",
    "dependencies": [
        "github.com/mjdaws0n/std",
        "github.com/mjdaws0n/file_io"
    ]
}
```

---

## 22. Complete Example: CLI Tool

Here is a complete, practical CLI tool written in Novus. It reads command-line arguments, processes text, and demonstrates multiple language features.

```novus
module wordcount;

import std;
import file_io;

// Global configuration
let verbose: bool = false;
let show_help: bool = false;

// Count the number of words in a string by counting space-separated tokens
fn count_words(text: str) -> i32 {
    let count: i32 = 0;
    let in_word: bool = false;
    let i: i32 = 0;

    while (i < len(text)) {
        let ch: i32 = text[i];
        if (ch == 32 || ch == 10 || ch == 9 || ch == 13) {
            // space, newline, tab, carriage return
            if (in_word) {
                count = count + 1;
                in_word = false;
            }
        } else {
            in_word = true;
        }
        i = i + 1;
    }

    // Count the last word if text doesn't end with whitespace
    if (in_word) {
        count = count + 1;
    }

    return count;
}

// Count lines in a string
fn count_lines(text: str) -> i32 {
    let count: i32 = 0;
    let i: i32 = 0;
    while (i < len(text)) {
        if (text[i] == 10) {
            count = count + 1;
        }
        i = i + 1;
    }
    return count;
}

// Print usage information
fn print_usage() -> void {
    print("Usage: wordcount [options] <file>\n");
    print("\n");
    print("Options:\n");
    print("  --help       Show this help message\n");
    print("  --verbose    Show detailed output\n");
    print("  --words      Count words only\n");
    print("  --lines      Count lines only\n");
    print("\n");
    print("Examples:\n");
    print("  wordcount document.txt\n");
    print("  wordcount --verbose report.txt\n");
}

// Get the platform name for display
#if(os == "darwin") {
    fn get_platform() -> str {
        return "macOS";
    }
}

#if(os == "linux") {
    fn get_platform() -> str {
        return "Linux";
    }
}

#if(os == "windows") {
    fn get_platform() -> str {
        return "Windows";
    }
}

fn main() -> i32 {
    let args: []str = argv();
    let filename: str = "";
    let words_only: bool = false;
    let lines_only: bool = false;

    // Parse command-line arguments
    for (let i: i32 = 1; i < len(args); i = i + 1) {
        if (args[i] == "--help") {
            show_help = true;
        } else if (args[i] == "--verbose") {
            verbose = true;
        } else if (args[i] == "--words") {
            words_only = true;
        } else if (args[i] == "--lines") {
            lines_only = true;
        } else if (starts_with(args[i], "--")) {
            print("Error: unknown option '" + args[i] + "'\n");
            exit(1);
        } else {
            filename = args[i];
        }
    }

    if (show_help) {
        print_usage();
        return 0;
    }

    if (filename == "") {
        print("Error: no input file specified\n");
        print_usage();
        exit(1);
    }

    if (verbose) {
        print("Platform: " + get_platform() + "\n");
        print("Reading file: " + filename + "\n");
    }

    // Read the file contents
    let content: str = read_file(filename);

    if (len(content) == 0) {
        print("Warning: file is empty or could not be read\n");
        return 1;
    }

    // Calculate statistics
    let chars: i32 = len(content);
    let words: i32 = count_words(content);
    let lines: i32 = count_lines(content);

    // Output results
    if (words_only) {
        print(i32_to_str(words) + "\n");
    } else if (lines_only) {
        print(i32_to_str(lines) + "\n");
    } else {
        print("  Lines: " + i32_to_str(lines) + "\n");
        print("  Words: " + i32_to_str(words) + "\n");
        print("  Chars: " + i32_to_str(chars) + "\n");
    }

    if (verbose) {
        print("Done.\n");
    }

    return 0;
}
```

This example demonstrates:

- **Module and imports** (`module wordcount; import std; import file_io;`)
- **Global variables** (`verbose`, `show_help`)
- **Functions with different return types** (`count_words` → `i32`, `print_usage` → `void`, `get_platform` → `str`)
- **String manipulation** (indexing, concatenation, length, comparison)
- **Arrays** (`argv()`, iterating with `len()`)
- **Control flow** (`if`/`else if`/`else`, `while` loops, `for` loops)
- **Conditional compilation** (`#if(os == ...)` for `get_platform`)
- **Standard library usage** (`print`, `exit`, `starts_with`, `i32_to_str`, `read_file`)

---

## 23. Compiler Flags

The Novus compiler accepts the following command-line flags:

### `--debug`

Enables verbose compilation output. Shows each compilation stage, generated assembly paths, and assembler/linker invocations:

```bash
novus --debug main.nov
```

### `--os <os>`

Sets the target operating system. Valid values:

| Value     | Target     |
|-----------|------------|
| `darwin`  | macOS      |
| `linux`   | Linux      |
| `windows` | Windows    |

```bash
novus --os linux main.nov
```

### `--arch <arch>`

Sets the target CPU architecture. Valid values:

| Value   | Architecture      |
|---------|-------------------|
| `arm64` | ARM 64-bit        |
| `amd64` | x86 64-bit        |
| `386`   | x86 32-bit        |

```bash
novus --arch amd64 main.nov
```

### `--nasm-path <path>`

Specifies a custom path to the NASM assembler binary. This is primarily useful on Windows where NASM may not be in the system PATH:

```bash
novus --nasm-path "C:\tools\nasm\nasm.exe" --os windows --arch amd64 main.nov
```

### `--golink-path <path>`

Specifies a custom path to the GoLink linker binary. Used for Windows targets:

```bash
novus --golink-path "C:\tools\golink\golink.exe" --os windows --arch amd64 main.nov
```

### Combined Example

Cross-compile for Linux x86_64 with debug output:

```bash
novus --debug --os linux --arch amd64 main.nov
```

The resulting binary will be at `build/linux_amd64/main`.
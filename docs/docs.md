# Novus language reference

This document is the canonical reference for Novus. It covers the language
syntax, types, builtins and how Novus integrates with [Nox](https://github.com/MJDaws0n/Nox).

For historical / scratchpad material see [`legacy_novus_docs.md`](legacy_novus_docs.md).

---

## 1. Files and modules

Every Novus file starts with a `module` declaration:

```novus
module my_app
```

A program's entry point is the function `main() -> i32` in any file that is
compiled as the root.

---

## 2. Comments

```novus
// single-line comment
/* multi-line
   comment */
```

---

## 3. Types

| Type     | Notes                                              |
| -------- | -------------------------------------------------- |
| `i32`    | 32-bit signed integer                              |
| `i64`    | 64-bit signed integer                              |
| `u32`    | 32-bit unsigned                                    |
| `u64`    | 64-bit unsigned                                    |
| `f32`    | 32-bit float                                       |
| `f64`    | 64-bit float                                       |
| `bool`   | `true` / `false`                                   |
| `str`    | UTF-8 string (length-prefixed in memory)           |
| `void`   | no value (function return only)                    |
| `[]T`    | dynamic array of `T` (e.g. `[]i32`, `[]str`)       |

> **Note**: there is no plain `int`. Always pick a width (`i32`, `i64`, ...).

---

## 4. Variables

```novus
let x: i32 = 0;
let name: str = "Novus";
let xs: []i32 = [1, 2, 3];
```

Variables are mutable. Type annotations are required at declaration.

### Compound assignment

```novus
x += 1;
x -= 2;
x *= 3;
x /= 4;
x %= 5;
```

### Increment / decrement

```novus
x++;
x--;
```

These are statements, not expressions.

---

## 5. Functions

```novus
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}
```

### Overloading

Functions can be overloaded on parameter types (and arity):

```novus
fn show(x: i32) -> void  { std.print(std.to_str(x)); }
fn show(s: str) -> void  { std.print(s); }
fn show(xs: []i32) -> void { /* ... */ }
```

The compiler picks the matching overload at the call site. Return type is
*not* part of the signature.

---

## 6. Control flow

```novus
if (cond) {
    ...
} else if (other) {
    ...
} else {
    ...
}

while (cond) {
    ...
}
```

Logical operators: `&&`, `||`, `!`.

Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`.

Arithmetic: `+`, `-`, `*`, `/`, `%`.

String concatenation uses `+`.

---

## 7. Arrays

```novus
let xs: []i32 = [10, 20, 30];
xs[0];                  // index
array_append(xs, 40);   // append (mutates xs)
std.len(xs);            // length
```

Arrays are heap-allocated and grow as needed.

---

## 8. Imports

```novus
import lib/std std;          // import folder lib/std as `std`
import lib/file_io fio;      // alias
import lib/std;              // no alias: names imported into current module
```

When using [Nox](https://github.com/MJDaws0n/Nox), packages are pulled into
`lib/<name>/` and have a `main.nov` that re-exports everything.

---

## 9. Builtins

These are provided by the compiler without any import:

| Builtin                         | Notes                                          |
| ------------------------------- | ---------------------------------------------- |
| `len(value) -> i32`             | length of a string or array, including class arrays |
| `array_append(xs: []T, v: T)`   | grow an array                                  |
| `mov(reg, value)`               | inline asm: load register                      |
| `syscall()`                     | inline asm: trigger a system call              |
| `push(reg)` / `pop(reg)`        | inline asm: stack manipulation                 |
| `store_word(address, value)`   | write one native pointer-width word (8 bytes on 64-bit targets, 4 on x86) |
| `store_byte(address, value)`   | write the low byte to memory                    |
| `load8` / `load32` / `load64`   | raw memory reads; `load32` sign-extends          |
| `gc_collect()`                 | collect unreachable heap allocations           |
| `alloc_string(size) -> str`     | allocate GC-tracked writable bytes and a trailing zero; initialize the bytes before reading them |

`alloc_string` is a low-level primitive for library authors. Its size must be
non-negative and fit in the target address space. It reserves `size + 1` bytes
and writes a zero byte at offset `size`; the preceding bytes are uninitialized.
Application code can use `make_buffer(size)` for a space-filled buffer,
`copy_bytes(address, count)` for an owned copy, or `StringBuilder` for text.

Class references can be `null`. The `&&` and `||` operators short-circuit, so
`value != null && value.field > 0` safely guards the field access.

> Inline-asm builtins are only useful when writing freestanding code without
> the standard library. Most user code should call `std.print`, `std.exit`,
> etc. instead.

For everything else — string manipulation, conversions, file IO, networking,
math, etc. — see the [novus-std documentation](https://github.com/MJDaws0n/novus-std/blob/main/docs.md)
and other libraries in the [Nox registry](https://github.com/MJDaws0n/Nox/blob/main/registry.txt).

---

## 10. The CLI

```
novus [flags] <file.nov>

Flags:
  --target=<os/arch>    Select the compilation target
  --heap-size=<size>    Heap reservation, e.g. 64m or 1g (default: 64m)
  --gc-entries=<N>      Maximum tracked allocations (default: 65536)
  --asm-only           Emit assembly without assembling or linking
  --skip-link          Assemble without linking
  --debug              Print tokens, AST, IR and toolchain details
```

Artifacts go to `build/<target>/<name>[.exe]`.

Run `novus` without arguments to display usage. Supported targets are
`darwin/arm64`, `darwin/amd64`, `linux/amd64`, `linux/arm64`, `linux/386`,
`windows/amd64`, and `windows/arm64`. The 32-bit Linux backend supports full-width
`i64`/`u64` values and floating-point operations, and can compile the compiler itself; see the
[verification matrix](testing-matrix.md) for the distinction between compiler
hosts and compilation targets.

---

## 11. Working with Nox

```sh
nox init               # scaffold a project, adds std by default
nox pull <pkg>         # install a registry package into lib/
nox update             # refresh installed packages
nox publish            # publish a package (registry maintainers)
```

`libraries.conf` records the project's dependencies in the form:

```
pkg:std:version=1.3.0
pkg:file_io:version=1.0.2
```

Nox resolves these against its registry and downloads them on `nox pull`/`nox update`.

---

## 12. Cross-compilation cheatsheet

```sh
# from macOS arm64 to linux amd64
novus --target=linux/amd64 main.nov

# from linux to windows
novus --target=windows/amd64 main.nov
```

See the top-level [README](../README.md) for the host-toolchain requirements
per target.

# novus compiler issues

a list of bugs and quirks ive found in the novus compiler while building golemmc. these are things that tripped me up and had to work around, figured id write em down so they can get fixed at some point.

## 1. overloaded functions dont link across modules

if you have two functions with the same name but different param types (like `int_to_str(i32)` and `int_to_str(i64)`), the compiler mangles the labels to something like `_int_to_str.i32` and `_int_to_str.i64`. but when another module calls `int_to_str()`, the call site emits `bl _int_to_str` (unmangled), which the linker cant find. so you get undefined symbol errors.

**workaround**: dont use overloaded function names across modules. use unique names instead, like `i32_to_str()` and `i64_to_str()`.

**expected behaviour**: the compiler should emit the correct mangled label at the call site based on the argument types.

## 2. cross-module globals generate bad assembly

if you declare a `let` variable at module top-level in an imported file, the generated assembly uses the variable name directly in instructions like `cbz debug_enabled` or `stur cfg_motd`, instead of properly loading the value from memory first. this means any global variable in an imported module will produce invalid assembly that either crashes or behaves unpredictably.

**workaround**: dont use global variables in any imported module. use getter functions that return values, or use file-based state.

**expected behaviour**: globals in imported modules should generate proper memory load/store instructions (adrp + ldr for reads, adrp + str for writes).

## 3. main module globals cant be used inside functions

variables declared at the top level of the main module file (outside any function) cant be referenced inside `fn main()` or any other function in that file. the compiler reports "undefined identifier".

**workaround**: use getter functions that return the value, like `fn get_version() -> str { return "0.1.0"; }` instead of `let version: str = "0.1.0";`.

**expected behaviour**: top-level variables in the main module should be accessible from all functions in that file.

## 4. diamond imports can OOM the compiler

if the same module gets imported by multiple files in the import tree (e.g. both `a.nov` and `b.nov` import `utils.nov`, and `main.nov` imports both `a.nov` and `b.nov`), the compiler can run out of memory or get stuck in a loop. this is probably because it processes the same module multiple times without deduplication.

**workaround**: structure your imports carefully to avoid the same module being pulled in from multiple paths. it seems to work fine if the same module is imported multiple times from different relative paths, but can be flaky. keep the import tree clean.

**expected behaviour**: the compiler should track which modules have already been processed and skip duplicates.

## 5. syscall error codes not accessible on macOS ARM64

this isnt really a compiler bug but more of a missing feature. on macOS ARM64, syscalls indicate errors by setting the carry flag in NZCV, and putting the errno in x0. the `getflag()` builtin exists in the docs but none of the ARM64 flag names (like `c`, `carry`, `cf`, `nzcv`) are recognised by the compiler. so theres no way to check if a syscall failed or succeeded when the return value could be either a valid result or an errno.

for example, `open()` returns an fd on success (small positive int like 3) and errno on failure (also small positive int like 2 for ENOENT). without checking the carry flag, these are indistinguishable.

**workaround**: use syscalls where success and failure return values dont overlap. for example, `access()` returns 0 on success and errno > 0 on failure, so you can tell them apart. or use `stat()` which also returns 0 on success.

**expected behaviour**: `getflag(c)` or similar should work on ARM64 to read the carry flag after a syscall. alternatively, provide a `syscall_ok()` builtin that checks the carry flag.

## 6. no bitwise operations

novus doesnt have bitwise AND (`&`), OR (`|`), XOR (`^`), NOT (`~`), or shift (`<<`, `>>`) operators. for a systems language that compiles to native code and lets you do raw syscalls, this is a pretty big gap. lots of protocol work and binary data manipulation needs bitwise ops.

**workaround**: use arithmetic equivalents. `x & 0x7F` becomes `x % 128`. `x | 0x80` becomes `x + 128` (only works if the bit isnt already set). `x << n` becomes `x * 2^n` (using a loop). `x >> n` becomes `x / 2^n`. these are slower and more error-prone but they work.

**expected behaviour**: add `&`, `|`, `^`, `~`, `<<`, `>>` operators that map to the corresponding ARM64/x86 instructions.
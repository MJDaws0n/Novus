# Novus Bugs Found During Nox Development

Bugs discovered while building the Nox package manager. These are issues with the Novus language/compiler, not with Nox itself.

## Bug 1: Function overloading doesn't resolve across modules

**Severity:** Medium
**Workaround:** Use non-overloaded function names (e.g. `i32_to_str` instead of `int_to_str`)

When a function like `int_to_str` is overloaded (one version for `i32`, one for `i64`), the overloaded names get mangled and the compiler doesn't resolve the calls properly when the function is defined in one module and called from another.

GolemMC's standard_lib.nov already has the workaround: separate `i32_to_str()` and `i64_to_str()` functions that aren't overloaded however please fix it so that it's needing the workaround.

## Bug 2: Global variables in the main module don't work in functions

**Severity:** Medium
**Workaround:** Use functions that return the value instead of global variables

From GolemMC: "version info - in a function cos globals in main module dont work in funcs". Global variables declared in the main module file cannot be read from functions in the same file. The workaround is to wrap the value in a function:

```novus
// BAD - won't work in functions
let version: str = "1.0.0";

// GOOD - works everywhere
fn get_version() -> str {
    return "1.0.0";
}
```
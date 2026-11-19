# novus-std

Standard library for the [Novus](https://github.com/MJDaws0n/Novus) programming
language. Provides core types, string/array utilities, I/O, memory access and
platform abstractions for **macOS**, **Linux** and **Windows**.

```novus
module myapp;
import lib_std std;

fn main() -> i32 {
    let name: str = "world";
    print("hello, " + name);
    return 0;
}
```

Installed automatically by `nox init` (added as a default dependency in
`libraries.conf`). To add manually:

```
pkg:std:version=1.3.0
```

See [`docs.md`](./docs.md) for the complete API reference.

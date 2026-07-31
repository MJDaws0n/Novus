# Novus

Novus is a small, statically-typed systems language that compiles directly to native
machine code. It targets darwin/arm64, linux/amd64, linux/arm64, linux/x86 and
windows/amd64 today.

> **Status:** active development. The language is stable enough to write real
> command-line apps (see [Todo-App](https://github.com/MJDaws0n/Todo-App) and
> [stopwatch](https://github.com/MJDaws0n/stopwatch)), but expect rough edges.

---

## Install

Automatic:

```sh
curl -fsSL https://raw.githubusercontent.com/MJDaws0n/novus/main/install.sh | bash
```

Manual: grab a binary for your platform from the
[releases page](https://github.com/MJDaws0n/Novus/releases) and put it on your `PATH`.

```sh
sudo cp novus_<your-platform> /usr/local/bin/novus
sudo chmod +x /usr/local/bin/novus
# macOS only:
sudo xattr -d com.apple.quarantine /usr/local/bin/novus
```

It is strongly recommended to also install the package manager,
[Nox](https://github.com/MJDaws0n/Nox).

---

## Quick start

```sh
nox init my-app
cd my-app
novus main.nov
./build/<target>/my-app
```

`nox init` already adds the standard library (`std`) by default. The starter
`main.nov` looks like:

```novus
module my_app

import lib/std std;

fn main() -> i32 {
    std.print("Hello, world!");
    std.exit(0);
    return 0;
}
```

---

## Language at a glance

```novus
module example

import lib/std std;

fn greet(name: str) -> void {
    std.print("Hi " + name);
}

fn greet(times: i32) -> void {       // function overloading
    let i: i32 = 0;
    while (i < times) {
        std.print("Hi!");
        i += 1;                      // compound assignment
    }
}

fn main() -> i32 {
    greet("Max");
    greet(3);

    let xs: []i32 = [1, 2, 3];
    std.print(std.to_str(std.len(xs)));    // unified len/to_str

    std.exit(0);
    return 0;
}
```

See [`docs/docs.md`](docs/docs.md) for the full language reference.

---

## Build the compiler from source

```sh
go run cmd/novus/main.go --target=darwin/arm64 path/to/file.nov
```

To rebuild all compiler release binaries (the compiler itself is a Go
program and cross-compiles without target SDKs):

```sh
./build-release.sh
```

### Cross-compilation toolchain notes

- `linux/amd64`, `linux/386` need NASM + `ld`.
- `linux/arm64` needs `aarch64-linux-gnu-as` and `aarch64-linux-gnu-ld` when
  the host is not arm64.
- `windows/amd64` needs NASM + GoLink (or `link.exe`).
- `darwin/arm64` needs Apple's `as`/`ld` (i.e. Xcode CLT) or a darwin cross
  toolchain.

Artifacts are written to `build/<target>/`. Valid target names:
`darwin_arm64`, `linux_x86_64`, `linux_x86`, `linux_arm64`, `windows_x86_64`.

---

## Repository layout

- `cmd/novus/` — compiler CLI entry point
- `internal/` — lexer, parser, semantic analysis, IR, codegen
- `docs/` — language and library reference
- `docs/notes/` — developer notes / learning material
- `vscode-novus/` — VS Code syntax extension
- `examples/` — example apps and tiny libraries

---

## Related projects

- **[Nox](https://github.com/MJDaws0n/Nox)** — the package manager
- **[novus-std](https://github.com/MJDaws0n/novus-std)** — standard library
- **[Todo-App](https://github.com/MJDaws0n/Todo-App)** and
  **[stopwatch](https://github.com/MJDaws0n/stopwatch)** — example CLI apps

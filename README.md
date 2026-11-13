# Novus

Novus is a small, statically-typed systems language that compiles directly to native
machine code. Its compiler is written in Novus, with the previous Go implementation
preserved in `legacy-go/` for bootstrapping and regression tests.

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

Install Python 3, Go, and your platform's native assembler/linker, then run:

```sh
python3 scripts/bootstrap.py
```

This builds the preserved Go bootstrap, compiles `main.nov`, and uses the resulting
Novus compiler to build two more generations. It requires identical assembly from
generations two and three and runs the language and platform-library fixtures.
The compiler and machine-readable results are in `build/selfhost/<target>/`.

An existing Novus compiler can replace the Go bootstrap:

```sh
python3 scripts/bootstrap.py --bootstrap /absolute/path/to/novus
```

For a verified release binary in `dist/`, use `./build-release.sh` (or
`python scripts/bootstrap.py --package` on Windows). CI builds on native runners
for macOS ARM64/Intel, Linux ARM64/x64/x86 (32-bit), and Windows ARM64/x64.
Every host runs the same self-compilation and runtime verification suite.
CI also compares fresh-process compilation of Nox and the compiler itself with
the Go bootstrap. Each large workload must take at most 1.25 times Go's median
time, measured over six alternating runs after a warm-up.

See the [migration and benchmark report](docs/self-hosting.md) and
[verification matrix](docs/testing-matrix.md) for measured results and platform
limitations.

### Cross-compilation toolchain notes

- `linux/amd64`, `linux/386` need GNU `as` + `ld`.
- `linux/arm64` needs `aarch64-linux-gnu-as` and `aarch64-linux-gnu-ld` when
  the host is not arm64.
- `windows/amd64` needs NASM + GoLink (or `link.exe`).
- `windows/arm64` needs Clang's assembler and the MSVC linker/Windows SDK.
- `darwin/arm64` needs Apple's `as`/`ld` (i.e. Xcode CLT) or a darwin cross
  toolchain.

Artifacts are written to `build/<os>_<architecture>/`. Compiler target flags use
`--target=darwin/arm64`, `darwin/amd64`, `linux/amd64`, `linux/arm64`, `linux/386`,
`windows/amd64`, or `windows/arm64`.

---

## Repository layout

- `main.nov` — native compiler entry point
- `src/` — lexer, parser, semantic analysis, IR, backends and command-line driver
- `lib/` — bundled Novus text, number, file, process, environment, archive and download libraries
- `legacy-go/` — preserved Go bootstrap and its regression tests
- `scripts/bootstrap.py` — reproducible self-build and native test runner
- `tests/` — language and platform runtime fixtures
- `docs/` — language and library reference
- `docs/notes/` — developer notes / learning material
- `vscode-novus/` — VS Code syntax extension

---

## Related projects

- **[Nox](https://github.com/MJDaws0n/Nox)** — the package manager
- **[novus-std](https://github.com/MJDaws0n/novus-std)** — standard library
- **[Todo-App](https://github.com/MJDaws0n/Todo-App)** and
  **[stopwatch](https://github.com/MJDaws0n/stopwatch)** — example CLI apps

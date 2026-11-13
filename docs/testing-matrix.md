# Compiler verification matrix

The migration is tested with the preserved Go suite, native Novus self-builds,
runtime fixtures and comparisons against the Go bootstrap. Native self-builds
require identical assembly from generations two and three. A compilation-target
comparison does not establish native runtime support.

## Native hosts

The CI workflow executes all seven host targets, including 32-bit Linux binaries
on an x64 Linux runner. Results and executable artifacts are attached to the
[Compiler workflow runs](https://github.com/MJDaws0n/Novus/actions/workflows/compiler.yml).

| Host | Native self-build and runtime status |
| --- | --- |
| macOS ARM64 | Passed locally and in CI |
| macOS Intel | Passed in CI |
| Linux x64 | Passed in CI |
| Linux ARM64 | Passed in CI |
| Windows ARM64 | Passed in CI |
| Windows x64 | Passed in CI |
| Linux x86 (32-bit) | Passed in CI |

All seven targets passed in [run 43489144967](https://github.com/MJDaws0n/Novus/actions/runs/43489144967)
at compiler commit `79f98fca7be9c073bbec0b5dc33e6596947d386d`: 21 compiler builds,
763 Go/Novus comparisons and 110 recorded runtime checks, plus the Go suite and
first-generation smoke tests. Generations two and three emitted identical
assembly on every target. The same compiler source was verified locally on
macOS ARM64. [Machine-readable results](self-hosting-verification.json) include
source/assembly/binary hashes and individual checks.

## Coverage

- Classes, constructors, overloads, aliases, selective and transitive imports,
  static members and class arrays.
- Loops, nested scope, short-circuit boolean evaluation and nullable references.
- Signed/unsigned arithmetic, narrowing, shifts across both 32-bit halves,
  full-width division/multiplication, floating-point operations and
  exact decimal literal conversion. The rounding suite has 184 reference inputs.
- Explicit register arguments and their preservation across computed values.
- Global and graph roots, automatic/manual collection, array growth and reuse,
  including a 1,024-entry allocation-table stress run.
- Binary files larger than one I/O buffer, metadata, path resolution, deletion,
  time and memory helpers, Unicode paths/imports and file positions beyond 4 GiB.
- Executable lookup skips directories and non-executable files on Unix.
- Child-process exit status, empty/quoted/Unicode arguments, inherited Unicode
  environment values and 85,000 bytes of captured output.
- HTTP download and ZIP extraction with Unicode paths and replacement of existing
  downloads, plus 404/missing-archive error paths, using a local test server.

`scripts/differential.py` compares exit status and exact assembly for 105 module
fixtures extracted from the preserved Go tests, plus decimal literals, compiler
regressions, processor flags and full-width arithmetic. CI repeats these 109
comparisons on every host. These comparisons supplement the executed runtime
fixtures and three-generation compiler reproduction.

The preserved Go tests pass. Optional historical example-integration tests still
depend on example checkouts absent from this repository; the checked-in runtime
fixtures above provide the migration's native execution evidence.

## 32-bit Linux

`linux/386` supports the compiler as both an output target and a running host.
The backend emits two-word integer operations, SSE2/x87 floating-point operations,
eight-byte scalar/local/class/array slots, and eight-byte function arguments.
Physical addresses, OS structures and GC metadata retain four-byte words.
`store_word` writes one native pointer-sized word; `store64` writes all eight
bytes on every target. Arrays use a 12-byte header and eight-byte elements.

The Linux libraries provide 32-bit file, metadata, clock, process, environment,
argument and memory bindings. Process argument arrays are packed into the
four-byte pointer layout expected by `execve`. CI also cross-compiles and runs
32-bit regression fixtures with the x64 Novus compiler.

## Commands

```sh
(cd legacy-go && go test ./...)
python3 scripts/bootstrap.py --package
python3 scripts/bootstrap.py --test-only /absolute/path/to/novus
python3 scripts/differential.py --reference /path/to/go-bootstrap \
  --compiler /path/to/novus --target=linux/amd64
python3 scripts/test-x86.py --compiler /path/to/novus-linux-amd64
```

See [self-hosting.md](self-hosting.md) for build details and the controlled local
compilation benchmark.

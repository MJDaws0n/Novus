# Novus compiler migration

The compiler implementation lives in `main.nov` and `src/*.nov`. Lexing, parsing,
import resolution, semantic analysis, class handling, IR lowering, all assembly
backends, the command-line driver and toolchain management run as Novus code.
The resulting executable does not invoke Go. The Go sources and their tests are
preserved in `legacy-go/` as the bootstrap and comparison implementation.

`lib/` contains the compiler's Novus dependencies. File and process operations,
environment access, text builders, integer and decimal conversion, downloads and
ZIP extraction are public library functions. Platform libraries contain their
native system-call/API bindings. Assembly generation remains part of the compiler
backends; application-level operations use the libraries.

## Reproduction

```sh
python3 scripts/bootstrap.py --package
```

The script builds the Go bootstrap, then three Novus generations. Generations two
and three must emit byte-identical assembly from an unchanged source tree. The
script records source and assembly hashes, elapsed times and runtime results in
`build/selfhost/<target>/results.json`. It packages the verified third generation
as `dist/novus-<os>-<arch>` (with `.exe` on Windows).

An existing Novus executable can bootstrap the tree without Go:

```sh
python3 scripts/bootstrap.py --bootstrap /absolute/path/to/novus --package
```

The compiler build uses a 1 GiB heap setting and 8,388,608 tracked allocations.
Ordinary application defaults remain 64 MiB and 65,536 tracked allocations.
Exhausting the allocation table traps rather than silently leaving allocations
untracked. The collector uses size-class reuse, a sorted allocation table, a
bounded page index and a marking queue. Its tracing algorithm is Novus source.
Radix sorting reuses the marking queue's scratch space. A bounded pointer cache
reuses that space after sorting to avoid repeated lookups during tracing; it is
cleared on every collection and does not increase the reserved heap or metadata.

The [verification matrix](testing-matrix.md) and
[machine-readable results](self-hosting-verification.json) distinguish native execution from
cross-target output comparisons. Linux x86 is a full 32-bit compiler host, with
64-bit scalar values, floating-point operations and native 32-bit OS pointers.

## Corrections made during the migration

- Empty `for` initializers, `null` class references, class-array lengths, lexical
  scope restoration, imported-body type checking and short-circuit evaluation.
- Unsigned arithmetic/comparisons/shifts and signed or unsigned narrowing.
- Full-width integer and floating-point operations on 32-bit Linux, including
  eight-byte local, class, array and argument slots, plus 32-bit OS bindings.
- Register preservation for local, global and computed `mov`, `lea` and `setreg`
  arguments; flag reads that preserve processor flags; Windows stack probing,
  argument startup and ARM64 reserved registers. NASM uses bounded optimization
  for large generated assembly files.
- Go value-copy and slice-aliasing differences in the port.
- Memory tracking, allocator reuse and macOS time-call memory corruption.
- Correct decimal-to-binary floating-point rounding, including ties, signed zero,
  subnormal values, overflow and underflow. Fixed and generated inputs have
  independent IEEE-754 reference bit patterns.
- Portable binary files, metadata, canonical/absolute paths, process arguments,
  inherited environment and output capture; missing files are distinguished from
  other metadata errors. Windows paths, command lines and environment values
  convert between UTF-8 and the native UTF-16 APIs. File seeking preserves
  positions above 4 GiB. Executable lookup checks Unix execution permissions,
  and downloads can replace existing files and use Unicode destinations.

The one-time translation tools are retained under `legacy-go/migration/` for
provenance. They are not part of a normal build and must not overwrite the reviewed
Novus sources.

## Performance changes

Compiler maps retain insertion order while using hash buckets for lookup. Tokens,
positions and operands are shared when immutable; empty AST lists allocate their
own storage only when modified. Assembly generation avoids temporary operand
arrays, redundant instruction copies and repeated decimal formatting, and combines
adjacent literal output into a single append.

The standard library uses a contiguous growing text builder and direct text file
I/O. Byte copying uses native words, and text replacement appends each segment
once instead of repeatedly copying the growing result. ARM64's assembly cleanup
scans and copies ranges without allocating a string for each line. These paths,
including embedded zero bytes, Unicode, builder snapshots and GC reuse, have
runtime regression tests.

## Compilation benchmarks

All seven hosts pass the 1.25× limit for both large workloads in
[run 43502727450](https://github.com/MJDaws0n/Novus/actions/runs/43502727450), using compiler source `977a6ae4777ad5e6816cd5f00264a16f56400d67`.
Each pair uses identical source files, bundled libraries and target architecture;
the 32-bit Linux comparison uses a 32-bit Go bootstrap. A ratio below 1 means
Novus compiles faster than Go.

| Host | Nox: Novus / Go | Compiler: Novus / Go |
| --- | ---: | ---: |
| macOS ARM64 | 0.915× | 0.493× |
| macOS Intel | 0.724× | 0.170× |
| Linux x64 | 0.493× | 0.137× |
| Linux ARM64 | 1.233× | 0.304× |
| Windows x64 | 0.989× | 0.315× |
| Windows ARM64 | 1.099× | 0.727× |
| Linux x86 (32-bit) | 1.090× | 0.329× |

Measurements start a fresh compiler process and include parsing, imports,
semantic analysis, assembly generation, assembly and linking. One warm-up is
excluded; six measured runs alternate compiler order. CI fails if either large
workload exceeds 1.25×. These are measured workload results, not a guarantee for
every possible program or machine. Small-program timings are recorded separately
and are not part of the large-program limit.

The application is Nox 3.4.2 at `af635107c6e8d81d1059062fecdbb96e408fc43f`;
CI also runs the resulting executable's version command. Complete samples,
source/library/compiler hashes and timings are in
[self-hosting-performance.json](self-hosting-performance.json).

### Local macOS measurement

Measured on 13 November 2026 on an Apple M2, with no local build or container
workload running alongside the benchmark, using the same procedure:

| Application | Go bootstrap median | Novus median | Novus / Go |
| --- | ---: | ---: | ---: |
| Small class/import fixture | 0.177 s | 0.184 s | 1.039× |
| Nox 3.4.2 | 1.951 s | 1.981 s | 1.016× |
| Novus compiler | 16.804 s | 11.508 s | 0.685× |

The local samples are in [self-hosting-benchmark.json](self-hosting-benchmark.json).
The earlier port's 7.23× Nox result is preserved in
[self-hosting-benchmark-before-optimization.json](self-hosting-benchmark-before-optimization.json).
Each comparison uses matching dependencies at its own revision. The Go bootstrap
includes the compiler correctness fixes and runtime intrinsics needed by the port.

To repeat the measurement, prepare the pinned Nox source with its `lib/` pointing
at this checkout's bundled libraries, then run:

```sh
python3 scripts/benchmark.py \
  --reference build/selfhost/darwin_arm64/bootstrap-go \
  --compiler dist/novus-darwin-arm64 \
  --large /absolute/path/to/nox/main.nov \
  --include-self --target=darwin/arm64 --runs=6 --max-ratio=1.25
```

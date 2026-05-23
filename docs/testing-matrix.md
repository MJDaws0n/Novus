# Novus Example + Cross-Target Test Matrix

This matrix reflects the current test strategy and latest run in this workspace.

## 1. Automated test entrypoints

### Compiler tests

```sh
go test ./...
```

### Example/cross-target matrix

```sh
chmod +x scripts/run-example-matrix.sh
NOVUS_BIN=/home/max/.local/bin/novus ./scripts/run-example-matrix.sh
```

## 2. Latest run results (Linux host)

| Check | Result |
|---|---|
| `go test ./...` | PASS |
| `internal/imports` example integration tests | PASS |
| `hello-matrix` build + run (`linux/amd64`) | PASS |
| `dice-duel` build + run (`linux/amd64`) | PASS |
| `string-lab` build + run (`linux/amd64`) | PASS |
| `portable-sanity` build + run (`linux/amd64`) | PASS |
| `portable-sanity` asm-only (`linux/arm64`) | PASS |
| `portable-sanity` asm-only (`darwin/arm64`) | PASS |
| `portable-sanity` asm-only (`windows/amd64`) | PASS |
| `hello-matrix` full windows link (`windows/amd64`) | FAIL in Wine+GoLink (`Insufficient memory for the task`) |
| `hello-matrix.exe` runtime in Wine | SKIPPED (no executable generated due link failure) |

## 3. Cross-OS notes

- Native runtime checks are currently performed on Linux.
- Windows runtime was attempted via Wine; linking currently fails in this environment with GoLink memory allocation errors.
- Docker and `qemu-aarch64` user-mode binaries are not currently installed in this host session, so Linux ARM64 runtime emulation was not executed here.
- macOS runtime cannot be executed natively on this Linux host; macOS coverage is compile/assembly validation.

## 4. Re-running with system-installed tools

If you install additional host tooling, rerun:

```sh
go test ./...
NOVUS_BIN=/home/max/.local/bin/novus ./scripts/run-example-matrix.sh
```

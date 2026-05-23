# Novus Examples

This folder contains modern example apps and reusable libraries.

## Layout

- `apps/hello-matrix` — prints a numeric table and checksum.
- `apps/dice-duel` — small terminal game simulation.
- `apps/string-lab` — string/array manipulation demo.
- `apps/portable-sanity` — target-portable smoke app (no platform I/O).
- `lib/core` — utility functions (integer/string conversion, clamp, PRNG).
- `lib/game` — game helpers built on `core`.
- `lib/term` — platform-aware terminal output + process exit (`linux`, `darwin`, `windows`).

## Build and run (Linux host)

```sh
novus --target=linux/amd64 examples/apps/hello-matrix/main.nov
./build/linux_x86_64/hello_matrix
```

```sh
novus --target=linux/amd64 examples/apps/dice-duel/main.nov
./build/linux_x86_64/dice_duel
```

## Cross-target smoke checks

```sh
novus --target=linux/arm64 --asm-only examples/apps/portable-sanity/main.nov
novus --target=darwin/arm64 --asm-only examples/apps/portable-sanity/main.nov
novus --target=windows/amd64 --asm-only examples/apps/portable-sanity/main.nov
```

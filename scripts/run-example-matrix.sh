#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
NOVUS_BIN="${NOVUS_BIN:-novus}"

pass_count=0
fail_count=0
skip_count=0

run_step() {
  local label="$1"
  shift
  printf "\n== %s ==\n" "$label"
  if "$@"; then
    echo "PASS"
    pass_count=$((pass_count + 1))
  else
    echo "FAIL"
    fail_count=$((fail_count + 1))
  fi
}

skip_step() {
  local label="$1"
  local reason="$2"
  printf "\n== %s ==\n" "$label"
  printf "SKIP (%s)\n" "$reason"
  skip_count=$((skip_count + 1))
}

cd "$ROOT_DIR"

run_step "Build hello-matrix linux/amd64" "$NOVUS_BIN" --target=linux/amd64 examples/apps/hello-matrix/main.nov
run_step "Run hello-matrix" ./build/linux_x86_64/hello_matrix

run_step "Build dice-duel linux/amd64" "$NOVUS_BIN" --target=linux/amd64 examples/apps/dice-duel/main.nov
run_step "Run dice-duel" ./build/linux_x86_64/dice_duel

run_step "Build string-lab linux/amd64" "$NOVUS_BIN" --target=linux/amd64 examples/apps/string-lab/main.nov
run_step "Run string-lab" ./build/linux_x86_64/string_lab

run_step "Build portable-sanity linux/amd64" "$NOVUS_BIN" --target=linux/amd64 examples/apps/portable-sanity/main.nov
run_step "Run portable-sanity" ./build/linux_x86_64/portable_sanity

run_step "Asm-only portable linux/arm64" "$NOVUS_BIN" --target=linux/arm64 --asm-only examples/apps/portable-sanity/main.nov
run_step "Asm-only portable darwin/arm64" "$NOVUS_BIN" --target=darwin/arm64 --asm-only examples/apps/portable-sanity/main.nov
run_step "Asm-only portable windows/amd64" "$NOVUS_BIN" --target=windows/amd64 --asm-only examples/apps/portable-sanity/main.nov

if command -v nasm >/dev/null 2>&1 && command -v golink >/dev/null 2>&1; then
  run_step "Build hello-matrix windows/amd64" "$NOVUS_BIN" --target=windows/amd64 examples/apps/hello-matrix/main.nov
  if [[ -f build/windows_x86_64/hello_matrix.exe ]]; then
    if command -v wine >/dev/null 2>&1; then
      run_step "Run hello-matrix.exe via wine" wine ./build/windows_x86_64/hello_matrix.exe
    else
      skip_step "Run hello-matrix.exe via wine" "wine not installed"
    fi
  else
    skip_step "Run hello-matrix.exe via wine" "hello_matrix.exe not generated"
  fi
else
  skip_step "Build hello-matrix windows/amd64" "nasm or golink not available"
  skip_step "Run hello-matrix.exe via wine" "windows build step skipped"
fi

printf "\nSummary: %d passed, %d failed, %d skipped\n" "$pass_count" "$fail_count" "$skip_count"
if (( fail_count > 0 )); then
  exit 1
fi

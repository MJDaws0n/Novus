#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
# Each native CI runner produces its own verified release binary. Cross-compiling
# the Go bootstrap alone would package the legacy compiler instead of Novus.
exec python3 scripts/bootstrap.py --package "$@"

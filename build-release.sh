#!/usr/bin/env bash

set -euo pipefail

if ! command -v go >/dev/null 2>&1; then
    echo "error: Go is required to build Novus release binaries" >&2
    exit 1
fi

build_one() {
    local target_os="$1"
    local target_arch="$2"
    local output="$3"

    echo "building ${target_os}/${target_arch} -> ${output}"
    CGO_ENABLED=0 GOOS="$target_os" GOARCH="$target_arch" \
        go build -trimpath -ldflags="-s -w" -o "$output" ./cmd/novus
}

build_one darwin amd64 novus-darwin-amd64
build_one darwin arm64 novus-darwin-arm64
build_one linux amd64 novus-linux-amd64
build_one linux arm64 novus-linux-arm64
build_one windows amd64 novus-windows-amd64.exe

echo "release binaries:"
ls -lh novus-darwin-amd64 novus-darwin-arm64 \
    novus-linux-amd64 novus-linux-arm64 novus-windows-amd64.exe

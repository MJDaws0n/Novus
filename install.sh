#!/bin/bash
set -e

# Novus Compiler — Global Install Script
# Downloads and installs the latest Novus binary for your OS/architecture.
# Usage: curl -fsSL https://raw.githubusercontent.com/MJDaws0n/novus/main/install.sh | bash

REPO="MJDaws0n/novus"
INSTALL_DIR="/usr/local/bin"
BINARY_NAME="novus"

# Detect OS
OS="$(uname -s)"
case "$OS" in
    Linux)   OS_NAME="linux" ;;
    Darwin)  OS_NAME="darwin" ;;
    MINGW*|MSYS*|CYGWIN*) OS_NAME="windows" ;;
    *)
        echo "Error: Unsupported operating system: $OS"
        exit 1
        ;;
esac

# Detect architecture
ARCH="$(uname -m)"
case "$ARCH" in
    x86_64|amd64)   ARCH_NAME="amd64" ;;
    aarch64|arm64)   ARCH_NAME="arm64" ;;
    *)
        echo "Error: Unsupported architecture: $ARCH"
        exit 1
        ;;
esac

BINARY="novus-${OS_NAME}-${ARCH_NAME}"
if [ "$OS_NAME" = "windows" ]; then
    BINARY="${BINARY}.exe"
fi

echo "╔══════════════════════════════════════════╗"
echo "║       Novus Compiler — Installer         ║"
echo "╚══════════════════════════════════════════╝"
echo ""
echo "  OS:           $OS_NAME"
echo "  Architecture: $ARCH_NAME"
echo "  Binary:       $BINARY"
echo ""

# Get latest release tag
echo "Fetching latest release..."
LATEST_TAG=$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" | grep '"tag_name"' | head -1 | sed 's/.*"tag_name": *"\([^"]*\)".*/\1/')

if [ -z "$LATEST_TAG" ]; then
    echo "Warning: Could not determine latest release tag, using 'main' branch."
    DOWNLOAD_URL="https://raw.githubusercontent.com/${REPO}/main/${BINARY}"
else
    echo "  Version:      $LATEST_TAG"
    DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${LATEST_TAG}/${BINARY}"
fi

echo ""

# Download binary
TMPFILE=$(mktemp)
echo "Downloading ${BINARY}..."
if ! curl -fsSL -o "$TMPFILE" "$DOWNLOAD_URL"; then
    echo ""
    echo "Error: Failed to download from: $DOWNLOAD_URL"
    echo "Check that a binary exists for your platform (${OS_NAME}/${ARCH_NAME})."
    rm -f "$TMPFILE"
    exit 1
fi

chmod +x "$TMPFILE"

# Install
echo "Installing to ${INSTALL_DIR}/${BINARY_NAME}..."
if [ -w "$INSTALL_DIR" ]; then
    mv "$TMPFILE" "${INSTALL_DIR}/${BINARY_NAME}"
else
    echo "(requires sudo)"
    sudo mv "$TMPFILE" "${INSTALL_DIR}/${BINARY_NAME}"
    sudo chmod +x "${INSTALL_DIR}/${BINARY_NAME}"
fi

# macOS: remove quarantine attribute
if [ "$OS_NAME" = "darwin" ]; then
    sudo xattr -d com.apple.quarantine "${INSTALL_DIR}/${BINARY_NAME}" 2>/dev/null || true
fi

echo ""
echo "✓ Novus installed successfully!"
echo "  Run 'novus --help' to get started."
echo ""

# Verify
if command -v novus &>/dev/null; then
    novus 2>&1 | head -1
fi

# mac_silicon_window_manager

This is a tiny C helper used by Novus on macOS Apple Silicon.

## Build

```sh
clang -O2 -Wall -Wextra -std=c11 \
  lib/mac_silicon_window_manager/unbuilt/app.c \
  -o lib/mac_silicon_window_manager/window_manager
```

## Protocol

UNIX socket (default `/tmp/novus_wm.sock`) line-based commands:

- `TITLE <text>`
- `TEXT <text>`
- `SHOW`
- `HIDE`
- `PING` -> `PONG`
- `QUIT`

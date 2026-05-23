# Novus Library Reference

This reference covers:

1. **Official Nox ecosystem libraries** (registry-level names used in `nox pull`)
2. **Legacy in-repo libraries** under `example_old_lib/`
3. **New example libraries** under `examples/lib/`

## 1. Official Nox ecosystem libraries

These are the core libraries referenced by Novus/Nox docs:

| Library | Purpose |
|---|---|
| `std` | Core utilities (string/int conversion, printing, process exit, common helpers) |
| `file_io` | File and path operations |
| `process` | Process spawning and command execution |
| `net` | Networking primitives |
| `http` | HTTP helper layer on top of lower-level net/file/process tools |
| `maths` | Numeric helpers |
| `time` | Time and duration helpers |
| `env` | Environment variable access |
| `window` | Window/UI integration helpers |

## 2. Legacy libraries (`example_old_lib/`)

These are older, mostly macOS-Apple-Silicon-focused libraries kept for compatibility and reference.

### `standard_lib.nov`

**Purpose:** base utility helpers.

**Functions:** `str_repeat`, `char_eq`, `len`, `str_to_i32`, `int_to_str` (i32/i64 overloads), `u64_to_i32`, `i32_to_str`, `i64_to_str`.

### `standard_lib_macos_silicon.nov`

**Purpose:** Darwin ARM64 syscall/process helpers layered on top of `standard_lib`.

**Functions:** `i32_to_u64`, `i64_to_u64`, `u64_to_i64`, `array_len`, `array_len_u64`, `print`, `exit`, `get_time_ns`, `input`, `ptr`, `array_data_ptr_u64`, `execve_raw`, `fork`, `spawn_execve`, `print_raw`.

### `file_io.nov`

**Purpose:** file descriptors, file reads/writes, path helpers, mkdir/chmod helpers.

**Functions:** `file_open`, `file_open_read`, `file_open_write`, `file_close`, `file_read`, `file_write`, `file_write_str`, `file_seek`, `file_size`, `pipe_create`, `dup2`, `path_ext`, `path_stem`, `path_insert_suffix`, `path_dir`, `path_basename`, `sys_mkdir`, `sys_chmod`, `file_exists`, `file_delete`, `read_file`, `write_file`.

### `maths.nov`

**Purpose:** integer math helpers.

**Functions:** `is_even`, `is_prime`, `abs`, `max`, `min`, `clamp`, `abs64`, `max64`, `min64`.

### `memory.nov`

**Purpose:** byte/memory operations, C-string conversion, substring/search helpers.

**Functions:** `read_byte`, `cstr_len`, `make_buffer`, `copy_bytes_raw`, `copy_bytes`, `cstr_to_str`, `starts_with`, `ends_with`, `str_contains`, `str_find`, `substr`, `substr_len`, `byte_at`, `argv_get`.

### `net.nov`

**Purpose:** socket operations and polling helpers (Darwin-focused implementation).

**Functions:** `to_u8_net`, `make_sockaddr_in`, `net_socket`, `net_set_reuse`, `net_bind`, `net_listen`, `net_accept`, `net_read`, `net_write`, `net_close`, `net_set_nonblock`, `net_poll_read`, `net_make_buf`, `net_ignore_sigpipe`, `net_set_nosigpipe`, `store8`.

### `process.nov`

**Purpose:** child process management and output capture.

**Functions:** `wait_pid`, `capture_output`, `run_cmd`.

### `mac_silicon_window_manager.nov`

**Purpose:** macOS Apple Silicon window-manager client API over Unix socket protocol.

**Functions:** `to_u8`, `syscall0`, `syscall1_i32`, `syscall3_i32`, `syscall3`, `sock_path_default`, `wm_exe_default`, `wm_unlink`, `sockaddr_un_make`, `wm_socket`, `wm_connect`, `wm_close`, `wm_send_line`, `wm_escape_arg`, `wm_unescape_arg`, `wm_send_cmd`, `wm_recv_line`, `wm_recv_ok`, `wm_spawn`, `wm_write_i32_le`, `wm_sleep_ms`, `wm_start_auto`, `wm_start`, `wm_open`, `wm_title`, `wm_serve`, `wm_navigate`, `wm_jseval`, `wm_show`, `wm_hide`, `wm_ping`, `wm_quit`, `wm_escape_js`, `wm_send_to_js`, `wm_recv_js_msg`, `wm_parse_port`.

> Companion native helper source is under `example_old_lib/mac_silicon_window_manager/unbuilt/app.c`.

## 3. New example libraries (`examples/lib/`)

### `examples/lib/core/main.nov`

**Purpose:** portable utilities for examples.

**Functions:** `int_to_str`, `abs_i32`, `clamp_i32`, `repeat`, `bool_to_str`, `lcg_next`, `roll`.

### `examples/lib/game/main.nov`

**Purpose:** small game/simulation helpers.

**Functions:** `duel_delta`, `damage_from_delta`, `winner_label`, `hp_bar` (plus internal utility helpers in same module).

### `examples/lib/term/main.nov`

**Purpose:** platform-specific terminal output/exit abstraction.

**Public functions:** `println`, `print_key_value`, `write_raw`, `exit_with`.

**Platform blocks:**
- `#if(os == "linux")` uses Linux syscall-based writer/exit.
- `#if(os == "darwin")` uses Darwin syscall-based writer/exit.
- `#if(os == "windows")` uses `win_call` (`GetStdHandle`, `WriteConsoleA`, `ExitProcess`).

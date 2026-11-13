# process

Process control: spawn, wait, kill, pipe, exec.

This document is auto-generated from the function signatures in this repository.
It lists every public function the library exposes.

## Install

```sh
nox pull process
```

## Import

```novus
import lib/process process;
```

## Functions

### `capture_output`

```novus
fn capture_output(exe: str, argv: []u64, max_len: i32) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `capture_output_full`

```novus
fn capture_output_full(exe: str, argv: []u64, max_len: i32) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `pick_file_dialog`

```novus
fn pick_file_dialog(file_type: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `pick_folder_dialog`

```novus
fn pick_folder_dialog() -> str;
```
_Defined in: `darwin_arm64.nov`_

### `run_cmd`

```novus
fn run_cmd(exe: str, argv: []u64) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `shell_exec`

```novus
fn shell_exec(cmd: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `shell_output`

```novus
fn shell_output(cmd: str, max_len: i32) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `wait_pid`

```novus
fn wait_pid(pid: i32) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `waitpid_nohang`

```novus
fn waitpid_nohang() -> void;
```
_Defined in: `darwin_arm64.nov`_

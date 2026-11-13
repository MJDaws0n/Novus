# file_io

File system I/O: read, write, delete, exists, mkdir, list.

This document is auto-generated from the function signatures in this repository.
It lists every public function the library exposes.

## Install

```sh
nox pull file_io
```

## Import

```novus
import lib/file_io file_io;
```

## Functions

### Whole-file text and binary I/O

`read_all_text(path)` returns a `FileTextResult` with `data: str` and `error: str`.
It reads through the platform's file APIs and preserves the bytes, including
embedded zero bytes. Ordinary string functions see the prefix before the first
zero. `write_all_text(path, text)` writes a zero-terminated string and returns
an empty string on success or an error description. Both helpers handle partial
reads/writes and close the file on success and failure.

Use `read_all_bytes(path)` and `write_all_bytes(path, buffer)` when binary data
needs an explicit length; their `ByteBuffer` representation includes zero bytes.
The text helpers avoid expanding each source/assembly byte into an integer
array when the compiler reads or writes text.

### `at_fdcwd`

```novus
fn at_fdcwd() -> i32;
```
_Defined in: `linux_arm64.nov`_

### `dir_exists`

```novus
fn dir_exists(path: str) -> bool;
```
_Defined in: `darwin_arm64.nov`_

### `dup2`

```novus
fn dup2(oldfd: i32, newfd: i32) -> void;
```
_Defined in: `darwin_arm64.nov`_

### `file_append`

```novus
fn file_append(path: str, content: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_chdir`

```novus
fn file_chdir(path: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_close`

```novus
fn file_close(fd: i32) -> void;
```
_Defined in: `darwin_arm64.nov`_

### `file_copy`

```novus
fn file_copy(src_path: str, dst_path: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_delete`

```novus
fn file_delete(path: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_exists`

```novus
fn file_exists(path: str) -> bool;
```
_Defined in: `darwin_arm64.nov`_

### `file_mmap_read`

```novus
fn file_mmap_read(fd: i32, size: u64) -> u64;
```
_Defined in: `darwin_arm64.nov`_

### `file_munmap`

```novus
fn file_munmap(addr: u64, size: u64) -> void;
```
_Defined in: `darwin_arm64.nov`_

### `file_open`

```novus
fn file_open(path: str, flags: i32, mode: i32) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_open_read`

```novus
fn file_open_read(path: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_open_write`

```novus
fn file_open_write(path: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_read`

```novus
fn file_read(fd: i32, buf_addr: u64, count: i32) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_rename`

```novus
fn file_rename(old_path: str, new_path: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_seek`

```novus
fn file_seek(fd: i32, offset: u64, whence: i32) -> i64;
```
_Defined in: `darwin_arm64.nov`_

### `file_size`

```novus
fn file_size(fd: i32) -> u64;
```
_Defined in: `darwin_arm64.nov`_

### `file_write`

```novus
fn file_write(fd: i32, buf_addr: u64, count: i32) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `file_write_str`

```novus
fn file_write_str(fd: i32, s: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `mkdirp`

```novus
fn mkdirp(path: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `path_basename`

```novus
fn path_basename(path: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `path_dir`

```novus
fn path_dir(path: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `path_ext`

```novus
fn path_ext(path: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `path_ext_no_dot`

```novus
fn path_ext_no_dot(path: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `path_insert_suffix`

```novus
fn path_insert_suffix(path: str, suffix: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `path_join`

```novus
fn path_join(a: str, b: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `path_stem`

```novus
fn path_stem(path: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `pipe_create`

```novus
fn pipe_create(res: []i32) -> void;
```
_Defined in: `darwin_arm64.nov`_

### `read_file`

```novus
fn read_file(path: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `sys_chmod`

```novus
fn sys_chmod(path: str, mode: i32) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `sys_mkdir`

```novus
fn sys_mkdir(path: str, mode: i32) -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `write_file`

```novus
fn write_file(path: str, content: str) -> i32;
```
_Defined in: `darwin_arm64.nov`_

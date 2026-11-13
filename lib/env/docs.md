# env

Reading and writing environment variables and home/user info.

This document is auto-generated from the function signatures in this repository.
It lists every public function the library exposes.

## Install

```sh
nox pull env
```

## Import

```novus
import lib/env env;
```

## Functions

### `get_arch`

```novus
fn get_arch() -> str;
```
_Defined in: `darwin_arm64.nov`_

### `get_home_dir`

```novus
fn get_home_dir() -> str;
```
_Defined in: `darwin_arm64.nov`_

### `get_os_name`

```novus
fn get_os_name() -> str;
```
_Defined in: `darwin_arm64.nov`_

### `get_os_version`

```novus
fn get_os_version() -> str;
```
_Defined in: `darwin_arm64.nov`_

### `get_username`

```novus
fn get_username() -> str;
```
_Defined in: `darwin_arm64.nov`_

### `getcwd`

```novus
fn getcwd() -> str;
```
_Defined in: `darwin_arm64.nov`_

### `getenv`

```novus
fn getenv(name: str) -> str;
```
_Defined in: `darwin_arm64.nov`_

### `geteuid`

```novus
fn geteuid() -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `gethostname`

```novus
fn gethostname() -> str;
```
_Defined in: `darwin_arm64.nov`_

### `getpid`

```novus
fn getpid() -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `getppid`

```novus
fn getppid() -> i32;
```
_Defined in: `darwin_arm64.nov`_

### `getuid`

```novus
fn getuid() -> i32;
```
_Defined in: `darwin_arm64.nov`_

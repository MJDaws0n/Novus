# Novus
**NOTE: ONLY WORKS PROPERLY ON MACOS SILICON (darwin_arm64). - Windows comming soon!**

*This may be used for my a-level project, therefore there are many references to clients and such.*

The aim in the short future is to re-write novus, in novus!

# Quick install
1. Download the latest from the realeases tab.
2. Run the following command to install gobally (macos) (replace novus_download with the directy of the executable)
```sh
sudo cp "novus_download" /usr/local/bin/novus
sudo chmod +x /usr/local/bin/novus
```

# Build your first app
It's suggested you use the offical [novus package manager nox](https://github.com/mjdaws0n/nox), however it's not required.

## Using nox
1. Download and install [nox](https://github.com/mjdaws0n/nox)
2. Run the following command in the folder you wish to use
'''sh
nox init
'''
3. Import the standard library
```sh
nox pull std
```
4. Write your first app by edditing the main.nov file
```novus
module novus_example

// Import standard lib as std
// Use import lib/std; to import without std prefix
import lib/std std;

fn main() -> i32 {
    // Print something
    std.print("Hello World!");

    // Exit program
    std.exit(0);
    return 0;
}
```
5. Build to an executable
```sh
novus main.nov
```

## Without using nox - with libraries
1. Download the std package, or copy the functions for the respective operating system from [here](https://github.com/MJDaws0n/novus-std)
2. Create a novus file and add your code.
```novus
module novus_example

// Import standard lib as std
// Use import lib/std; to import without std prefix
import lib/std std;

fn main() -> i32 {
    // Print something
    std.print("Hello World!");

    // Exit program
    std.exit(0);
    return 0;
}
```
3. Build to an exectuabl
```sh
novus your_file.nov
```

## Without using nox - without libraries
1. Create a novus file and add your code.
```novus
module novus_example

fn main() -> i32 {
    // Print something
    print("Hello World!");

    // Exit program
    exit(0);
    return 0;
}

// Define a print function for macos silicon
fn print(msg: str) -> void {
    msg = msg + "\n";
    mov(x0, 1);
    mov(x1, msg);
    mov(x2, len(msg));
    mov(x16, 0x2000004);
    syscall();
}

// Deffine an exit function for macos silicon
fn exit(code: i32) -> void {
    mov(x0, code);
    mov(x16, 0x2000001);
    syscall();
}
```
2. Build to an exectuable
```sh
novus your_file.nov
```

# Novus syntax and built in functions
Take a look at novus syntax and built in functions [here](/mjdaws0n/novus/novus_docs.md)
Take a look at novus libraries [here](/mjdaws0n/nox/registry.txt)

## Existing projects in novus
- Manifold Edge Remove - Fixes STL files so they can be 3d printed. Perfect for novus as it needs to be done quickly. Uses novus's window library for rendering a window.
https://github.com/MJDaws0n/Manifold-Edge-Remover-V2

# Build and run
```sh
go run cmd/novus/main.go --target=darwin/arm64 novus-examples/example.nov
```

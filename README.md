# Novus
**NOTE: ONLY WORKS PROPERLY ON MACOS SILICON (darwin_arm64). - Windows comming soon!**

*This may be used for my a-level project, therefore there are many references to clients and such.*

The aim in the short future is to re-write novus, in novus!

# Quick install
1. Download the latest from the realeases tab.
2. Run the following command to install gobally (replace novus with the directy of the executable)
```sh
sudo cp "novus" /usr/local/bin/novus
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
4. Write your first app.
```novus
module novus_example

// Import standard lib as std
// Use import lib/std; so import without std prefix
import lib/std std;

fn main() -> i32 {
    // Print something
    std.print("Hello World!");

    // Exit program
    std.exit(0);
    return 0;
}
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

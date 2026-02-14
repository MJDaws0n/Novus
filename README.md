# Novus
**NOTE: ONLY WORKS PROPERLY ON MACOS SILICON (darwin_arm64). - Windows comming soon!**

*This may be used for my a-level project, therefore there are many references to clients and such.*

## Existing projects in novus
Manifold Edge Remove - Fixes STL files so they can be 3d printed. Perfect for novus as it needs to be done quickly. Uses novus's window library for rendering a window.
https://github.com/MJDaws0n/Manifold-Edge-Remover-V2

# Build and run
```sh
go run cmd/novus/main.go --target=darwin/arm64 novus-examples/example.nov
```
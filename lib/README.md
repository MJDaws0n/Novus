# Bundled compiler dependencies

These Novus libraries are included so a clean checkout can bootstrap without local symlinks or a package-manager installation. The compiler uses their public file, process, text, number, environment, archive and download APIs.

Upstream revisions before the self-hosting additions:

- std: `577118e8d7ef9e988c53fb807d2de0f1df87a2a3`

- file_io: `6e3543bfcb6d4c0b79333ec9a965574cd4869d54`

- process: `8f6e072f636ff8c74fdfb5ea65b2d6e43538ccf3`

- env: `161a8e15546ba1b0ca5aca842686559de7eac1d2`


The archive library uses the operating system ZIP utility (PowerShell Expand-Archive on Windows, unzip on POSIX). Downloads use URLMon on Windows and curl on POSIX. Both report failures through their APIs.

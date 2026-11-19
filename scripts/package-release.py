#!/usr/bin/env python3
"""Package a release only when every binary matches its passing CI records."""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import statistics
import subprocess
import zipfile

ROOT = Path(__file__).resolve().parents[1]
TARGETS = [
    ("darwin/arm64", "darwin_arm64", "macOS ARM64"),
    ("darwin/amd64", "darwin_x86_64", "macOS Intel"),
    ("linux/amd64", "linux_x86_64", "Linux x64"),
    ("linux/arm64", "linux_arm64", "Linux ARM64"),
    ("windows/amd64", "windows_x86_64", "Windows x64"),
    ("windows/arm64", "windows_arm64", "Windows ARM64"),
    ("linux/386", "linux_x86", "Linux x86 (32-bit)"),
]


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def source_digest(root, windows=False, normalized_paths=False):
    result = hashlib.sha256()
    paths = [root / "main.nov", *sorted((root / "src").glob("*.nov")),
             *sorted((root / "lib").rglob("*.nov"))]
    for path in paths:
        name = path.relative_to(root).as_posix()
        data = path.read_bytes().replace(b"\r\n", b"\n")
        if windows:
            data = data.replace(b"\n", b"\r\n")
            if not normalized_paths:
                name = name.replace("/", "\\")
        result.update(name.encode())
        result.update(data)
    return result.hexdigest()


def package(args):
    root, records, output = args.source_root.resolve(), args.records.resolve(), args.output.resolve()
    require(re.fullmatch(r"v\d+\.\d+\.\d+", args.version), "Use a stable version such as v0.4.0")
    commit = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip()
    require(commit == args.commit, "The release checkout does not match the requested commit")
    require(subprocess.run(["git", "diff", "--quiet", "HEAD", "--", "main.nov", "src", "lib"], cwd=root).returncode == 0,
            "Compiler or library source has uncommitted changes")
    version = args.version[1:]
    require(f'Novus Compiler V{version}"' in (root / "src/driver_main.nov").read_text(), "Native compiler version mismatch")
    require(f'const VERSION = "{version}"' in (root / "legacy-go/cmd/novus/main.go").read_text(), "Bootstrap version mismatch")
    hashes = {(windows, normalized): source_digest(root, windows, normalized)
              for windows in (False, True) for normalized in (False, True)}
    targets, assets, rows = [], [], []
    for index, (target, folder, label) in enumerate(TARGETS):
        verification = json.loads((records / "verification" / f"selfhost-{index}" / folder / "results.json").read_text())
        performance = json.loads((records / "performance" / f"performance-{index}" / "results.json").read_text())
        require(verification["target"] == performance["target"] == target, f"Target mismatch: {target}")
        require(verification.get("passed") and performance.get("passed"), f"Verification failed: {target}")
        stages = verification["stages"]
        require([s["stage"] for s in stages] == [1, 2, 3], f"Missing compiler generations: {target}")
        require(stages[1]["assembly_sha256"] == stages[2]["assembly_sha256"], f"Compiler reproduction failed: {target}")
        comparisons = verification["comparisons"]
        require(comparisons["passed"] > 0 and comparisons["failed"] == 0, f"Differential checks failed: {target}")
        require(verification["tests"] and all(t["passed"] for t in verification["tests"]), f"Runtime checks failed: {target}")
        windows = target.startswith("windows/")
        require(verification["source_sha256"] == hashes[windows, False], f"Verification source mismatch: {target}")
        require(performance["compiler_source_sha256"] == hashes[windows, True], f"Benchmark source mismatch: {target}")
        require(performance["warmups"] == 1 and performance["measured_runs"] >= 6
                and performance["maximum_large_ratio"] == 1.25, f"Incomplete performance measurement: {target}")
        ratios = []
        for size in ("large", "compiler"):
            application = performance["applications"][size]
            samples = application["seconds"]
            require(all(len(samples[name]) == performance["measured_runs"]
                        and all(math.isfinite(x) and x > 0 for x in samples[name])
                        for name in ("go", "novus")), f"Invalid samples: {target}/{size}")
            ratio = statistics.median(samples["novus"]) / statistics.median(samples["go"])
            require(abs(ratio - application["novus_go_ratio"]) < 1e-9 and ratio <= 1.25,
                    f"Performance limit exceeded: {target}/{size}: {ratio:.3f}x")
            ratios.append(ratio)
        filename = "novus-" + target.replace("/", "-") + (".exe" if windows else "")
        binary = output / filename
        require(binary.is_file(), f"Missing binary: {filename}")
        binary_hash = digest(binary)
        require(binary_hash == performance["compiler_sha256"]["novus"], f"Binary hash mismatch: {target}")
        assets.append(binary)
        targets.append({"target": target, "binary": filename, "binary_sha256": binary_hash,
                        "verification": verification, "performance": performance})
        rows.append(f"| {label} | {ratios[0]:.3f}× | {ratios[1]:.3f}× |")

    # No output is changed until all seven platforms have been validated.
    tracked = subprocess.check_output(["git", "ls-files", "-z", "--", "lib"], cwd=root).decode().split("\0")
    library_files = [root / name for name in tracked if name and (root / name).is_file()]
    require(library_files and root / "lib/std/main.nov" in library_files, "Bundled standard library is missing")
    libraries = output / f"novus-libraries-{args.version}.zip"
    with zipfile.ZipFile(libraries, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=9) as archive:
        for path in sorted(library_files):
            item = zipfile.ZipInfo(path.relative_to(root).as_posix(), (1980, 1, 1, 0, 0, 0))
            item.compress_type = zipfile.ZIP_DEFLATED
            item.external_attr = 0o100644 << 16
            archive.writestr(item, path.read_bytes())
    assets.append(libraries)
    proof = output / f"novus-verification-{args.version}.json"
    proof.write_text(json.dumps({"version": args.version, "commit": commit,
                                "workflow_run": args.workflow_url, "maximum_large_ratio": 1.25,
                                "targets": targets}, indent=2) + "\n")
    assets.append(proof)
    (output / "SHA256SUMS").write_text("".join(f"{digest(path)}  {path.name}\n" for path in sorted(assets)))
    comparisons = sum(t["verification"]["comparisons"]["passed"] for t in targets)
    runtime_checks = sum(len(t["verification"]["tests"]) for t in targets)
    notes = f"""Novus now runs as a compiler written in Novus across all seven supported platforms, including 32-bit Linux. The preserved Go implementation is available for bootstrapping and comparison tests.

This release includes full-width arithmetic on 32-bit Linux, portable file/process/environment/download/archive libraries, and compiler correctness and performance improvements.

Download the binary for your OS and architecture below. The existing installer selects the matching binary from this release. The library ZIP contains the matching `lib/` directory; source builds are available in GitHub's source archives.

Both large compilation benchmarks passed the 1.25× Go-time limit, with one excluded warm-up and six alternating measurements per compiler. Lower ratios are faster.

| Host | Nox / Go | Compiler / Go |
| --- | ---: | ---: |
""" + "\n".join(rows) + f"""

Verification passed: 21 compiler builds, {comparisons} differential comparisons and {runtime_checks} runtime checks, plus the Go suite, first-generation smoke tests and 32-bit cross-compilation tests. Compiler generations two and three emit identical assembly on every host. These timings describe the measured workloads.

`SHA256SUMS` covers all binaries, the library ZIP and the verification JSON. The JSON includes raw benchmark samples and source/binary hashes.

Verified source: `{commit}`. [Full verification run]({args.workflow_url}).
"""
    (output / "release-notes.md").write_text(notes)
    print(f"Verified {len(targets)} platforms; packaged {len(assets)} assets and SHA256SUMS")


def verify_uploaded(args):
    release = json.loads(subprocess.check_output(
        ["gh", "release", "view", args.version, "--json", "assets,tagName,isDraft,targetCommitish"],
        cwd=args.source_root, text=True))
    require(release["isDraft"], "The release must remain a draft until its uploads are verified")
    require(release["tagName"] == args.version and release["targetCommitish"] == args.commit,
            "Uploaded release version or commit mismatch")
    checksums = args.output / "SHA256SUMS"
    expected = {name: "sha256:" + checksum for checksum, name
                in (line.split("  ", 1) for line in checksums.read_text().splitlines())}
    expected["SHA256SUMS"] = "sha256:" + digest(checksums)
    uploaded = {asset["name"]: asset for asset in release["assets"]}
    require(set(uploaded) == set(expected), "The uploaded asset list is incomplete or contains unexpected files")
    for name, checksum in expected.items():
        require(uploaded[name]["state"] == "uploaded" and uploaded[name].get("digest") == checksum,
                f"Uploaded file hash mismatch: {name}")
    print(f"Verified all {len(expected)} uploaded files against their local SHA-256 hashes")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--version", required=True)
    parser.add_argument("--commit", required=True)
    parser.add_argument("--workflow-url", required=True)
    parser.add_argument("--source-root", type=Path, default=ROOT)
    parser.add_argument("--records", type=Path, default=ROOT / "build/release")
    parser.add_argument("--output", type=Path, default=ROOT / "dist")
    parser.add_argument("--verify-upload", action="store_true", help="Verify a draft release before publishing it")
    arguments = parser.parse_args()
    if arguments.verify_upload:
        verify_uploaded(arguments)
    else:
        package(arguments)

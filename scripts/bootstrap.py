#!/usr/bin/env python3
"""Build three compiler generations and verify the native runtime suite."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser()
parser.add_argument("--bootstrap", help="Existing Go bootstrap executable")
parser.add_argument("--target", help="Native host target, e.g. linux/amd64")
parser.add_argument("--test-only", help="Test an existing Novus compiler")
parser.add_argument("--package", action="store_true", help="Copy the verified native compiler into dist/")
args = parser.parse_args()
host_os = {"Darwin": "darwin", "Linux": "linux", "Windows": "windows"}[platform.system()]
host_arch = {"aarch64": "arm64", "arm64": "arm64", "amd64": "amd64", "x86_64": "amd64", "i686": "x86"}[platform.machine().lower()]
target = args.target or f"{host_os}/{host_arch}"
target_os, target_arch = target.split("/")
artifact_arch = {"amd64": "x86_64", "386": "x86"}.get(target_arch, target_arch)
extension = ".exe" if target_os == "windows" else ""
output = ROOT / "build" / f"{target_os}_{artifact_arch}"
work = ROOT / "build" / "selfhost" / f"{target_os}_{artifact_arch}"
work.mkdir(parents=True, exist_ok=True)
report = {"target": target, "stages": [], "tests": []}
flags = [f"--target={target}", "--heap-size=1g", "--gc-entries=8388608"]

def run(command, name, timeout=600, expected=0, cwd=ROOT, env=None):
    started = time.perf_counter()
    result = subprocess.run([str(x) for x in command], cwd=cwd, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=timeout)
    (work / (name + ".log")).write_bytes(result.stdout)
    elapsed = time.perf_counter() - started
    if result.returncode != expected:
        print(result.stdout.decode(errors="replace")[-12000:], flush=True)
        raise RuntimeError(f"{name}: exit {result.returncode}, expected {expected}")
    return elapsed

def record():
    (work / "results.json").write_text(json.dumps(report, indent=2) + "\n")

try:
    if args.test_only:
        compiler = Path(args.test_only).resolve()
    else:
        if args.bootstrap:
            compiler = Path(args.bootstrap).resolve()
        else:
            compiler = work / ("bootstrap-go" + extension)
            run(["go", "build", "-o", compiler, "./cmd/novus"], "bootstrap-go", cwd=ROOT / "legacy-go")
        previous_assembly = None
        for stage in range(1, 4):
            elapsed = run([compiler, *flags, "main.nov"], f"stage{stage}", timeout=1200)
            produced = output / ("novus" + extension)
            if not produced.is_file():
                raise RuntimeError(f"stage {stage} did not produce an executable")
            compiler = work / (f"stage{stage}" + extension)
            # Replace the inode: overwriting an executed Mach-O in place can
            # leave macOS with a cached code signature for the old executable.
            temporary = compiler.with_name(compiler.name + ".new")
            shutil.copy2(produced, temporary)
            temporary.replace(compiler)
            asm_ext = ".asm" if target_os == "windows" and target_arch == "amd64" else ".s"
            assembly = (output / ("novus" + asm_ext)).read_bytes()
            digest = hashlib.sha256(assembly).hexdigest()
            report["stages"].append({"stage": stage, "seconds": elapsed, "assembly_sha256": digest})
            record()
            print(f"Stage {stage}: {elapsed:.3f}s", flush=True)
            if stage == 3 and assembly != previous_assembly:
                raise RuntimeError("Stage 2 and stage 3 produced different assembly")
            previous_assembly = assembly
        print("Stage 2 and stage 3 assembly is identical", flush=True)
    fixtures = [("tests/classes/main.nov", "classes"),
                ("tests/selfhost/compiler_regressions.nov", "compiler_regressions"),
                ("tests/selfhost/register_arguments.nov", "register_arguments"),
                ("tests/selfhost/gc_graph.nov", "gc_graph"),
                ("tests/selfhost/time_memory.nov", "time_memory"),
                ("tests/selfhost/libraries.nov", "library_smoke"),
                ("tests/selfhost/numeric_features.nov", "numeric_features"),
                ("tests/selfhost/process_child.nov", "process_child"),
                ("tests/selfhost/platform_libraries.nov", "platform_libraries")]
    for source, name in fixtures:
        run([compiler, f"--target={target}", source], name + "-build")
        if name == "process_child":
            continue
        command = [output / (name + extension)]
        environment = os.environ.copy()
        if name == "platform_libraries":
            command.append(output / ("process_child" + extension))
            environment["NOVUS_TEST_ENV"] = "inherited environment"
        elapsed = run(command, name + "-run", timeout=60, env=environment)
        report["tests"].append({"name": name, "passed": True, "seconds": elapsed})
        record()
        print(f"PASS {name}", flush=True)
    run([compiler, f"--target={target}", "--gc-entries=1024", "tests/selfhost/gc_graph.nov"], "gc-small-table-build")
    run([output / ("gc_graph" + extension)], "gc-small-table-run", timeout=60)
    report["tests"].append({"name": "gc-small-table", "passed": True})
    report["passed"] = True
    if args.package:
        release_arch = "386" if target_arch in ("386", "x86") else target_arch
        destination = ROOT / "dist" / f"novus-{target_os}-{release_arch}{extension}"
        destination.parent.mkdir(parents=True, exist_ok=True)
        temporary = destination.with_name(destination.name + ".new")
        shutil.copy2(compiler, temporary)
        temporary.replace(destination)
        report["artifact"] = str(destination.relative_to(ROOT))
        print(f"Packaged {destination}", flush=True)
finally:
    record()

#!/usr/bin/env python3
"""Build three compiler generations and verify the native runtime suite."""
import argparse
import hashlib
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from functools import partial
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import time
import threading
import zipfile

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

def source_digest():
    digest = hashlib.sha256()
    paths = [ROOT / "main.nov", *sorted((ROOT / "src").glob("*.nov")), *sorted((ROOT / "lib").rglob("*.nov"))]
    for path in paths:
        digest.update(str(path.relative_to(ROOT)).encode())
        digest.update(path.read_bytes())
    return digest.hexdigest()

report["source_sha256"] = source_digest()

def run(command, name, timeout=600, expected=0, cwd=ROOT, env=None):
    started = time.perf_counter()
    log = work / (name + ".log")
    with log.open("wb") as output_log:
        try:
            result = subprocess.run([str(x) for x in command], cwd=cwd, env=env, stdout=output_log, stderr=subprocess.STDOUT, timeout=timeout)
        except subprocess.TimeoutExpired:
            print(log.read_bytes().decode(errors="replace")[-12000:], flush=True)
            raise
    elapsed = time.perf_counter() - started
    if result.returncode != expected:
        print(log.read_bytes().decode(errors="replace")[-12000:], flush=True)
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
            if source_digest() != report["source_sha256"]:
                raise RuntimeError("Compiler sources changed during verification; restart with a stable source tree")
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
            if stage == 1:
                run([compiler, f"--target={target}", "--debug", "tests/classes/main.nov"], "stage1-smoke-build", timeout=120)
                run([output / ("classes" + extension)], "stage1-smoke-run", timeout=60)
                print("PASS first-generation compiler smoke test", flush=True)
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
                ("tests/selfhost/float_rounding.nov", "float_rounding"),
                ("tests/selfhost/float_literals.nov", "float_literals"),
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
    run([compiler, f"--target={target}", "tests/selfhost/download_archive.nov"], "download-archive-build")
    downloads = work / "download-fixtures"
    downloads.mkdir(exist_ok=True)
    with zipfile.ZipFile(downloads / "fixture.zip", "w") as archive:
        archive.writestr("nested/message.txt", "download and archive passed\n")
    class QuietHandler(SimpleHTTPRequestHandler):
        def log_message(self, *_):
            pass
    server = ThreadingHTTPServer(("127.0.0.1", 0), partial(QuietHandler, directory=str(downloads)))
    server_thread = threading.Thread(target=server.serve_forever, daemon=True)
    server_thread.start()
    try:
        run([output / ("download_archive" + extension), f"http://127.0.0.1:{server.server_port}", downloads], "download-archive-run", timeout=60)
    finally:
        server.shutdown()
        server.server_close()
        server_thread.join()
    report["tests"].append({"name": "download-archive", "passed": True})
    print("PASS download and archive libraries", flush=True)
    if not args.test_only and not args.bootstrap:
        run([sys.executable, ROOT / "scripts" / "differential.py", "--reference", work / ("bootstrap-go" + extension),
             "--compiler", compiler, "--target", target], "differential", timeout=600)
        compared = json.loads((ROOT / "build" / "differential" / f"{target_os}_{artifact_arch}" / "results.json").read_text())
        report["comparisons"] = {"passed": compared["passed"], "failed": compared["failed"]}
        print(f"PASS {compared['passed']} compiler comparisons", flush=True)
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

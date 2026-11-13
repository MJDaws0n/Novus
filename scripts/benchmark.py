#!/usr/bin/env python3
"""Compare fresh-process compilation of identical inputs and enforce a time ratio."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import platform
import re
import statistics
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser()
parser.add_argument("--reference", required=True)
parser.add_argument("--compiler", required=True)
parser.add_argument("--large", required=True, help="Large application's main.nov, using the same bundled libraries")
parser.add_argument("--include-self", action="store_true", help="Also benchmark the full compiler sources")
parser.add_argument("--target", help="Target shared by both compilers, e.g. linux/386")
parser.add_argument("--runs", type=int, default=6)
parser.add_argument("--max-ratio", type=float, help="Maximum Novus/Go median time for each large workload")
parser.add_argument("--output", help="JSON report location")
args = parser.parse_args()
if args.runs < 3:
    parser.error("Use at least three measured runs")
work = ROOT / "build" / "benchmarks"
work.mkdir(parents=True, exist_ok=True)
report_path = Path(args.output).resolve() if args.output else work / "results.json"
report_path.parent.mkdir(parents=True, exist_ok=True)
compilers = {"go": Path(args.reference).resolve(), "novus": Path(args.compiler).resolve()}
sources = {"small": ROOT / "tests/classes/main.nov", "large": Path(args.large).resolve()}
if args.include_self:
    sources["compiler"] = ROOT / "main.nov"
report = {"recorded_at": datetime.now(timezone.utc).isoformat(), "host": platform.platform(),
          "target": args.target, "warmups": 1, "measured_runs": args.runs,
          "maximum_large_ratio": args.max_ratio,
          "includes": "fresh process, parsing, imports, semantic analysis, assembly generation, assembly and linking",
          "compiler_sha256": {name: hashlib.sha256(path.read_bytes()).hexdigest() for name, path in compilers.items()},
          "applications": {}}

def source_digest():
    digest = hashlib.sha256()
    for path in [ROOT / "main.nov", *sorted((ROOT / "src").glob("*.nov")), *sorted((ROOT / "lib").rglob("*.nov"))]:
        digest.update(path.relative_to(ROOT).as_posix().encode())
        digest.update(path.read_bytes())
    return digest.hexdigest()

report["compiler_source_sha256"] = source_digest()
dependencies = hashlib.sha256()
for path in sorted((ROOT / "lib").rglob("*.nov")):
    dependencies.update(path.relative_to(ROOT).as_posix().encode())
    dependencies.update(path.read_bytes())
report["bundled_libraries_sha256"] = dependencies.hexdigest()
failed = []
for size, source in sources.items():
    samples = {"go": [], "novus": []}
    source_hash = hashlib.sha256(source.read_bytes()).hexdigest()
    flags = [f"--target={args.target}"] if args.target else []
    if size == "compiler":
        flags += ["--heap-size=1g", "--gc-entries=8388608"]
    module = re.search(r"\bmodule\s+(\w+)\s*;", source.read_text()).group(1)
    for iteration in range(args.runs + 1):
        order = ("go", "novus") if iteration % 2 == 0 else ("novus", "go")
        for label in order:
            started = time.perf_counter()
            result = subprocess.run([str(compilers[label]), *flags, str(source)], cwd=ROOT, capture_output=True, timeout=1200)
            elapsed = time.perf_counter() - started
            log = result.stdout + result.stderr
            (work / f"{size}-{label}-{iteration}.log").write_bytes(log)
            if result.returncode != 0 or b"Binary:" not in log:
                raise RuntimeError(f"{size}/{label} did not produce a binary: see its benchmark log")
            if iteration > 0:
                samples[label].append(elapsed)
                print(f"{size} {label}: {elapsed:.3f}s", flush=True)
    if hashlib.sha256(source.read_bytes()).hexdigest() != source_hash or source_digest() != report["compiler_source_sha256"]:
        raise RuntimeError("Inputs changed during benchmarking")
    medians = {name: statistics.median(values) for name, values in samples.items()}
    ratio = medians["novus"] / medians["go"]
    passed = size == "small" or args.max_ratio is None or ratio <= args.max_ratio
    if not passed:
        failed.append(size)
    report["applications"][size] = {"source": str(source), "source_sha256": source_hash,
        "seconds": samples, "median_seconds": medians, "novus_go_ratio": ratio, "passed": passed}
    report["passed"] = not failed
    report_path.write_text(json.dumps(report, indent=2) + "\n")
    print(f"{size}: {ratio:.3f}x Go ({'PASS' if passed else 'FAIL'})", flush=True)
if failed:
    raise SystemExit("Performance limit exceeded: " + ", ".join(failed))

#!/usr/bin/env python3
"""Measure complete, fresh-process compilations with identical application inputs."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import platform
import statistics
import subprocess
import time

ROOT = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser()
parser.add_argument("--reference", required=True)
parser.add_argument("--compiler", required=True)
parser.add_argument("--large", required=True, help="Large application's main.nov")
parser.add_argument("--runs", type=int, default=6)
args = parser.parse_args()
work = ROOT / "build" / "benchmarks"
work.mkdir(parents=True, exist_ok=True)
compilers = {"go": Path(args.reference).resolve(), "novus": Path(args.compiler).resolve()}
sources = {"small": ROOT / "tests" / "classes" / "main.nov", "large": Path(args.large).resolve()}
report = {"recorded_at": datetime.now(timezone.utc).isoformat(), "host": platform.platform(),
          "includes": "fresh process, parsing, imports, semantic analysis, assembly generation, assembly and linking",
          "compiler_sha256": {name: hashlib.sha256(path.read_bytes()).hexdigest() for name, path in compilers.items()},
          "applications": {}}
for size, source in sources.items():
    samples = {"go": [], "novus": []}
    for iteration in range(args.runs + 1):
        order = ("go", "novus") if iteration % 2 == 0 else ("novus", "go")
        for label in order:
            started = time.perf_counter()
            result = subprocess.run([str(compilers[label]), str(source)], cwd=ROOT, capture_output=True, timeout=600)
            elapsed = time.perf_counter() - started
            (work / f"{size}-{label}-{iteration}.log").write_bytes(result.stdout + result.stderr)
            if result.returncode != 0:
                raise RuntimeError(f"{size}/{label} failed: see its benchmark log")
            if iteration > 0:
                samples[label].append(elapsed)
                print(f"{size} {label}: {elapsed:.3f}s", flush=True)
    report["applications"][size] = {"source": str(source), "source_sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
                                    "seconds": samples, "median_seconds": {name: statistics.median(values) for name, values in samples.items()}}
    (work / "results.json").write_text(json.dumps(report, indent=2) + "\n")
print(json.dumps({name: result["median_seconds"] for name, result in report["applications"].items()}, indent=2))

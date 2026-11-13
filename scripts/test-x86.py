#!/usr/bin/env python3
"""Run 32-bit cross-compiled regression fixtures on a Linux x64 host."""
import argparse
import json
from pathlib import Path
import subprocess

root = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser()
parser.add_argument("--compiler", required=True)
args = parser.parse_args()
compiler = Path(args.compiler).resolve()
work = root / "build" / "selfhost" / "linux_x86"
work.mkdir(parents=True, exist_ok=True)
results = []
for source, name in (("tests/classes/main.nov", "classes"),
                     ("tests/selfhost/register_arguments.nov", "register_arguments"),
                     ("tests/selfhost/flags.nov", "flags"),
                     ("tests/selfhost/x86_compat.nov", "x86_compat"),
                     ("tests/selfhost/wide_values.nov", "wide_values"),
                     ("tests/selfhost/numeric_features.nov", "numeric_features")):
    for phase, command in (("build", [compiler, "--target=linux/386", "--gc-entries=1024", source]),
                           ("run", [root / "build" / "linux_x86" / name])):
        completed = subprocess.run([str(x) for x in command], cwd=root, capture_output=True, timeout=120)
        (work / f"{name}-{phase}.log").write_bytes(completed.stdout + completed.stderr)
        if completed.returncode:
            raise RuntimeError(f"{name}/{phase}: exit {completed.returncode}")
    results.append({"name": name, "passed": True})
    print(f"PASS linux/386 {name}", flush=True)
(work / "results.json").write_text(json.dumps({"target": "linux/386", "tests": results}, indent=2) + "\n")

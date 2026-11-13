#!/usr/bin/env python3
"""Compare Novus with the Go bootstrap using the preserved compiler's fixtures."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser()
parser.add_argument("--reference", required=True)
parser.add_argument("--compiler", required=True)
parser.add_argument("--target", required=True)
args = parser.parse_args()
reference = Path(args.reference).resolve()
compiler = Path(args.compiler).resolve()
target_os, target_arch = args.target.split("/")
artifact_arch = {"amd64": "x86_64", "386": "x86"}.get(target_arch, target_arch)
output = ROOT / "build" / f"{target_os}_{artifact_arch}"
work = ROOT / "build" / "differential" / f"{target_os}_{artifact_arch}"
work.mkdir(parents=True, exist_ok=True)
fixtures = {}
for source in sorted((ROOT / "legacy-go" / "internal").rglob("*_test.go")):
    for match in re.finditer(r"`(\s*module\s+[\s\S]*?)`", source.read_text()):
        program = match.group(1)
        if len(program) < 15000 and not re.search(r"\bimport\s", program):
            fixtures.setdefault(program, str(source.relative_to(ROOT)))
for source in (ROOT / "tests" / "selfhost" / "float_literals.nov",):
    fixtures[source.read_text()] = str(source.relative_to(ROOT))

results = []
for index, (program, origin) in enumerate(fixtures.items()):
    name = f"diff_{index:04d}"
    program = re.sub(r"\bmodule\s+[A-Za-z_][A-Za-z_0-9]*", "module " + name, program, count=1)
    path = work / (name + ".nov")
    path.write_text(program)
    assembly = output / (name + (".asm" if target_os == "windows" and target_arch == "amd64" else ".s"))
    builds = []
    for executable, label in ((reference, "reference"), (compiler, "novus")):
        assembly.unlink(missing_ok=True)
        completed = subprocess.run([str(executable), "--asm-only", f"--target={args.target}", str(path)], cwd=ROOT, capture_output=True, timeout=60)
        log = completed.stdout + completed.stderr
        (work / f"{name}-{label}.log").write_bytes(log)
        digest = hashlib.sha256(assembly.read_bytes()).hexdigest() if completed.returncode == 0 and assembly.exists() else None
        builds.append({"exit": completed.returncode, "assembly_sha256": digest})
    passed = builds[0] == builds[1]
    results.append({"name": name, "origin": origin, "passed": passed, "reference": builds[0], "novus": builds[1]})
    if not passed:
        print(f"FAIL {name}: {origin}: {builds}", flush=True)
    elif (index + 1) % 50 == 0:
        print(f"Compared {index + 1}/{len(fixtures)} cases", flush=True)
failed = sum(not result["passed"] for result in results)
(work / "results.json").write_text(json.dumps({"target": args.target, "passed": len(results) - failed, "failed": failed, "cases": results}, indent=2) + "\n")
print(f"{len(results) - failed}/{len(results)} compiler comparisons passed", flush=True)
raise SystemExit(1 if failed else 0)

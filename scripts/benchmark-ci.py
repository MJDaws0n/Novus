#!/usr/bin/env python3
"""Prepare identical Nox inputs and benchmark both compilers on their native host."""
import argparse
from pathlib import Path
import shutil
import subprocess
import sys

root = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser()
parser.add_argument('--target', required=True)
args = parser.parse_args()
os_name, arch = args.target.split('/')
folder_arch = {'amd64': 'x86_64', '386': 'x86'}.get(arch, arch)
ext = '.exe' if os_name == 'windows' else ''
work = root / 'build/benchmarks/nox'
work.mkdir(parents=True, exist_ok=True)
shutil.copy2(root / 'build/nox-reference/main.nov', work / 'main.nov')
shutil.copytree(root / 'lib', work / 'lib', dirs_exist_ok=True)
compiler = root / 'dist' / f'novus-{os_name}-{arch}{ext}'
reference = root / 'build/selfhost' / f'{os_name}_{folder_arch}' / f'bootstrap-go{ext}'
result = subprocess.run([sys.executable, str(root / 'scripts/benchmark.py'),
    '--reference', str(reference), '--compiler', str(compiler), '--large', str(work / 'main.nov'),
    '--include-self', '--target', args.target, '--runs', '6', '--max-ratio', '1.25'], cwd=root)
# Exercise the actual large application's executable as well as compiling it.
program = root / 'build' / f'{os_name}_{folder_arch}' / f'nox{ext}'
version = subprocess.run([str(program), 'version'], cwd=root, capture_output=True, timeout=30)
if version.returncode or b'3.4.2' not in version.stdout:
    raise RuntimeError('The benchmark Nox executable failed its version check')
raise SystemExit(result.returncode)

#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

mkdir -p build

sbt -error -batch "run"

clang -arch arm64 build/out.s -o build/out

set +e
./build/out
echo "exit code: $?"
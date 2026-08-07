#!/usr/bin/env bash
set -euo pipefail

benchmark_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(git -C "$benchmark_dir" rev-parse --show-toplevel)
state_dir=${HNIX_NAR_STATE_DIR:-"$repo_root/benchmark-results/nar-stream-state"}
iterations=${HNIX_NAR_ITERATIONS:-7}
warmups=${HNIX_NAR_WARMUPS:-5}
runs=${HNIX_NAR_RUNS:-30}
max_load_per_cpu=${HNIX_NAR_MAX_LOAD_PER_CPU:-0.5}
cpu_count=$(nproc)
default_cpu=$((cpu_count > 1 ? cpu_count - 2 : 0))
benchmark_cpu=${HNIX_NAR_CPU:-$default_cpu}

baseline_bin="$state_dir/bin/baseline"
fixed_bin="$state_dir/bin/fixed"
fixture="$state_dir/fixture"

for required_path in "$baseline_bin" "$fixed_bin" "$fixture" "$state_dir/metadata.txt"; do
  if [[ ! -e "$required_path" ]]; then
    echo "Missing benchmark state: $required_path" >&2
    echo "Run prepare-hyperfine.sh first." >&2
    exit 1
  fi
done

check_load() {
  local load_one
  load_one=$(awk '{ print $1 }' /proc/loadavg)
  if ! awk \
    -v current_load="$load_one" \
    -v cpus="$cpu_count" \
    -v maximum="$max_load_per_cpu" \
    'BEGIN { exit !((current_load / cpus) <= maximum) }'
  then
    echo "Refusing to benchmark: load is $load_one across $cpu_count CPUs." >&2
    echo "Wait for load/CPU <= $max_load_per_cpu, or override HNIX_NAR_MAX_LOAD_PER_CPU." >&2
    exit 75
  fi
}

if ! taskset -c "$benchmark_cpu" true; then
  echo "CPU $benchmark_cpu is not available to taskset." >&2
  exit 1
fi

check_load

timestamp=$(date -u +%Y%m%dT%H%M%SZ)
results_dir="$state_dir/results/$timestamp"
mkdir -p "$results_dir"

baseline_command="taskset -c $benchmark_cpu $baseline_bin run $fixture $iterations +RTS -N1 -A64m -RTS"
fixed_command="taskset -c $benchmark_cpu $fixed_bin run $fixture $iterations +RTS -N1 -A64m -RTS"

common_options=(
  --warmup "$warmups"
  --runs "$runs"
  --shell=none
)

echo "Benchmarking baseline first on CPU $benchmark_cpu..."
hyperfine \
  "${common_options[@]}" \
  --export-json "$results_dir/baseline-first.json" \
  --export-markdown "$results_dir/baseline-first.md" \
  --command-name baseline "$baseline_command" \
  --command-name fixed "$fixed_command"

check_load

echo "Benchmarking fixed first on CPU $benchmark_cpu..."
hyperfine \
  "${common_options[@]}" \
  --export-json "$results_dir/fixed-first.json" \
  --export-markdown "$results_dir/fixed-first.md" \
  --command-name fixed "$fixed_command" \
  --command-name baseline "$baseline_command"

cp "$state_dir/metadata.txt" "$results_dir/metadata.txt"

echo
echo "Benchmark results: $results_dir"

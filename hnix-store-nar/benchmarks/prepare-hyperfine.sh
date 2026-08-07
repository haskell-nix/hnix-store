#!/usr/bin/env bash
set -euo pipefail

benchmark_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(git -C "$benchmark_dir" rev-parse --show-toplevel)
state_dir=${HNIX_NAR_STATE_DIR:-"$repo_root/benchmark-results/nar-stream-state"}
baseline_ref=${HNIX_NAR_BASELINE_REF:-HEAD}
file_count=${HNIX_NAR_FILES:-5000}

if [[ -e "$state_dir" ]]; then
  echo "Benchmark state already exists: $state_dir" >&2
  echo "Set HNIX_NAR_STATE_DIR to a new path or remove the old state first." >&2
  exit 1
fi

scratch_dir=$(mktemp -d "${TMPDIR:-/tmp}/hnix-nar-prepare.XXXXXX")
baseline_tree="$scratch_dir/baseline"
baseline_added=false

cleanup() {
  if [[ "$baseline_added" == true ]]; then
    git -C "$repo_root" worktree remove --force "$baseline_tree" >/dev/null 2>&1 || true
  fi
  rm -rf -- "$scratch_dir"
}
trap cleanup EXIT

mkdir -p "$state_dir/bin"

build_runner() {
  local source_tree=$1
  local label=$2
  local build_dir="$scratch_dir/dist-$label"
  local object_dir="$scratch_dir/objects-$label"

  mkdir -p "$object_dir"
  (
    cd "$source_tree"
    cabal build hnix-store-nar \
      --builddir="$build_dir" \
      --disable-tests \
      --disable-benchmarks \
      -j1
    cabal exec --builddir="$build_dir" -- \
      ghc -O2 -threaded -rtsopts "-with-rtsopts=-N1" \
        -odir "$object_dir" \
        -hidir "$object_dir" \
        -package bytestring \
        -package directory \
        -package filepath \
        -package hnix-store-nar \
        "$benchmark_dir/NarStream.hs" \
        -o "$state_dir/bin/$label"
  )
}

echo "Building fixed benchmark runner..."
build_runner "$repo_root" fixed

echo "Creating baseline worktree at $baseline_ref..."
git -C "$repo_root" worktree add --detach "$baseline_tree" "$baseline_ref"
baseline_added=true

echo "Building baseline benchmark runner..."
build_runner "$baseline_tree" baseline

echo "Creating the shared $file_count-file fixture..."
"$state_dir/bin/fixed" prepare "$state_dir/fixture" "$file_count"

baseline_commit=$(git -C "$baseline_tree" rev-parse HEAD)
fixed_commit=$(git -C "$repo_root" rev-parse HEAD)
{
  printf 'baseline_ref=%s\n' "$baseline_ref"
  printf 'baseline_commit=%s\n' "$baseline_commit"
  printf 'fixed_commit=%s\n' "$fixed_commit"
  printf 'file_count=%s\n' "$file_count"
  printf 'ghc=%s\n' "$(ghc --numeric-version)"
  printf 'cabal=%s\n' "$(cabal --numeric-version)"
} > "$state_dir/metadata.txt"
git -C "$repo_root" diff --binary > "$state_dir/fixed.patch"

echo
echo "Benchmark state is ready: $state_dir"
echo "When the machine is idle, run:"
echo "  nix-shell $benchmark_dir/shell.nix --run '$benchmark_dir/run-hyperfine.sh'"

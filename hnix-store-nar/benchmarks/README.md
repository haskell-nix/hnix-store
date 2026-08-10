# NAR streaming wall-time benchmark

This benchmark compares the working tree's NAR streamer with a baseline Git
revision. Both binaries archive the same pre-created wide directory, so fixture
creation, compilation, and cleanup are excluded from Hyperfine's measurements.

## 1. Prepare

From the repository root:

```console
nix-shell hnix-store-nar/benchmarks/shell.nix --run \
  'hnix-store-nar/benchmarks/prepare-hyperfine.sh'
```

The default baseline is `HEAD`. This is appropriate while the fix is still an
uncommitted working-tree change. After committing the fix, select its parent or
another known revision explicitly:

```console
HNIX_NAR_BASELINE_REF=HEAD^ \
  nix-shell hnix-store-nar/benchmarks/shell.nix --run \
    'hnix-store-nar/benchmarks/prepare-hyperfine.sh'
```

Preparation creates ignored state under `benchmark-results/nar-stream-state`:
the two prebuilt runners, a shared 5,000-file fixture, build metadata,
and the working-tree patch. It does not run Hyperfine.

## 2. Measure when the machine is idle

```console
nix-shell hnix-store-nar/benchmarks/shell.nix --run \
  'hnix-store-nar/benchmarks/run-hyperfine.sh'
```

The runner refuses to start when one-minute load divided by the CPU count is
above `0.5`. It pins both binaries to the same CPU, performs five warmups and 30
measurements, then repeats the comparison in reverse order to expose drift.
JSON and Markdown reports are written below the prepared state's `results/`
directory.

Useful overrides:

```console
HNIX_NAR_FILES=10000             # preparation only
HNIX_NAR_ITERATIONS=10           # dumps per measured process
HNIX_NAR_WARMUPS=5
HNIX_NAR_RUNS=30
HNIX_NAR_CPU=14
HNIX_NAR_MAX_LOAD_PER_CPU=0.5
HNIX_NAR_STATE_DIR=/some/path
```

This is a warm-cache benchmark of NAR traversal and encoding. Do not clear the
machine's page cache between runs; doing so needs elevated privileges, disrupts
other work, and measures a different workload.

#!/usr/bin/env sh
export BENCHMARKS_RUNNER=TRUE
case "$1" in
  "dune" ) test="dune_bench"; main="main";;
  "fiber" ) test="fiber_bench"; main="fiber_bench_main";;
esac
shift;
export BENCH_LIB="$test"
exec /home/quyen/.opam/default/bin/dune exec --release -- "./_esy/default/build/default/foo/$main.exe" -fork -run-without-cross-library-inlining "$@"

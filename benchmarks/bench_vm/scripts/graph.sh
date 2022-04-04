
RESULTPATH=./benchmarks/bench_vm/results

FIRSTGRAPH=_nanos_minor.png
SECONDGRAPH=_major_promo.png
FIGSIZE=1800x1300
for benchmark in "prim" "gas" "expr" "recursive" "bench_results"
  do
  graph $RESULTPATH/$benchmark.csv \
    --xcol full_benchmark_name \
    -X ''\
    --ycol time_per_run_nanos \
    -Y ''\
    --fill \
    --chain | \

  graph $RESULTPATH/$benchmark.csv -o $RESULTPATH/$benchmark$FIRSTGRAPH \
    --xcol full_benchmark_name \
    -X ''\
    --ycol minor_words_per_run \
    -Y ''\
    --title runtime_in_nanos_and_minor_words \
    --figsize $FIGSIZE \
    --xtick-angle 30 \
    --fill \
    --tick-fontsize 12

  graph $RESULTPATH/$benchmark.csv \
    --xcol full_benchmark_name \
    -X ''\
    --ycol major_words_per_run \
    -Y ''\
    --fill \
    --chain | \

  graph $RESULTPATH/$benchmark.csv -o $RESULTPATH/$benchmark$SECONDGRAPH.png \
    --xcol full_benchmark_name \
    -X ''\
    --ycol promoted_words_per_run \
    -Y ''\
    --title major_and_promoted_words \
    --figsize $FIGSIZE \
    --xtick-angle 30 \
    --fill \
    --tick-fontsize 12
  done
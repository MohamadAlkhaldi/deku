# Required programs: 
#   sexp (clean.sh)      (https://github.com/janestreet/sexp)
#   csvkit               (https://github.com/wireservice/csvkit)
#   graph-cli (graph.sh) (https://github.com/mcastorina/graph-cli)

# build benchmarks
esy b dune build @benchmarks/bench &> /dev/null

RESULTPATH=./benchmarks/bench_vm/results

touch $RESULTPATH/bench_results.csv
touch $RESULTPATH/bench_results.txt

# TODO: figure out how to substitute this in the for loop for more modularity
#BENCHMARKS = "expr" "gas" "prim" "recursive"

for benchmark in "expr" "gas" "prim" "recursive"
  do
    ./_esy/default/build/default/benchmarks/bench_vm/benchmarks.exe $benchmark -sexp | \
    ./benchmarks/bench_vm/scripts/clean.sh | \
    tee $RESULTPATH/$benchmark.csv >> $RESULTPATH/bench_results.csv 
    csvlook $RESULTPATH/$benchmark.csv >> $RESULTPATH/$benchmark.txt 
  done

# outputs nice table of results
csvlook $RESULTPATH/bench_results.csv >> $RESULTPATH/bench_results.txt

# graphs our results for easy comparison
./benchmarks/bench_vm/scripts/graph.sh

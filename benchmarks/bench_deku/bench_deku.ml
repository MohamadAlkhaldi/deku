open Core

(*
    $ dune build @benchmarks/bench_deku
    $ dune exec -- ./benchmarks/bench_deku/bench_deku.exe subcommand

     - rpc: for tezos_rpc
     - ledger: for src/core_deku/ledger.ml
     - validators: for src/protocol/validators.ml
     - patricia: for incremental patricia tree
     - interop: for tezos internal operations
     -

     or using esy:
    $ esy b dune build
    $ esy b dune exec ./benchmarks/bench_deku/bench_deku.exe subcommand
*)

let main () =
  Command_unix.run
    (Command.group ~summary:"Several benchmarks"
       [
         ("rpc", Bench_tezos_rpc.command);
         ("ledger", Bench_ledger.command);
         ("validators", Bench_validators.command);
         ("patricia", Bench_patricia.command);
         ("interop", Bench_tezos_interop.command);
       ])

let () = main ()

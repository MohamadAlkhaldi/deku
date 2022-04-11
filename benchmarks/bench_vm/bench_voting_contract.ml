open Lambda_vm
open Core_bench
open Vm_utils

let value_of_string str =
  let rec aux idx pair =
    if idx >= 0 then
      let c = str.[idx] |> Char.code |> Int64.of_int in
      aux (idx - 1) [%lambda_vm.value [%e Int64 c], [%e pair]]
    else
      pair in
  let length = String.length str in
  let c = str.[length - 1] |> Char.code |> Int64.of_int in

  let length' = Ast.Int64 (Int64.of_int length) in
  let list = aux (length - 2) [%lambda_vm.value [%e Int64 c], (0L, 0L)] in
  [%lambda_vm.value [%e length'], [%e list]]

let compare =
  [%lambda_vm
    let delta f = f f in
    let aux aux size a b =
      if size then
        if fst a - fst b then
          0L
        else
          aux aux (size - 1L) (snd a) (snd b)
      else
        1L in
    let aux = delta aux in
    fun a b ->
      let size_a = fst a in
      let size_b = fst b in
      if size_a - size_b then
        0L
      else
        aux size_a (snd a) (snd b)]

let vote_contract =
  (* Reference: https://github.com/juanProject/tezosVote/blob/master/src/voteContract.ligo *)
  [%lambda_vm.script
    fun pair ->
      let compare = [%e compare] in

      let is_admin storage address =
        let admin = fst storage in
        compare admin address in

      let find_voter votes sender =
        let aux aux size votes =
          if size then
            let pair = fst votes in
            let key = fst pair in
            if compare key sender then
              (1L, snd pair)
            else
              aux aux (size - 1L) (snd votes)
          else
            (0L, 0L) in
        let aux = (fun f -> f f) aux in
        aux (fst votes) (snd votes) in

      let count_votes votes =
        let aux aux size votes count =
          if size then
            let pair = fst votes in
            let value = snd pair in
            let count =
              if value then
                count + 1L
              else
                count - 1L in
            aux aux (size - 1L) (snd votes) count
          else
            count in
        let aux = (fun f -> f f) aux in
        aux (fst votes) (snd votes) 0L in

      let append_list lst value =
        let size = fst lst in
        let lst = snd lst in
        (size + 1L, (value, lst)) in

      let sub_vote sender vote storage =
        let admin = fst storage in
        let paused = fst (snd (snd (snd storage))) in
        if paused then
          0L
        else if is_admin storage sender then
          0L
        else
          let storage = snd storage in
          let votes = fst storage in
          let voter = find_voter votes sender in
          if fst voter then
            1L
          else
            let votes = append_list votes (sender, vote) in
            let vote_count = fst (snd storage) + 1L in
            let result =
              if vote_count - 10L then
                snd (snd (snd storage))
              else
                let count = count_votes votes in
                (* Checks if count is negative *)
                if count land 0x8000000000000000L then
                  1L (* non *)
                else if count then
                  2L (* oui *)
                else
                  3L
              (* egalite *) in
            let paused =
              if vote_count - 10L then
                paused
              else
                1L in
            let storage = (admin, (votes, (vote_count, (paused, result)))) in
            (storage, (0L, 0L)) in

      let reset sender storage =
        if is_admin storage sender then
          let admin = fst storage in
          let votes = (0L, (0L, 0L)) in
          let vote_count = 0L in
          let paused = 0L in
          let result = 0L in
          let storage = (admin, (votes, (vote_count, (paused, result)))) in
          (storage, (0L, 0L))
        else
          0L in

      let sender = fst pair in
      let action = fst (snd pair) in

      let storage = snd (snd pair) in

      if fst action then
        sub_vote sender (snd action) storage
      else
        reset sender storage]


let bench_compile_vote_contract = 
  bench_compile_script "voting contract"  ~initial_gas:200000 ~script:vote_contract


let bench_run_contract test_name storage = 
  let initial_gas = 925000 in
  let name = "execute " ^ test_name in
  Bench.Test.create ~name (fun () ->   
    let gas = Gas.make ~initial_gas in
    let vote_contract = Vm_utils.compile_exn gas vote_contract in
    let storage = Vm_utils.compile_value_exn gas storage in
    let _ = Vm_utils.execute_exn gas storage vote_contract in
    ())
    
let empty_storage admin = 
  [%lambda_vm.value 
    [%e admin],
      ( (* votes *) (0L, (0L, 0L)),
      ((* vote count *) 0L, ((* paused *) 0L, (* result *) 0L)) )]

let storage_with_vote admin vote = 
  [%lambda_vm.value 
    [%e admin],
      ( (* votes *) (1L, ([%e vote], (0L, 0L))),
        ((* vote count *) 1L, ((* paused *) 0L, (* result *) 0L)) )]

let reset = [%lambda_vm.value 0L, 0L]

let vote vote = 
  if vote then
    [%lambda_vm.value 1L, 1L]
  else 
    [%lambda_vm.value 1L, 0L]

let admin = value_of_string "tz1ibMpWS6n6MJn73nQHtK5f4ogyYC1z9T9z"
let alice = value_of_string "tz1L738ifd66ah69PrmKAZzckvvHnbcSeqjf"
let bob = value_of_string "tz1LFuHW4Z9zsCwg1cgGTKU12WZAs27ZD14v"
let frank = value_of_string "tz1Qd971cetwNr5f4oKp9xno6jBvghZHRsDr"
let pascal = value_of_string "tz1TgK3oaBaqcCHankT97AUNMjcs87Tfj5vb"
let jacob = value_of_string "tz1VphG4Lgp39MfQ9rTUnsm7BBWyXeXnJSMZ"
let lucina = value_of_string "tz1ZAZo1xW4Veq5t7YqWy2SMbLdskmeBmzqs"
let mark = value_of_string "tz1ccWCuJqMxG4hoa1g5SKhgdTwXoJBM8kpc"
let jean = value_of_string "tz1hQzKQpprB5JhNxZZRowEDRBoieHRAL84b"
let boby = value_of_string "tz1hTic2GpaNumpTtYwqyPSBd9KcWifRMuEN"
let bartholome = value_of_string "tz1hv9CrgtaxiCayc567KUvCyWDQRF9sVNuf"

let bench_vote = 
  bench_run_contract
    "bench vote"
    [%lambda_vm.value
      (*sender*)
      [%e alice], ([%e vote true], [%e empty_storage admin])] 

let bench_auto_pause = 
  let storage =
    [%lambda_vm.value
      [%e admin],
        ( ( 9L,
            ( ([%e bob], 1L),
              ( ([%e frank], 0L),
                ( ([%e pascal], 0L),
                  ( ([%e jacob], 0L),
                    ( ([%e lucina], 0L),
                      ( ([%e mark], 0L),
                        ( ([%e jean], 0L),
                          (([%e boby], 0L), (([%e bartholome], 0L), (0L, 0L)))
                        ) ) ) ) ) ) ) ),
          (9L, (0L, 0L)) )] in
  bench_run_contract 
    "bench auto pause"
        [%lambda_vm.value (*sender*) [%e alice], ([%e vote true], [%e storage])]

let bench_reset = 
  let storage =
    [%lambda_vm.value
      [%e admin],
        ( ( 10L,
            ( ([%e alice], 1L),
              ( ([%e bob], 0L),
                ( ([%e frank], 0L),
                  ( ([%e pascal], 0L),
                    ( ([%e jacob], 0L),
                      ( ([%e lucina], 0L),
                        ( ([%e mark], 0L),
                          ( ([%e jean], 0L),
                            (([%e boby], 0L), (([%e bartholome], 0L), (0L, 0L)))
                          ) ) ) ) ) ) ) ) ),
          (10L, (1L, 1L)) )] in
  bench_run_contract
    "bench reset"
    [%lambda_vm.value (*sender*) [%e admin], ([%e reset], [%e storage])]

let bench_get_result = 
  let storage =
    [%lambda_vm.value
      [%e admin],
        ( ( 9L,
            ( ([%e bob], 1L),
              ( ([%e frank], 1L),
                ( ([%e pascal], 1L),
                  ( ([%e jacob], 1L),
                    ( ([%e lucina], 1L),
                      ( ([%e mark], 0L),
                        ( ([%e jean], 0L),
                          (([%e boby], 0L), (([%e bartholome], 0L), (0L, 0L)))
                        ) ) ) ) ) ) ) ),
          (9L, (0L, 0L)) )] in
  bench_run_contract
    "bench get result"
    [%lambda_vm.value (*sender*) [%e alice], ([%e vote true], [%e storage])]

let bench_draw =
  let storage =
    [%lambda_vm.value
      [%e admin],
        ( ( 9L,
            ( ([%e bob], 1L),
              ( ([%e frank], 1L),
                ( ([%e pascal], 1L),
                  ( ([%e jacob], 1L),
                    ( ([%e lucina], 0L),
                      ( ([%e mark], 0L),
                        ( ([%e jean], 0L),
                          (([%e boby], 0L), (([%e bartholome], 0L), (0L, 0L)))
                        ) ) ) ) ) ) ) ),
          (9L, (0L, 0L)) )] in
  bench_run_contract
  "bench draw"
    [%lambda_vm.value (*sender*) [%e alice], ([%e vote true], [%e storage])]


let tests = [
    bench_compile_vote_contract;
    bench_vote;
    bench_auto_pause;
    bench_reset;
    bench_get_result;
    bench_draw;
  ]

let command = Bench.make_command tests

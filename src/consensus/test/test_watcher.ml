open Chelper.Consensus_helpers

module P = struct
  module Threshold = struct
    let get = fun _height -> 3
  end
end

module MyWatcher = Consensus.Watcher.Raw (P)

let create_validators_list : identical_validator:bool -> proposer list = fun ~identical_validator ->
  if identical_validator then (
    let (_fake_secret, fake_key) = Crypto.Ed25519.generate () in 
    let fake_key = Crypto.Key.Ed25519 fake_key in 
    let fake_validator = Validator.of_key fake_key in 
    List.init 4 (fun _ -> fake_validator)
  ) else (
    List.init 4 (fun _ ->
      let (_fake_secret, fake_key) = Crypto.Ed25519.generate () in 
      let fake_key = Crypto.Key.Ed25519 fake_key in 
      let fake_validator = Validator.of_key fake_key in 
      fake_validator)
  )

let create_message_list : identical_round:bool -> identical_validator:bool -> Consensus.Message.t list = fun ~identical_round ~identical_validator ->
  let open Consensus.Message in 
  let fake_bytes = Bytes.of_string "test_value" in
  let validator_list = create_validators_list ~identical_validator in

  if identical_round then (
    let message_list = List.init 4 (fun i ->
      let fake_validator = List.nth validator_list i in 
      Precommit (Precommit.make ~height:1 ~round:0 ~value_opt:(Some fake_bytes) ~validator:fake_validator))
    in 
    message_list
  ) else (
  let message_list = List.init 4 (fun i ->
    let fake_validator = List.nth validator_list i in
    Precommit (Precommit.make ~height:1 ~round:i ~value_opt:(Some fake_bytes) ~validator:fake_validator)
  )
  in 
  message_list)

let create_wrong_message_list : identical_round:bool -> identical_validator:bool -> Consensus.Message.t list = fun ~identical_round ~identical_validator ->
  let open Consensus.Message in 
  let fake_bytes = Bytes.of_string "test_value" in
  let validator_list = create_validators_list ~identical_validator in

  if identical_round then (
    let message_list = List.init 4 (fun i ->
      let fake_validator = List.nth validator_list i in 
      Prevote (Prevote.make ~height:1 ~round:0 ~value_opt:(Some fake_bytes) ~validator:fake_validator))
    in 
    message_list
  ) else (
  let message_list = List.init 4 (fun i ->
    let fake_validator = List.nth validator_list i in
    Precommit (Precommit.make ~height:1 ~round:i ~value_opt:(Some fake_bytes) ~validator:fake_validator)
  )
  in 
  message_list)
  

let _test_counting_precommits identical_round identical_validator () = 
  let open Consensus.Message in 
  let message_list = create_message_list ~identical_round ~identical_validator in 
  let state = MyWatcher.empty in 
  let state = List.fold_left (fun state msg -> 
    let state, _msgs = MyWatcher.process_message msg state in 
    state) state message_list in
  let content = HeightMap.find_opt 1 state in 
  match content with 
  | None -> 0, None
  | Some content -> (
    let MyWatcher.{ precommits = precommit_set ; commited_value_opt = commited_val } = content in 
    let count = PrecommitSet.cardinal precommit_set in
    count, commited_val
  )


let test_counting_precommits () = 
  Alcotest.(check bool)
    "Finding 4 precommits and the value" true 
    (
      match _test_counting_precommits true false () with 
      | count, Some x -> (count = 4) && (x = Some (Bytes.of_string "test_value"))
      | _, None -> false
    ) ;
  Alcotest.(check bool)
    "Finding no value because 4 differents rounds" true 
    (
      match _test_counting_precommits false false () with 
      | _, Some _ -> false 
      | count, None -> (count = 4)
    ) ;
  Alcotest.(check bool)
    "Finding 1 precommit and no value because 4 precommits from same validator" true 
    (
      match _test_counting_precommits true true () with 
      | _, Some _ -> false 
      | count, None -> count = 1
    )

let _test_counting_prevotes () = 
  let message_list = create_wrong_message_list ~identical_round:true ~identical_validator:false in 
  let state = MyWatcher.empty in 
  let state = List.fold_left (fun state msg -> 
    let state, _msgs = MyWatcher.process_message msg state in 
    state) state message_list in

  let content = HeightMap.find_opt 1 state in 
  match content with 
  | None -> 0, None
  | Some content -> (
  let MyWatcher.{ precommits = precommit_set ; commited_value_opt = commited_val } = content in 
  let count = Consensus.Message.PrecommitSet.cardinal precommit_set in
  count, commited_val)

let test_counting_prevotes () = 
  Alcotest.(check bool)
    "Finding no value and empty precommit set because receiving prevotes" true 
    (
      match _test_counting_prevotes () with 
      | count, None -> count = 0
      | _, Some _ -> false
    )

let _test_tick empty_state () = 
  if empty_state then (
    let state = MyWatcher.empty in 
    let new_state, _ = MyWatcher.tick 12 state in
    state, new_state
  ) else (
    let message_list = create_message_list ~identical_round:false ~identical_validator:false in 
    let state = MyWatcher.empty in 
    let state = List.fold_left (fun state msg -> 
      let state, _msgs = MyWatcher.process_message msg state in 
      state) state message_list in
    let new_state, _ = MyWatcher.tick 12 state in
    state, new_state
  )

let test_tick () = 
  Alcotest.(check bool)
    "Does nothing to empty state" true 
    (
      let old_s, new_s = _test_tick false () in 
      old_s = new_s
    ) ;
  Alcotest.(check bool)
    "Does nothing to non-empty state" true 
    (
      let old_s, new_s = _test_tick true () in 
      old_s = new_s
    )

let watcher_counts_commits_value = 
  [
    ("Processes several types of precommits"
    , `Quick
    , test_counting_precommits
    )
    ; ("Does not process prevotes"
    , `Quick
    , test_counting_prevotes
    )
  ]

(**** RUN TESTS ****)

let () = 
  Alcotest.run "Watcher test"
    [
      ("Watcher counts precommits correctly and commits value", watcher_counts_commits_value)
    ]
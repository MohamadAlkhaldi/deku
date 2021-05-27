/*

 */

// TODO: does computers have multiple RTC nowadays?
// Can a core send a message and the other receive it in the past?
// TODO: should start signing before being in sync?

Random.self_init();
Mirage_crypto_rng_unix.initialize();

open Opium;
open Helpers;
open Protocol;
open Node;
open Networking;

let ignore_some_errors =
  fun
  | Error(#Flows.ignore) => Ok()
  | v => v;
let log_errors = f =>
  fun
  | Ok(_) => ()
  | Error(err) => print_endline(f(err));
let handle_request =
    (
      type req,
      type res,
      module E:
        Request_endpoint with type request = req and type response = res,
      f,
    ) =>
  App.post(
    E.path,
    request => {
      let update_state = state => {
        Server.set_state(state);
        state;
      };
      let.await json = Request.to_json(request);
      let response = {
        let.ok json = Option.to_result(~none=`Not_a_json, json);
        let.ok request =
          E.request_of_yojson(json)
          |> Result.map_error(err => `Not_a_valid_request(err));
        f(update_state, request);
      };
      switch (response) {
      | Ok(response) =>
        let response = E.response_to_yojson(response);
        await(Response.of_json(~status=`OK, response));
      | Error(_err) =>
        await(Response.make(~status=`Internal_server_error, ()))
      };
    },
  );
let handle_received_block_and_signature =
  handle_request(
    (module Block_and_signature_spec),
    (update_state, request) => {
      open Flows;
      let.ok () =
        received_block(Server.get_state(), update_state, request.block)
        |> ignore_some_errors;

      let.ok () =
        received_signature(
          Server.get_state(),
          update_state,
          ~hash=request.block.hash,
          ~signature=request.signature,
        )
        |> ignore_some_errors;
      Ok();
    },
  );
let handle_received_signature =
  handle_request(
    (module Signature_spec),
    (update_state, request) => {
      open Flows;
      let.ok () =
        received_signature(
          Server.get_state(),
          update_state,
          ~hash=request.hash,
          ~signature=request.signature,
        )
        |> ignore_some_errors;
      Ok();
    },
  );
let handle_block_by_hash =
  handle_request(
    (module Block_by_hash_spec),
    (_update_state, request) => {
      let block = Flows.find_block_by_hash(Server.get_state(), request.hash);
      Ok(block);
    },
  );
let handle_protocol_snapshot =
  handle_request(
    (module Protocol_snapshot),
    (_update_state, ()) => {
      let State.{snapshots, _} = Server.get_state();
      Ok({
        snapshot: snapshots.Snapshots.last_snapshot.data,
        snapshot_hash: snapshots.last_snapshot.hash,
        additional_blocks: snapshots.additional_blocks,
        last_block: snapshots.last_block,
        last_block_signatures:
          Signatures.to_list(snapshots.last_block_signatures),
      });
    },
  );

module Utils = {
  let read_file = file => {
    let.await ic = Lwt_io.open_file(~mode=Input, file);
    let.await lines = Lwt_io.read_lines(ic) |> Lwt_stream.to_list;
    let.await () = Lwt_io.close(ic);
    await(lines |> String.concat("\n"));
  };

  let read_identity_file = file => {
    let.await file_buffer = read_file(file);
    await(
      try({
        let json = Yojson.Safe.from_string(file_buffer);
        State.identity_of_yojson(json);
      }) {
      | _ => Error("failed to parse json")
      },
    );
  };
  // TODO: write only file system signed by identity key and in binary identity key
  let read_validators = file => {
    let.await file_buffer = read_file(file);
    await(
      try({
        let json = Yojson.Safe.from_string(file_buffer);
        [%of_yojson: list(Validators.validator)](json);
      }) {
      | _ => Error("failed to parse json")
      },
    );
  };
};

let node = {
  open Utils;
  let identity_file =
    Array.length(Sys.argv) >= 2 ? Sys.argv[1] : "identity.json";
  let.await identity = read_identity_file(identity_file);
  let identity = Result.get_ok(identity);

  let.await validators = read_validators("validators.json");
  let validators = Result.get_ok(validators);

  let node = State.make(~identity);
  let node = {
    ...node,
    protocol: {
      ...node.protocol,
      validators:
        List.fold_right(Validators.add, validators, Validators.empty),
    },
  };
  await(node);
};
let node = node |> Lwt_main.run;
let server = Node.Server.start(~initial=node);

let _server =
  App.empty
  |> App.port(Node.Server.get_port() |> Option.get)
  |> handle_received_block_and_signature
  |> handle_received_signature
  |> handle_block_by_hash
  |> handle_protocol_snapshot
  |> App.start
  |> Lwt_main.run;

let (forever, _) = Lwt.wait();
let () = Lwt_main.run(forever);

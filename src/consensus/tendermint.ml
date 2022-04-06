open Chelper.Consensus_helpers
open Message
open Prenode

module type PARAMETER = sig
  val get_proposer : Height.t -> Round.t -> proposer
  val now : unit -> Timestamp.t

  module Threshold : sig
    val get : Height.t -> int
  end


  module Value : sig
    val get : Value.Hashed.t -> Value.t
    val set : Value.t -> unit
  end

  module Log : Chelper.Log.LOGGER
end

(* Tendermint implementation *)
module Raw (P : PARAMETER) = struct

  module Log = Chelper.Log.Make(P.Log)

  module Step = struct
    type t =
    | Proposal
    | Prevote
    | Precommit
  end
  
  type consensus_state = {
    step : Step.t ;
    height : Height.t ;
    round : Round.t ;
    valid_round : Round.t ;
    valid_value_opt : Value.Hashed.t option ;
    proposal_timeout_opt : Timestamp.t option ;
    prevote_timeout_opt : Timestamp.t option ;
    proposed_value_opt : Value.Hashed.t option ;
    prevote_counter : int ; (* counts (PREVOTE, heighs, round, NOT_NIL_VALUE)  *)
    prevote_counter_nil_value : int ; (* counts (PREVOTE, height, round, NIL) *)
    prevote_counter_vr : int ; (* counts (PREVOTE, height, valid_round, NOT_NIL_VALUE) *)
    locked_round_opt : Round.t option ;
    locked_value_opt : Value.Hashed.t option ;
    delay_counter : int ; (* counts messages received with r > round while h = height *)
    clock : Timestamp.t ;
    self : Validator.t ;
  }
  
  let get_step t = t.step
  let set_step s t = {t with step = s}
  
  let get_height t = t.height
  let set_height h t = {t with height = h}
  
  let get_round t = t.round
  let set_round r t = {t with round = r}
  
  let get_proposal_timeout_opt t = t.proposal_timeout_opt
  let set_proposal_timeout v t = {t with proposal_timeout_opt = Some (v)}
  let remove_proposal_timout t = {t with proposal_timeout_opt = None}
  
  let get_prevote_timeout_opt t = t.prevote_timeout_opt
  let set_prevote_timeout v t = {t with prevote_timeout_opt = Some (v)}
  let remove_prevote_timout t = {t with prevote_timeout_opt = None}
  
  let get_proposed_value_opt t = t.proposed_value_opt
  
  let set_proposed_value v t = {t with proposed_value_opt = Some (v)}
  
  let get_valid_round t = t.valid_round
  
  let set_valid_round vr t = {t with valid_round = vr}
  
  let get_valid_value_opt t = t.valid_value_opt
  let set_valid_value v t = {t with valid_value_opt = Some (v)}
  
  let get_prevote_counter t = t.prevote_counter
  let get_prevote_counter_nil_value t = t.prevote_counter_nil_value
  
  let get_delay_counter t = t.delay_counter
  let set_delay_counter v t = { t with delay_counter = v}

  let get_prevote_counter_vr t = t.prevote_counter_vr
  
  let get_locked_value_opt t = t.locked_value_opt
  
  let set_locked_value v t = {t with locked_value_opt = Some (v)}
  let get_locked_round_opt t = t.locked_round_opt
  let set_locked_round r t = {t with locked_round_opt = Some (r)}
  
  type t = consensus_state
  
  let count_received_messages : t -> int = fun t ->
    let count = 1 + (get_prevote_counter t) + (get_prevote_counter_nil_value t) in 
    count
  
  let messages_threshold : t -> bool = fun t ->
    (count_received_messages t) > P.Threshold.get (get_height t)
  
  let is_valid_proposer (proposal: Proposal.t) (t:t) =
    let height = get_height t in 
    let round = get_round t in 
    let valid_proposer = P.get_proposer height round in 
    let proposer = Proposal.get_proposer proposal in 
    Crypto.Key_hash.equal valid_proposer proposer
  
  let register_proposal : Proposal.t -> t -> t = fun proposal t ->
    let value = Proposal.get_value proposal in 
    let height = Proposal.get_height proposal in 
    let round = Proposal.get_round proposal in 
    let valid_round = Proposal.get_valid_round proposal in
    let t = set_proposed_value (Value.hash value) t in
    P.Value.set value ;
    let t = set_height height t in 
    let t = set_round round t in 
    let t = set_valid_round valid_round t in 
    t
  
  let trigger_on_timeout_propose : Proposal.t -> t -> t = fun proposal t ->
    match get_proposal_timeout_opt t with 
    | None -> (
      (* No proposal timeout in progress, register the proposal and start the timeout. *)
      let timeout = Timestamp.add (P.now ()) Proposal.duration in
      let t = set_proposal_timeout timeout t in
    (* TODO: add is_valid *)
      let t = register_proposal proposal t in 
      t
    )
    | Some _ -> t (* If there already is a timeout, ignore. *)
  
  let process_delay_proposal : Proposal.t -> t -> t * Message.t list = fun proposal t ->
    (* If proposal.round > t.round, this counts towards delay. *)
    if (proposal.round > t.round) then (
      let delay_counter = (get_delay_counter t) + 1 in 
      let t = set_delay_counter delay_counter t in 
      t, []
    ) else (
      t, []
    )
  
  let process_proposal : Proposal.t -> t -> t * Message.t list = fun proposal t ->
    PseudoEffect.returner @@ fun { return } ->
    
    (* Handle potential delay *)
    let t, _msgs = process_delay_proposal proposal t in 
    
    (* Proposal are only dealt with during Proposal step. *)
    if not (t.step = Proposal) then (
      return (t, [])
    );

    (* Ignore Proposal from invalid round. *)
    if not (proposal.round = t.round) then (
      return (t, [])
    );

    (* Ignore Proposal from invalid proposer. *)
    if not (is_valid_proposer proposal t) then (
      return (t, [])
    );

    (* Update step. *)
    let t = set_step Prevote t in 

    let hashed_value = Value.hash (Proposal.get_value proposal) in 
    let same_locked_value = match get_locked_value_opt t with
      | Some v -> v = hashed_value
      | None -> false
    in
    let is_locked_round_none = Option.is_none (get_locked_round_opt t) in 

    (* Prevoting either Nil or hashed_value *)
    if (Value.is_valid hashed_value) && (is_locked_round_none || same_locked_value) then (
      let prevote_message =
        Message.Prevote 
          (Prevote.make 
            ~height:(get_height t)
            ~round:(get_round t)
            ~value_opt:(Some hashed_value)
            ~validator:t.self
          )
      in 
      t, [prevote_message]
    ) else (
      let prevote_message = 
        Message.Prevote
          (Prevote.make
            ~height:(get_height t)
            ~round:(get_round t)
            ~value_opt:None
            ~validator:t.self
        )
      in
      t, [prevote_message])

  
  let assert_prevote_height_and_round_matches_state : Prevote.t -> t -> unit = fun prevote t ->
    let prevote_height = Prevote.get_height prevote in 
    let prevote_round = Prevote.get_round prevote in 
    let height = get_height t in 
    let round = get_round t in 
    assert ((prevote_height = height) && (prevote_round = round))

  let register_prevote : Prevote.t -> t -> t = fun prevote t ->
    PseudoEffect.returner @@ fun { return } ->

    match Prevote.get_value_opt prevote with 
    | None -> (
      (* Received a NIL prevote. *)
      if (prevote.round = t.round) then (
        let t = {t with prevote_counter_nil_value = (get_prevote_counter_nil_value t) + 1} in
        t
      ) else (
          t
      )
    )
    | Some v -> (
      (* Received a NOT NIL, keeping only those matching proposed value. *)
      match get_proposed_value_opt t with 
      | None -> return t
      | Some x -> (
        if not (v = x) then (
          return t
        )
      );

      let valid_round = get_valid_round t in 
      let round = get_round t in 

      if (Prevote.get_round prevote = round) then (
        let t = {t with prevote_counter = (get_prevote_counter t) + 1} in 
        t
      ) else(
        if (Prevote.get_round prevote = valid_round) then (
          let t = {t with prevote_counter_vr = (get_prevote_counter_vr t) + 1} in 
          t
        ) else (
          t
        )
      )
    )

  let is_prevote_threshold_reached : value_is_nil:bool -> round_is_valid_round:bool -> t -> bool = fun ~value_is_nil ~round_is_valid_round t ->
    if value_is_nil then (
      get_prevote_counter_nil_value t = P.Threshold.get (get_height t)
    ) else (
      if round_is_valid_round then (
        get_prevote_counter_vr t = P.Threshold.get (get_height t)
      ) else (
        get_prevote_counter t = P.Threshold.get (get_height t)
      )
    )
  
  
  let process_prevote_when_step_is_proposal : Prevote.t -> t -> t * Message.t list = fun prevote t ->
    (* ALG 28. *)
    PseudoEffect.returner @@ fun { return } ->
  
    (* This function should only be called when step is Proposal. *)
    assert ((get_step t) = Proposal) ;
  
    (* If threshold isn't reached, do nothing since prevote is already registered. *)
    if not (is_prevote_threshold_reached ~value_is_nil:false ~round_is_valid_round:true t) then (
      return (t, [])
    );
  
    let valid_round = get_valid_round t in 
    let round = get_round t in 
    let value = match Prevote.get_value_opt prevote with 
      | None -> return (t, [])
      | Some v -> v
    in

    (* Discarding if invalid vr and r. *)
    if not ((0 <= valid_round) && (valid_round < round)) then (
      return (t, [])
    );
  
    (* Altering step. *)
    let t = set_step Prevote t in 

    let same_locked_value = match get_locked_value_opt t with
      | Some v -> v = value
      | None -> false
    in
    let is_locked_round_none = Option.is_none (get_locked_round_opt t) in 

    if (Value.is_valid value) && (is_locked_round_none || same_locked_value) then (
      let prevote_message =
        Message.Prevote 
          (Prevote.make 
            ~height:(get_height t)
            ~round:(get_round t)
            ~value_opt:(Some value)
            ~validator:t.self
          )
      in 
      t, [prevote_message]
    ) else (
      let prevote_message = 
        Message.Prevote
          (Prevote.make
            ~height:(get_height t)
            ~round:(get_round t)
            ~value_opt:None
            ~validator:t.self
        )
      in
      t, [prevote_message])
   
  
  let process_prevote_when_step_is_prevote : Prevote.t -> t -> t * Message.t list = fun prevote t ->
    (* ALG 34, 36, 44*)
    PseudoEffect.returner @@ fun { return } ->
  
    (* This hould only be called when step is Prevote. *)
    assert ((get_step t) = Prevote) ;
  
(*     if not (is_prevote_threshold_reached  t) then (
      return (t, [])
    ); *)
  
    let value = match Prevote.get_value_opt prevote with 
      | None -> return (t, [])
      | Some v -> v
    in
  
    if not (Value.is_valid value) then (
      return (t, [])
    );
  
    let round = Prevote.get_round prevote in
  
    let t = set_locked_value value t in
    let t = set_locked_round round t in 
    let t = set_valid_round round t in 
    let t = set_valid_value value t in 
    let t = set_step Precommit t in 
    let precommit_message = 
      Message.Precommit
        (Precommit.make
          ~height:(get_height t)
          ~round:round
          ~value_opt:(Some value)
          ~validator:t.self 
      )
    in
    (t, [precommit_message])
  
  let process_prevote_when_step_is_precommit : Prevote.t -> t -> t * Message.t list = fun prevote t ->
    PseudoEffect.returner @@ fun { return } ->
  
    assert ((get_step t) = Precommit) ;
    assert ((get_round t) = (Prevote.get_round prevote)) ;
  
    (* TODO: *)
(*     if not (reached_prevote_nil_threshold t) then (
      return (t, [])
    ); *)
  
    let value = match Prevote.get_value_opt prevote with 
      | None -> return (t, [])
      | Some v -> v
    in
  
    if not (Value.is_valid value) then (
      return (t, [])
    );
    
    let round = Prevote.get_round prevote in 
    let t = set_valid_round round t in 
    let t = set_valid_value value t in 
    (t, [])
  
  let process_delay_prevote : Prevote.t -> t -> t * Message.t list = fun prevote t ->
    (* If prevote.round > t.round, this counts towards delay. *)
    if (prevote.round > t.round) then (
      let delay_counter = (get_delay_counter t) + 1 in 
      let t = set_delay_counter delay_counter t in 
      t, []
    ) else (
      t, []
    )
  let process_prevote : Prevote.t -> t -> t * Message.t list = fun prevote t -> 
    PseudoEffect.returner @@ fun { return } ->
    
    (* Handle delay *)
    let t, _ = process_delay_prevote prevote t in 

    (* Discarding prevote if no proposed value is registered. *)
    (
      match get_proposed_value_opt t with 
      | None -> return (t, [])
      | Some _ -> ()
    );

    (* TODO: add 55*)
  
    (* Register prevote (if necessary) *)
    let t = register_prevote prevote t in 

  
  
    match t.step with 
    | Proposal -> (process_prevote_when_step_is_proposal prevote t)
    | Prevote -> (process_prevote_when_step_is_prevote prevote t)
    | Precommit -> (process_prevote_when_step_is_precommit prevote t)
  
  
  
  
  
  let process_precommit : Precommit.t -> t -> t * Message.t list = fun _precommit _t ->
    assert false
  
  let process_message (msg: Message.t) (t:consensus_state) = 
    match msg with
    | Proposal x -> process_proposal x t 
    | Prevote x -> process_prevote x t 
    | Precommit x -> process_precommit x t
  
  let on_proposal_timeout : t -> t * Message.t list = fun _ -> assert false
  let on_prevote_timeout : t -> t * Message.t list = fun _ -> assert false 
  
  let tick = fun timestep t ->
    let t = { t with clock = Timestamp.add t.clock timestep } in
    let t, proposal_timeout_msgs = 
      match t.proposal_timeout_opt with
      | Some proposal_timeout when Timestamp.more_recent t.clock proposal_timeout -> on_proposal_timeout t
      | _ -> t, []
    in
    let t, prevote_timeout_msgs = 
      match t.prevote_timeout_opt with
      | Some prevote_timeout when Timestamp.more_recent t.clock prevote_timeout -> on_prevote_timeout t
      | _ -> t, []
    in  
    let msgs = proposal_timeout_msgs @ prevote_timeout_msgs in 
    t, msgs 
  
  
  
  
  end
  
  module Make (P: PARAMETER) : MAIN = Raw (P)
  
  
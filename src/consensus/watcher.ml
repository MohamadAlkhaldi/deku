open Chelper.Consensus_helpers
open Message
open Prenode

module type PARAMATER = sig
  module Threshold : sig
    val get : Height.t -> int
  end
end

module Raw (P : PARAMATER)= struct
  

  type content = {
    precommits : PrecommitSet.t ;
    commited_value_opt : Value.Hashed.t option option ;
  }

  let content_empty = {
    precommits = PrecommitSet.empty ;
    commited_value_opt = None ;
  }
  type t = content HeightMap.t

  let empty = HeightMap.empty

  let tick = fun _timestamp t -> t , []
  
  let process_precommit = fun precommit t -> 

    let Precommit.{ value_opt; height; round = round ; validator = _validator } = precommit in 
    let content =
      match HeightMap.find_opt height t with 
      | None -> content_empty
      | Some x -> x
    in
    let { precommits = precommit_set ; commited_value_opt = commited_val } = content in 
    let precommit_set = PrecommitSet.add precommit precommit_set in 
    let precommits_matching = PrecommitSet.filter (fun x -> Option.equal Value.Hashed.equal x.value_opt value_opt) precommit_set in 
    let precommits_matching = PrecommitSet.filter (fun x -> round = x.round) precommits_matching in 
    let number_precommits_matching = PrecommitSet.cardinal precommits_matching in 
    let content = 
      if (number_precommits_matching > P.Threshold.get height) then (
        {precommits = precommit_set ; commited_value_opt = Some (value_opt) }
      ) else (
        {precommits = precommit_set ; commited_value_opt = commited_val}
      )
    in 
    let t = HeightMap.update height (fun _ -> Some content) t  in 
    t, []


  let process_message = fun msg t -> 
    match msg with 
    | Message.Precommit precommit -> process_precommit precommit t 
    | _ -> t, []
end

module Make (P: PARAMATER) : MAIN = Raw (P)
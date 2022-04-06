open Chelper.Consensus_helpers


module Proposal = struct
  type t = {
    proposer : proposer ;
    value : Value.t ;
    height : Height.t ;
    round : Round.t ;
    valid_round : Round.t ;
  }
   
  let get_proposer (t:t) : proposer = t.proposer 

  let get_value : t -> Value.t = fun t -> t.value

  let get_height : t -> Height.t = fun t -> t.height

  let get_round : t -> Round.t = fun t -> t.round

  let get_valid_round : t -> Round.t = fun t -> t.valid_round

  let duration = Timestep.Millisecond.of_int 1_000
end


module Prevote = struct
  type t = {
    value_opt : Value.Hashed.t option;
    height : Height.t ;
    round : Round.t ;
    validator : Validator.t ;
  }


  let duration = Timestep.Millisecond.of_int 1_000


  let make : height:Height.t -> round:Round.t -> value_opt:Value.Hashed.t option -> validator:Validator.t -> t
   = fun ~height ~round ~value_opt ~validator ->
    {
      height = height ;
      round = round ;
      value_opt = value_opt ;
      validator = validator ;
    }

  let get_value_opt : t -> Value.Hashed.t option = fun t -> t.value_opt

  let get_height : t -> Height.t = fun t -> t.height
  let get_round : t -> Round.t = fun t -> t.round

end

module Precommit = struct
  type t = {
    value_opt : Value.Hashed.t option;
    height : Height.t ;
    round : Round.t ;
    validator : Validator.t ;
  }
  [@@deriving ord]
  
  let make : height:Height.t -> round:Round.t -> value_opt:Value.Hashed.t option -> validator:Validator.t -> t
  = fun ~height ~round ~value_opt ~validator ->
    {
      height = height ;
      round = round ;
      value_opt = value_opt ;
      validator = validator ;
    }
end

module PrecommitSet = Set.Make (Precommit)
 
type t = 
| Proposal of Proposal.t 
| Prevote of Prevote.t 
| Precommit of Precommit.t


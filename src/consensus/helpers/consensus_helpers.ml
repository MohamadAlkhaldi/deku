module PseudoEffect = struct
  type 'a return = {
    return : 'b . 'a -> 'b ;
  }

  let returner (type r) f =
    let exception Return of r in
    let p = {
      return = fun x -> raise (Return x) ;
    } in
    try f p
    with Return r -> r
end

module Height = Int 
module Round = Int
type proposer = Crypto.Key_hash.t

module Validator = Crypto.Key_hash

module HeightMap = Map.Make (Height)
 
module Timestep = struct
  module Raw = struct
    type t = int
    module Millisecond = struct
      let of_int : int -> t = fun i -> i
      let to_int : t -> int = fun i -> i 
    end

  end
  include (Raw : sig 
    type t 
    module Millisecond : sig 
      val of_int : int -> t
      val to_int : t -> int
    end
  end)
end

module Timestamp = struct
  module Raw = struct
    (** Number of seconds since 1970*)
    type t = float

    let add : t -> Timestep.t -> t = fun now duration ->
      let f = 0.001 *. (float_of_int @@ Timestep.Millisecond.to_int duration) in 
      now +. f
    
    let more_recent : t -> t -> bool = fun v1 v2 -> v1 >= v2
  end
  include (Raw: sig
    type t 
    val add : t -> Timestep.t -> t
    val more_recent : t -> t -> bool
  end)
end

module Value = struct
  type t (* TODO: should be real_value and id_value aka hashed value*)

  module Hashed = Bytes

  let hash : t -> Hashed.t = fun _ -> assert false
  let get_value : t -> t = fun t -> t

  let is_valid : Hashed.t -> bool = fun t -> t = t

  let equal : t -> t -> bool = fun v1 v2 -> v1 = v2
end

module Threshold = struct
  type t 

  let get : Height.t -> int = fun _h -> 3
end

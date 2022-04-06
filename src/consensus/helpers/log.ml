module type LOGGER = sig
  type t
  val debug : string -> unit
  val info : string -> unit
end 

module Make (L : LOGGER) : LOGGER = struct
  include L
end

open Chelper.Consensus_helpers

module type MAIN = sig
  type t
  val process_message : Message.t -> t -> t * Message.t list

  val tick : Timestep.t -> t -> t * Message.t list
end

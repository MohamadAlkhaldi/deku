open Crypto
type t = BLAKE2B_20.t [@@deriving eq, ord, yojson]
val encoding : t Data_encoding.t
val to_string : t -> string
val of_string : string -> t option

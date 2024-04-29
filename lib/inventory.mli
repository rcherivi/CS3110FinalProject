type t

val create_inventory : t
val lookup : string -> t -> int
val lookup_option : string -> t -> int option
val insert : string -> int -> t -> t
val harvest : string -> t -> Garden.t -> t * Garden.t
val sell : string -> int -> t -> Garden.t -> t * Garden.t
val print : t -> unit
val add : string -> t -> t
val get_length : t -> int

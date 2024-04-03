type t

val create_inventory : t
val harvest : string -> t -> Garden.t -> t * Garden.t
val sell : string -> t -> Garden.t -> t * Garden.t
val print : t -> unit
type t

val create_garden : t
val add_plant : string -> t -> t
val feed_plants : t -> t
val print : t -> unit
val inc_money : string -> t -> t
val remove_plant : string -> t -> t
val get_flowers : t -> t
val get_fruits : t -> t
val count_plant : string -> t -> int

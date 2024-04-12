type t

val create_garden : unit -> t
val add_plant : string -> string -> t -> t
val feed_plants : t -> t
val print : t -> unit
val inc_money : string -> t -> t
val inc_money_amt : float -> t -> t
val remove_plant : string -> t -> t
val get_flowers : t -> t
val get_fruits : t -> t
val get_vegetables : t -> t
val get_grains : t -> t
val get_trees : t -> t
val get_other_plants : t -> t
val count_plant : string -> t -> int

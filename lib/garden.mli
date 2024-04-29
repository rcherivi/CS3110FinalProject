type t

val create_garden : unit -> t
val add_plant : string -> string -> t -> t
val feed_plants : t -> string -> t
val water_plants : t -> string -> t
val neglect_plants : t -> string -> t
val print : t -> unit
val inc_money : string -> t -> t
val incr_luck : t -> t
val incr_defense : t -> t
val get_plant_count : t -> int

(* val inc_money : Plant.t -> string -> t -> t *)
val inc_money_amt : float -> t -> t
val remove_plant : string -> t -> t
val get_flowers : t -> t
val get_fruits : t -> t
val get_vegetables : t -> t
val get_grains : t -> t
val get_defensive_items : t -> t
val count_plant : string -> t -> int
val print_plants_in_category : string -> t -> unit
val night_change : int ref -> t -> t
val has_plant : t -> string -> bool

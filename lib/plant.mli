type t

val feed : t -> string -> t
val water : t -> string -> t
val create_plant : string -> string -> t
val print_plant : t -> string
val check_life : t -> string -> t
val get_type : t -> string
val get_height : t -> int
val neglect : t -> string -> t
val get_sale_price : t -> string -> float
val get_price : t -> float
val apply_discount : t -> t
val get_hydration : t -> int
val get_name : t -> string
val get_category : t -> string
val get_life : t -> bool

type t

val feed : t -> t
val water : t -> t
val create_plant : string -> t
val print_plant : t -> string
val check_life : t -> t
val get_type : t -> string
val get_height : t -> int
val neglect : t -> t

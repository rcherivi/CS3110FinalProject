type t
(** [t] represents a garden. AF: Each garden has plants and money. RI: The
    representation of a plant is valid if the height of the plant is
    non-negative *)

type cell
(** [cell] represents a cell in the garden. AF: Each cell is either empty or
    contains a plant. RI: The representation of a plant is valid if the height
    of the plant is non-negative.*)

val create_garden : unit -> t
(**[create_garden] creates a new garden with default parameters.*)

val get_garden_cell : t -> cell array array
(** [get_garden_cell] gets the cell of the garden*)

val get_garden_money : t -> float
(** [get_garden_money] gets the money amount of the garden.*)

val get_garden_lucky : t -> int
(** [get_garden_lucky] gets the luckiness of the garden.*)

val get_garden_defense : t -> int
(**[get_garden_defense] gets the defense level of the garden.*)

val add_plant : string -> string -> t -> t
(**[add_plant x s] adds a plant with the given name and type to the garden.*)

val feed_plants : t -> string -> t
(**[feed_plants x s] feeds a plant in the garden*)

val water_plants : t -> string -> t
(**[water_plants x s] waters a plant in the garden*)

val neglect_plants : t -> string -> t
(**[neglect_plants x s] neglects a plant in the garden*)

val print : t -> unit
(**[print g] prints the visual representation of the garden.*)

val inc_money : string -> t -> t
(**[inc_money x g] increases the available funds in the garden based on the type
   of plant bought.*)

val inc_money_amt : float -> t -> t
(**[inc_money f g] increases the available funds in the garden [g] by a specific
   amount [f].*)

val remove_plant : string -> t -> t
(**[remove_plant s g] removes all plants of the specified type and name from the
   garden. *)

val get_flowers : t -> t
(**[get_flowers g] filters and retrieves only flowers from the garden. *)

val get_fruits : t -> t
(**[get_fruits g] filters and retrieves only fruits from the garden. *)

val get_vegetables : t -> t
(**[get_vegetables g] filters and retrieves only vegetables from the garden. *)

val get_grains : t -> t
(**[get_grains g] filters and retrieves only grains from the garden. *)

val get_defensive_items : t -> t
(**[get_defensive_items g] filters and retrieves only defensive items from the
   garden. *)

val count_plant : string -> t -> int
(**[count_plant s g] reduced the count of plants after they are a certain height*)

val print_plants_in_category : string -> t -> unit
(**[print_plants_in_category s g] prints the plants in a specific category from
   the garden.*)

val night_change : int ref -> t -> t
(**[night_change i g] simulates changes in the garden during the night based on
   random events.*)

val has_plant : t -> string -> bool
(**[has_plant g s] checks if a plant with the given name exists in the garden.*)

val incr_luck : t -> t
val incr_defense : t -> t
val get_plant_count : t -> int
val get_money : t -> float

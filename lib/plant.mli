type t
(** [t] represents a plant in the garden. AF: Each plant has a name, a type, a
    height, a sale price, a price, hydration level, life status, and category.
    RI: The representation of a plant is valid if the height of the plant is
    non-negative *)

val feed : t -> string -> t
(** [feed x s] modifies the height and price of the plant based on the nutrient
    provided and returns a new plant*)

val water : t -> string -> t
(** [water x s] modifies the hydration level of the plant based on the nutrient
    provided and returns a new plant*)

val create_plant : string -> string -> t
(** [create_plant x s] creates a new plant with the given name and type and
    returns a new plant*)

val print_plant : t -> string
(** [print_plant x] generates a visual representation of the plant. *)

val check_life : t -> string -> t
(** [check_life x s] Determines if the plant is affected by the environmental
    factor and updates its life status accordingly and returns a new plant with
    the updated life status *)

val get_type : t -> string
(** [get_type x] returns the type of the plant. *)

val get_height : t -> int
(** [get_height x] returns the height of the plant *)

val neglect : t -> string -> t
(** [neglect x s] returns a new plant after neglecting it based on the
    environmental factor. *)

val get_sale_price : t -> string -> float
(** [get_sale_price x s] returns the sale price of a specific plant after
    applying the event. *)

val get_price : t -> float
(** [get_price x] returns the original price of the plant. *)

val apply_discount : t -> t
(** [apply_discount x] reduces the price of the plant by a certain percentage
    and returns a new plant *)

val get_hydration : t -> int
(** [get_hydration x] returns the hydration level of the plant*)

val get_name : t -> string
(** [get_name x] returns the name of the plant*)

val get_category : t -> string
(** [get_category x] returns the category of the plant*)

val get_life : t -> bool
(** [get_life x] returns the life status of the plant*)

val stampede : float -> t -> t
(** [stampede x] returns a dead plant*)

val rain : t -> t
(** [rain x] returns a plant with higher hydration level*)

val drought : t -> t
(** [drought x] returns a plant with a lower hydration level*)

val pollinate : t -> t
(** [pollinate x] returns a plant with a higher height*)

val is_alive : t -> bool
(** [is_alive x] checks if a plant is alive*)

val dragon : t -> t
(** [dragon] returns a dead plant.*)

val ice : t -> t
(** [ice] returns a plant with a lower height.*)

val unicorn : t -> t
(** [unicorn] returns a plant with a higher height.*)

val fairies : t -> t
(** [fairies] returns a plant with a higher price.*)

val max_height : string -> int
(** [max_height x] returns a integer associated with plant [x] when it is
    matured *)

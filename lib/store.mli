type t
(** [t] represents a store. AF: Each store has items. RI: N/A*)

val create_store : t
(** [create_store] creates a store with the pricces of all the items *)

val print_store : t -> string -> string
(** [print_store s cat] prints the items of the store [s] using a specific
    category the user enters [cat] *)

val buy_item : string -> t -> Inventory.t -> Garden.t -> Inventory.t * Garden.t
(** [buy_item item s inv g] adds what item the user wants into the inventory or
    the store *)

val has_item : string -> bool
(** [has_item item] checks if the item that user enters is in the store *)

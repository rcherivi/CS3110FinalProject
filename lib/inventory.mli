type t
(** [t] represents an inventory. AF: The inventory is a list of items and their
    associated frequencies. RI: The representation of an inventory is valid if
    it is an association list, and each item in the list has an associated name
    and frequency number. *)

val create_inventory : t
(** [create_inventory] creates a new inventory. *)

val lookup : string -> t -> int
(** [lookup] lookups a certain item of the inventory and returns its associated
    frequency amount.*)

val lookup_option : string -> t -> int option
(** [lookup_option] lookups an item of the inventory, and if it does exist,
    returns its associated frequency amount. *)

val insert : string -> int -> t -> t
(** [insert] inserts an item into the inventory.*)

val harvest : string -> t -> Garden.t -> t * Garden.t
(** [harvest] removes a plant from the garden if its a certain height and puts
    it into the inventory. *)

val sell : string -> int -> t -> Garden.t -> t * Garden.t
(** [sell] sells a certain number of plants from the inventory *)

val print : t -> unit
(** [print] prints the inventory.*)

val add : string -> t -> t
(** [add] adds an item to the inventory.*)

val remove : string -> t -> t
(** [remove] removes an item from the inventory. *)

val get_length : t -> int
(** [get_length] gets the length of the inventory.*)

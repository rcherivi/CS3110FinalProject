(* type t *)
type t

val create_store : t
val print_store : t -> string
val buy_item : string -> t -> Inventory.t -> Garden.t -> Inventory.t * Garden.t

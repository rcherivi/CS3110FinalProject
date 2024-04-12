type t

val get_missing_ingredients :
  (string * int) list -> t -> Inventory.t -> (string * int) list

val have_ingredients : Inventory.t -> t -> unit
val tomato_soup_recipe : string
val bread_recipe : string
val apple_pie_recipe : string

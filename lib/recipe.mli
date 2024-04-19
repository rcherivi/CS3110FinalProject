type t
type recipe_type

val recipe_type_of_string : string -> recipe_type option
val recipe_ingredients : recipe_type -> t
val price : recipe_type -> float

val get_missing_ingredients :
  (string * int) list -> t -> Inventory.t -> (string * int) list

val have_ingredients : Inventory.t -> t -> string
val create_recipe : string -> Inventory.t -> Inventory.t

val sell_recipe :
  string -> int -> Inventory.t -> Garden.t -> Inventory.t * Garden.t

val tomato_soup_recipe : string
val bread_recipe : string
val apple_pie_recipe : string
val apple_juice_recipe : string
val popcorn_recipe : string
val french_fries_recipe : string
val cookie_recipe : string
val sandwich_recipe : string
val salad_recipe : string
val strawberry_cake_recipe : string
val bouquet_recipe : string
val curry_recipe : string
val chicken_soup_recipe : string
val hamburger_recipe : string
val smoothie_recipe : string

type t
(** [t] represents a recipe. AF: A recipe is an association list that is a list
    of items to make a recipe and its associated frequencies. RI: The
    representation of a recipe is valid if it is an association list. *)

type recipe_type
(** [recipe_type] represents the type of recipes. AF: A recipe type is valid if
    it one of the following recipes: ApplePie, TomatoSoup, Bread, AppleJuice,
    Popcorn, FrenchFries, Cookie, Sandwich, Salad, StrawberryCake, Bouquet,
    Curry, ChickenSoup, Hamburger, Smoothie. *)

val recipe_type_of_string : string -> recipe_type option
(** [recipe_type_of_string] matches each string to a recipe type*)

val recipe_ingredients : recipe_type -> t
(** [recipe_ingredients] matches each recipe type to its associated recipe.*)

val price : recipe_type -> float
(** [price] matches each recipe type with its associated price. *)

val create : t
(** [create] creates a new recipe. *)

val get_missing_ingredients :
  (string * int) list -> t -> Inventory.t -> (string * int) list
(** [get_missing_ingredients] returns all the missing ingredients if the
    inventory does not contain the ingredients to make a certain recipe. *)

val have_ingredients : Inventory.t -> t -> string
(** [have_ingredients] checks whether the inventory has enough ingredients to
    make the recipe*)

val update_inventory : Inventory.t -> t -> Inventory.t
(** [update_inventory] updates the inventory after creating a recipe.*)

val create_recipe : string -> Inventory.t -> Inventory.t
(** [create_recipe] creates a recipe and adds it to the inventory.*)

val sell_recipe :
  string -> int -> Inventory.t -> Garden.t -> Inventory.t * Garden.t
(** [sell_recipe] sells the recipe from the inventory.*)

val insert_recipe : string -> int -> t -> t
(** [insert_recipe] inserts ingredients into recipe.*)

val add_recipe : string -> t -> t
(** [add_recipe] adds ingredients to recipe.*)

val lookup_recipe : string -> t -> int
(** [lookup_recipe] lookups up the associated quantity with an ingredient. *)

val tomato_soup_recipe : string
(** [tomato_soup_recipe] returns the tomato soup recipe.*)

val bread_recipe : string
(** [bread_recipe] returns the bread recipe.*)

val apple_pie_recipe : string
(** [apple_pie_recipe] returns the apple pie recipe.*)

val apple_juice_recipe : string
(** [apple_juice_recipe] returns the apple juice recipe.*)

val popcorn_recipe : string
(** [popcorn_recipe] returns the popcorn recipe.*)

val french_fries_recipe : string
(** [french_fries_recipe] returns the french fries recipe.*)

val cookie_recipe : string
(** [cookie_recipe] returns the cookie recipe.*)

val sandwich_recipe : string
(** [sandwich_recipe] returns the sandwich recipe.*)

val salad_recipe : string
(** [salad_recipe] returns the salad recipe.*)

val strawberry_cake_recipe : string
(** [strawberry_cake_recipe] returns the strawberry cake recipe.*)

val bouquet_recipe : string
(** [bouquet_recipe] returns the bouquet recipe.*)

val curry_recipe : string
(** [curry_recipe] returns the curry recipe.*)

val chicken_soup_recipe : string
(** [chicken_soup_recipe] returns the chicken soup recipe.*)

val hamburger_recipe : string
(** [hamburger_recipe] returns the hamburger recipe.*)

val smoothie_recipe : string
(** [smoothie_recipe] returns the smoothie recipe.*)

type t = (string * int) list

type recipe_type =
  | ApplePie
  | TomatoSoup
  | Bread
  | AppleJuice
  | Popcorn
  | FrenchFries
  | Cookie
  | Sandwich
  | Salad
  | StrawberryCake
  | Bouquet
  | Curry
  | ChickenSoup
  | Hamburger
  | Smoothie

let recipe_type_of_string = function
  | "Apple Pie" -> Some ApplePie
  | "Tomato Soup" -> Some TomatoSoup
  | "Bread" -> Some Bread
  | "Apple Juice" -> Some AppleJuice
  | "Popcorn" -> Some Popcorn
  | "French Fries" -> Some FrenchFries
  | "Cookie" -> Some Cookie
  | "Sandwich" -> Some Sandwich
  | "Salad" -> Some Salad
  | "Strawberry Cake" -> Some StrawberryCake
  | "Bouquet" -> Some Bouquet
  | "Curry" -> Some Curry
  | "Chicken Soup" -> Some ChickenSoup
  | "Hamburger" -> Some Hamburger
  | "Smoothie" -> Some Smoothie
  | _ -> None

let recipe_ingredients = function
  | ApplePie -> [ ("Apple", 5); ("Water", 1); ("Eggs", 2); ("Butter", 1) ]
  | TomatoSoup -> [ ("Tomato", 3); ("Water", 1) ]
  | Bread -> [ ("Wheat", 3); ("Water", 1) ]
  | AppleJuice -> [ ("Apple", 2); ("Sugar", 2); ("Water", 4) ]
  | Popcorn -> [ ("Corn", 3); ("Butter", 1); ("Salt", 1) ]
  | FrenchFries -> [ ("Potato", 3); ("Butter", 1); ("Salt", 1) ]
  | Cookie ->
      [
        ("Milk", 2);
        ("Egg", 1);
        ("Butter", 1);
        ("Chocolate", 1);
        ("Sugar", 1);
        ("Wheat", 2);
      ]
  | Sandwich -> [ ("Bread", 1); ("Tomato", 1); ("Lettuce", 2); ("Cheese", 1) ]
  | Salad -> [ ("Lettuce", 4); ("Tomato", 2); ("Bell Pepper", 1); ("Onion", 1) ]
  | StrawberryCake ->
      [ ("Strawberry", 4); ("Butter", 1); ("Sugar", 2); ("Milk", 2) ]
  | Bouquet ->
      [ ("Yellow Flower", 2); ("Rose", 2); ("Tulip", 3); ("Sunflower", 2) ]
  | Curry -> [ ("Rice", 2); ("Curry Powder", 2); ("Water", 2) ]
  | ChickenSoup ->
      [
        ("Bell Pepper", 2);
        ("Salt", 1);
        ("Chicken", 1);
        ("Milk", 1);
        ("Tomato", 2);
        ("Water", 5);
      ]
  | Hamburger ->
      [
        ("Bread", 1); ("Tomato", 1); ("Lettuce", 1); ("Cheese", 1); ("Beef", 1);
      ]
  | Smoothie ->
      [
        ("Strawberry", 2);
        ("Peach", 2);
        ("Lemon", 1);
        ("Pineapple", 1);
        ("Mango", 3);
      ]

let price = function
  | ApplePie -> 10.0
  | TomatoSoup -> 5.0
  | Bread -> 3.0
  | AppleJuice -> 2.0
  | Popcorn -> 1.0
  | FrenchFries -> 2.50
  | Cookie -> 3.0
  | Sandwich -> 7.0
  | Salad -> 6.0
  | StrawberryCake -> 15.0
  | Bouquet -> 20.0
  | Curry -> 11.0
  | ChickenSoup -> 14.0
  | Hamburger -> 7.70
  | Smoothie -> 5.0

let rec get_missing_ingredients missing (recipe : t) (inventory : Inventory.t) =
  match recipe with
  | [] -> missing
  | (ingredient, recipe_qty) :: tl ->
      let m =
        match Inventory.lookup_option ingredient inventory with
        | Some inv_qty when inv_qty >= recipe_qty -> missing
        | _ -> (ingredient, recipe_qty) :: missing
      in
      get_missing_ingredients m tl inventory

let have_ingredients (inventory : Inventory.t) (recipe : t) =
  let missing_ingredients = get_missing_ingredients [] recipe inventory in
  if missing_ingredients = [] then
    "We have all the ingredients necessary for the recipe."
  else
    "Do not have enough ingredients. Missing ingredients: "
    ^ String.concat ", "
        (List.map
           (fun (ingredient, qty) -> ingredient ^ ": " ^ string_of_int qty)
           missing_ingredients)

let create_recipe recipe_name (inventory : Inventory.t) =
  match recipe_type_of_string recipe_name with
  | Some recipe_type -> (
      let ingredients = recipe_ingredients recipe_type in
      match have_ingredients inventory ingredients with
      | "We have all the ingredients necessary for the recipe." ->
          let updated_inv =
            List.fold_left
              (fun inv (ingred, quantity) ->
                let current_quantity = Inventory.lookup ingred inv in
                let new_qty = current_quantity - quantity in
                Inventory.insert ingred new_qty inv)
              inventory ingredients
          in
          let updated_inv_with_recipe =
            Inventory.insert recipe_name 1 updated_inv
          in
          Printf.printf "You have crafted 1 %s!\n" recipe_name;
          updated_inv_with_recipe
      | msg ->
          print_endline msg;
          inventory)
  | None ->
      print_endline "We don't have that recipe in our store.";
      inventory

let sell_recipe recipe_name (quantity : int) (inv : Inventory.t)
    (garden : Garden.t) =
  match recipe_type_of_string recipe_name with
  | Some recipe_type -> (
      match Inventory.lookup_option recipe_name inv with
      | Some current_quantity when current_quantity >= quantity ->
          let new_quantity = current_quantity - quantity in
          let updated_inventory =
            if new_quantity = 0 then inv
            else Inventory.insert recipe_name new_quantity inv
          in
          let revenue = price recipe_type *. float_of_int quantity in
          let updated_garden = Garden.inc_money_amt revenue garden in
          (updated_inventory, updated_garden)
      | _ ->
          print_endline "Not enough items in inventory";
          (inv, garden))
  | None ->
      print_endline "Unknown recipe name";
      (inv, garden)

let tomato_soup_recipe =
  "\n\
  \  Tomato Soup Recipe: \n\n\
  \   Ingredients: \n\
  \     • 3 Tomatoes \n\
  \     • 1 Cup of Water  \n\
  \  \n\
  \   Instructions: \n\
  \     1) Chop the tomatoes \n\
  \     2) In a pot, bring the tomatoes to a boil and add desired ingredients \n\
  \     3) Serve and enjoy!"

let bread_recipe =
  "\n\
  \  Bread Recipe: \n\n\
  \   Ingredients: \n\
  \     • 3 Bushels of Wheat \n\
  \     • 1 Cup of Water  \n\
  \  \n\
  \   Instructions: \n\
  \     1) Mix wet and dry ingredients in a bowl \n\
  \     2) Placce mixture in a baking pan and bake in oven at 350 degrees for \
   30 minutes \n\
  \     3) Serve and enjoy!"

let apple_pie_recipe =
  "\n\
  \  Apple Pie Recipe: \n\n\
  \   Ingredients: \n\
  \     • 5 Apples \n\
  \     • 1 Cup of Water  \n\
  \     • 2 Eggs \n\
  \     • 1 Stick of Butter \n\
  \  \n\
  \   Instructions: \n\
  \     1) Mix wet and dry ingredients in a bowl \n\
  \     2) Placce mixture in a baking pan and bake in oven at 350 degrees for \
   30 minutes \n\
  \     3) Serve and enjoy!"

let apple_juice_recipe =
  "\n\
  \  Apple Juice Recipe: \n\n\
  \   Ingredients: \n\
  \     • 2 Apples \n\
  \     • 2 Cups of Sugar  \n\
  \     • 4 Cups of Water  \n\
  \  \n\
  \   Instructions: \n\
  \     1) Wash the apples  \n\
  \     2) Juice the apples with a juicer or blender \n\
  \     3) Strain the apple puree through a strainer or cheesecloth \n\
  \     4) Chill, serve and enjoy!"

let popcorn_recipe =
  "\n\
  \  Popcorn Recipe: \n\n\
  \   Ingredients: \n\
  \     • 3 Cobs of Corn  \n\
  \     • 1 Stick of Butter  \n\
  \     • 1 Tablespoon of Salt \n\
  \  \n\
  \   Instructions: \n\
  \     1) Add popcorn kernels in a pan and add the butter \n\
  \     2) Cover lid and pop kernels until popping sound slows \n\
  \     3) Serve and enjoy!"

let french_fries_recipe =
  "\n\
  \  French Fries Recipe: \n\n\
  \   Ingredients: \n\
  \     • 3 Potatoes  \n\
  \     • 1 Stick of Butter  \n\
  \     • 1 Tablespoon of Salt \n\
  \  \n\
  \   Instructions: \n\
  \     1) Wash and peel the potatoes (if desired) \n\
  \     2) Cut the potatoes into long, even strips  \n\
  \     3) Soak the potato strips in a bowl of cold water for at least 30 \
   minutes \n\
  \     4) Dry the potato strips and pat dry thoroughly with paper towels \n\
  \     5) Fill a deep pot with butter and add potatoes to fry  \n\
  \     6) Once fries are golden and crispy, remove from oil \n\
  \     7) Immediately salt the fries  \n\
  \     8) Serve and enjoy!"

let cookie_recipe =
  "\n\
  \  Chocolate Chip Cookie Recipe: \n\n\
  \   Ingredients: \n\
  \     • 2 Cups of Milk  \n\
  \     • 1 Egg  \n\
  \     • 1 Stick of Butter \n\
  \     • 1 Bar of Chocolate  \n\
  \     • 1 Cup of Sugar  \n\
  \     • 2 Bushels of Wheat \n\
  \  \n\
  \   Instructions: \n\
  \     1) Preheat oven to 375 degrees F. Line a baking sheet with parchment \
   paper \n\
  \     2) Mix wet ingredients  \n\
  \     3) Combine dry ingredients  \n\
  \     4) Combine wet and dry ingredients \n\
  \     5) Add chocolate chips evenly  \n\
  \     6) Scoop the dough and space cookies about 2 in apart \n\
  \     7) Bake cookies for 9 to 11 minutes  \n\
  \     8) Cool, serve and enjoy!"

let sandwich_recipe =
  "\n\
  \  Sandwich Recipe: \n\n\
  \   Ingredients: \n\
  \     • 1 Loaf of Bread  \n\
  \     • 1 Tomato  \n\
  \     • 2 Pieces of Lettuce \n\
  \     • 1 Slice of Cheese  \n\
  \  \n\
  \   Instructions: \n\
  \     1) Cut two slices of bread from the loaf \n\
  \     2) On one slice, layer the tomato, cheese, and lettuce  \n\
  \     3) Add the second slice on top  \n\
  \     4) Serve and enjoy!"

let salad_recipe =
  "\n\
  \  Salad Recipe: \n\n\
  \   Ingredients: \n\
  \     • 4 Pieces of Lettuce  \n\
  \     • 2 Tomatoes \n\
  \     • 1 Bell Pepper \n\
  \     • 1 Onion  \n\
  \  \n\
  \   Instructions: \n\
  \     1) Chop all the ingredients \n\
  \     2) In a bowl, mix together the vegetables  \n\
  \     3) Cool, serve and enjoy!"

let strawberry_cake_recipe =
  "\n\
  \  Strawberry Cake Recipe: \n\n\
  \   Ingredients: \n\
  \     • 4 Strawberries  \n\
  \     • 1 Stick of Butter \n\
  \     • 2 Cups of Sugar  \n\
  \     • 2 Cups of Milk \n\
  \  \n\
  \   Instructions: \n\
  \     1) Preheat oven to 350 degrees F. Grease and flour a 9-in cake pan \n\
  \     2) Mix wet ingredients  \n\
  \     3) Combine wet and dry ingredients \n\
  \     4) Pour the batter into pan and bake for about 30-35 minutes \n\
  \     5) Add strawberries on top of cake  \n\
  \     6) Cool, serve and enjoy!"

let bouquet_recipe =
  "\n\
  \  Bouquet Recipe: \n\n\
  \   Ingredients: \n\
  \     • 2 Yellow Flowers  \n\
  \     • 2 Roses \n\
  \     • 3 Tulips \n\
  \     • 2 Sunflowers  \n\
  \  \n\
  \   Instructions: \n\
  \     1) Gather all flowers and tie them together to create a bouquet! \n"

let curry_recipe =
  "\n\
  \  Curry Recipe: \n\n\
  \   Ingredients: \n\
  \     • 2 Bowls of Rice  \n\
  \     • 2 Cans of Curry Powder \n\
  \     • 2 Cups of Water \n\
  \  \n\
  \   Instructions: \n\
  \     1) Cook the rice \n\
  \     2) In a bowl, combine curry powder and water. Stir.  \n\
  \     3) Cool, serve and enjoy!"

let chicken_soup_recipe =
  "\n\
  \  Chicken Soup Recipe: \n\n\
  \   Ingredients: \n\
  \     • 2 Bell Peppers  \n\
  \     • 1 Tablespoon of Salt \n\
  \     • 1 Chicken \n\
  \     • 1 Cup of Milk  \n\
  \     • 2 Tomatoes \n\
  \     • 5 Cups of Water  \n\
  \  \n\
  \   Instructions: \n\
  \     1) Chop all the tomatoes and bell peppers \n\
  \     2) Combine all ingredients and cook for 20 minutes \n\
  \     3) Serve and enjoy!"

let hamburger_recipe =
  "\n\
  \  Hamburger Recipe: \n\n\
  \   Ingredients: \n\
  \     • 1 Loaf of Bread  \n\
  \     • 1 Tomato  \n\
  \     • 1 Piece of Lettuce \n\
  \     • 1 Slice of Cheese  \n\
  \     • 1 Slice of Beef \n\
  \  \n\
  \   Instructions: \n\
  \     1) Slice two slices of bread for the buns \n\
  \     2) Cut a slice of tomato and cheese  \n\
  \     3) Cook the slice of beef  \n\
  \     4) Layer all of the ingredients  \n\
  \     5) Serve and enjoy!"

let smoothie_recipe =
  "\n\
  \  Smoothie Recipe: \n\n\
  \   Ingredients: \n\
  \     • 2 Strawberries  \n\
  \     • 2 Peaches  \n\
  \     • 1 Lemon \n\
  \     • 1 Pineapple  \n\
  \     • 3 Mangoes \n\
  \  \n\
  \   Instructions: \n\
  \     1) Wash all fruits \n\
  \     2) Chop all fruits  \n\
  \     3) In a blender, mix together all ingredients  \n\
  \     4) Serve and enjoy!"

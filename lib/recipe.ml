type t = (string * int) list

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
    print_endline "We have all the ingredients necessary for the recipe."
  else
    print_endline
      ("Do not have enough ingredients. Missing ingredients: "
      ^ String.concat ", "
          (List.map
             (fun (ingredient, qty) -> ingredient ^ ": " ^ string_of_int qty)
             missing_ingredients))

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

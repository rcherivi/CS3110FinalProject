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

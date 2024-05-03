open CS3110FinalProject

(**add a string for instructions*)
let my_garden = Garden.create_garden ()

let my_inventory = Inventory.create_inventory

let menu_options =
  "\n\
  \ Menu:\n\
  \  1. Buy Plant/Item\n\
  \  2. Feed Plants\n\
  \  3. Water Plants\n\
  \  4. Neglect Plants\n\
  \  5. Harvest\n\
  \  6. Observe Garden\n\
  \  7. View Inventory\n\
  \  8. Sell \n\
  \  9. Recipes\n\
  \  10. Create Recipe\n\
  \ Enter your option number: "

let check_attempts attempts =
  if !attempts >= 3 then begin
    print_endline "You've exceeded the maximum number of attempts.";
    print_string menu_options;
    true
  end
  else false

let rec get_valid_category plant_cat =
  match plant_cat with
  | "Flowers" | "Fruits" | "Vegetables" | "Grains" | "Defensive Items" ->
      plant_cat
  | _ ->
      print_string
        "You have entered an invalid category. Please enter a valid\n\
        \   category  here: ";
      let plant_cat_1 = read_line () in
      get_valid_category plant_cat_1

let rec get_valid_name garden attempts =
  attempts := !attempts + 1;
  let name = read_line () in
  if Garden.has_plant garden name then name
  else if check_attempts attempts then name
  else begin
    print_string "Invalid plant\n   name. Please enter a valid name: ";
    get_valid_name garden attempts
  end

let rec get_valid_item item =
  if Store.has_item item then item
  else begin
    print_string "Invalid item. Please enter a valid item: ";
    let item = read_line () in
    get_valid_item item
  end

let feed_garden_helper func n inv garden =
  print_endline
    "What category of plants do you want to feed?\n\
     Flowers | Fruits | Vegetables | Grains | Defensive Items";
  let plant_cat = read_line () in
  let acc_plant_cat = get_valid_category plant_cat in
  let _ = Garden.print_plants_in_category acc_plant_cat garden in
  print_string "You only have 3 chances to type the right name";
  print_string "What is the name of the plant you want to feed: ";
  let attempts = ref 0 in
  let name = get_valid_name garden attempts in
  let new_garden = Garden.feed_plants garden name in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let water_garden_helper func n inv garden =
  print_endline
    "What category of plants do you want to water?\n\
    \ Flowers | Fruits | Vegetables | Grains | Defensive Items";
  let plant_cat = read_line () in
  let acc_plant_cat = get_valid_category plant_cat in
  (* let acc_plant_cat = read_valid_category () in *)
  let _ = Garden.print_plants_in_category acc_plant_cat garden in
  print_string "You only have 3 chances to type the right name";
  print_string "What is the name of the plant you want to water: ";
  let attempts = ref 0 in
  let name = get_valid_name garden attempts in
  let new_garden = Garden.water_plants garden name in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let neglect_garden_helper func n inv garden =
  print_endline
    "What category of plants do you want to neglect?\n\
    \ Flowers | Fruits | Vegetables | Grains | Defensive Items";
  let plant_cat = read_line () in
  let acc_plant_cat = get_valid_category plant_cat in
  let _ = Garden.print_plants_in_category acc_plant_cat garden in
  print_string "You only have 3 chances to type the right name";
  print_string "What is the name of the plant you want to neglect: ";
  let attempts = ref 0 in
  let name = get_valid_name garden attempts in
  let new_garden = Garden.neglect_plants garden name in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let buy_plant_helper func n inv garden =
  let new_store = Store.create_store in
  let () = print_endline (Store.print_store new_store) in
  print_endline
    "\n\
     Plants will be added to garden, and other items will be added to the \
     inventory";
  print_string "What is the name of item you want to buy: ";
  let item = read_line () in
  let acc_item = get_valid_item item in
  let new_inv, new_garden = Store.buy_item acc_item new_store inv garden in
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let harvest_helper func n inv garden =
  print_string "You only have 3 chances to type the right type of plant";
  let () =
    print_endline
      "What type of plants do you want to harvest: \n\
       Daisy | Sunflower | Rose | Tulip |\n\
       Lemon | Pineapple | Apple | Peach | Strawberry | Mango |\n\
       Tomato | Lettuce | Bell Pepper |  Onion | Potato |\n\
       Rice | Wheat | Corn |\n\
       Clover | Cactus"
  in
  let attempts = ref 0 in
  let plant_name = get_valid_name garden attempts in
  let new_inv, new_garden = Inventory.harvest plant_name inv garden in
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let sell_helper func n inv garden =
  print_string "You only have 3 chances to type the right item";
  let () =
    print_endline
      "What type of item do you want to sell from your inventory: \n\
       Daisy | Sunflower | Rose | Tulip |\n\
       Lemon | Pineapple | Apple | Peach | Strawberry | Mango |\n\
       Tomato | Lettuce | Bell Pepper |  Onion | Potato |\n\
       Rice | Wheat | Corn |\n\
       Clover | Cactus |\n\
       Cheese | Eggs | Milk | Water | Butter | Sugar |\n\
       Chocolate | Plant Food | LadyBug | Beef | Chicken"
  in
  let attempts = ref 0 in
  let item_name = get_valid_name garden attempts in
  print_string ("How many " ^ item_name ^ "do you want to sell: ");
  let qty = int_of_string (read_line ()) in
  let new_inv, new_garden = Inventory.sell item_name qty inv garden in
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let view_inv_helper func n inv garden =
  let () = Inventory.print inv in
  func (n + 1) inv garden

let observe_menu_options =
  "\n\
  \  1. View Flowers \n\
  \  2. View Fruits \n\
  \  3. View Vegetables \n\
  \  4. View Grains \n\
  \  5. View Defensive Items \n\
  \  6. View Garden"

let observe_garden_helper func n inv garden count day =
  let print_menu = print_endline observe_menu_options in
  print_menu;
  let choice = read_line () in
  print_string "";
  count := !count + 1;
  if choice = "1" then
    let new_garden = Garden.get_flowers garden in
    Garden.print new_garden
  else if choice = "2" then
    let new_garden = Garden.get_fruits garden in
    Garden.print new_garden
  else if choice = "3" then
    let new_garden = Garden.get_vegetables garden in
    Garden.print new_garden
  else if choice = "4" then
    let new_garden = Garden.get_grains garden in
    Garden.print new_garden
  else if choice = "5" then
    let new_garden = Garden.get_defensive_items garden in
    Garden.print new_garden
  else if choice = "6" then Garden.print garden
  else func (n + 1) inv garden count day;
  func (n + 1) inv garden count day

let recipe_menu =
  "\n\
  \  Select which recipe you would like to view:\n\
  \  1. Tomato Soup 🥫 \n\
  \  2. Bread 🍞 \n\
  \  3. Apple Pie 🥧 \n\
  \  4. Apple Juice 🧃\n\
  \  5. Popcorn 🍿 \n\
  \  6. French Fries 🍟 \n\
  \  7. Chocolate Chip Cookie 🍪 \n\
  \  8. Sandwich 🥪 \n\
  \  9. Salad 🥗 \n\
  \  10. Strawberry Cake 🍰 \n\
  \  11. Flower Bouqet 💐\n\
  \  12. Curry 🍛 \n\
  \  13. Chicken Soup 🥘 \n\
  \  14. Hamburger 🍔 \n\
  \  15. Smoothie 🍹"

let missing_ingredient_message recipe_function func n inv garden count day =
  print_endline
    "Choose the following number to proceed:  \n\
    \  1. Go back to Recipe \n\
    \  2. Go back to menu";
  match read_line () with
  | "1" -> recipe_function func n inv garden count day
  | "2" -> func n inv garden count day
  | _ -> print_endline "Invalid option"

let display_and_handle_recipe recipe_func food_name recipe_name func n inv
    garden count day =
  print_endline recipe_name;
  print_endline
    "\n\
    \  Choose the following number to proceed: \n\
    \    1. Create this recipe \n\
    \    2. Go back to Recipe \n\
    \    3. Go back to menu ";
  match read_line () with
  | "1" ->
      ignore (Recipe.create_recipe food_name inv);
      missing_ingredient_message recipe_func func n inv garden count day
  | "2" -> recipe_func func n inv garden count day
  | "3" -> func n inv garden count day
  | _ ->
      print_endline "Invalid option";
      recipe_func func n inv garden count day

let all_recipes =
  [
    (1, "Tomato Soup", Recipe.tomato_soup_recipe);
    (2, "Bread", Recipe.bread_recipe);
    (3, "Apple Pie", Recipe.apple_pie_recipe);
    (4, "Apple Juice", Recipe.apple_juice_recipe);
    (5, "Popcorn", Recipe.popcorn_recipe);
    (6, "French Fries", Recipe.french_fries_recipe);
    (7, "Chocolate Chip Cookie", Recipe.cookie_recipe);
    (8, "Sandwich", Recipe.sandwich_recipe);
    (9, "Salad", Recipe.salad_recipe);
    (10, "Strawberry Cake", Recipe.strawberry_cake_recipe);
    (11, "Flower Bouquet", Recipe.bouquet_recipe);
    (12, "Curry", Recipe.curry_recipe);
    (13, "Chicken Soup", Recipe.chicken_soup_recipe);
    (14, "Hamburger", Recipe.hamburger_recipe);
    (15, "Smoothie", Recipe.smoothie_recipe);
  ]

(* Adapted From: 'Create Recipe: Helper Function' ChatGPT, 3 May. version 4.0,
   chat.openai.com. *)
let rec print_recipe_helper func n inv garden count day =
  print_endline recipe_menu;
  print_endline "Enter the number of the recipe you want to view and/or create:";
  let choice = read_line () in
  try
    let index = int_of_string choice in
    let _, name, recipe_details =
      List.find (fun (i, _, _) -> i = index) all_recipes
    in
    display_and_handle_recipe print_recipe_helper name recipe_details func n inv
      garden count day
  with
  | Failure _ -> print_endline "Please enter a number."
  | Not_found ->
      print_endline "Invalid selection, please enter a valid number.";
      print_recipe_helper func n inv garden count day

(* let rec print_recipe_helper func n inv garden count day = print_endline
   recipe_menu; print_endline "Enter the number of the recipe you want to view
   and/or create:"; let choice = read_line () in match choice with | "1" ->
   display_and_handle_recipe print_recipe_helper "Tomato Soup"
   Recipe.tomato_soup_recipe func n inv garden count day | "2" ->
   display_and_handle_recipe print_recipe_helper "Bread" Recipe.bread_recipe
   func n inv garden count day | "3" -> display_and_handle_recipe
   print_recipe_helper "Apple Pie" Recipe.apple_pie_recipe func n inv garden
   count day | "4" -> display_and_handle_recipe print_recipe_helper "Apple
   Juice" Recipe.apple_juice_recipe func n inv garden count day | "5" ->
   display_and_handle_recipe print_recipe_helper "Popcorn" Recipe.popcorn_recipe
   func n inv garden count day | "6" -> display_and_handle_recipe
   print_recipe_helper "French Fries" Recipe.french_fries_recipe func n inv
   garden count day | "7" -> display_and_handle_recipe print_recipe_helper
   "Chocolate Chip Cookie" Recipe.cookie_recipe func n inv garden count day |
   "8" -> display_and_handle_recipe print_recipe_helper "Sandwich"
   Recipe.sandwich_recipe func n inv garden count day | "9" ->
   display_and_handle_recipe print_recipe_helper "Salad" Recipe.salad_recipe
   func n inv garden count day | "10" -> display_and_handle_recipe
   print_recipe_helper "Strawberry Cake" Recipe.strawberry_cake_recipe func n
   inv garden count day | "11" -> display_and_handle_recipe print_recipe_helper
   "Flower Bouquet" Recipe.bouquet_recipe func n inv garden count day | "12" ->
   display_and_handle_recipe print_recipe_helper "Curry" Recipe.curry_recipe
   func n inv garden count day | "13" -> display_and_handle_recipe
   print_recipe_helper "Chicken Soup" Recipe.chicken_soup_recipe func n inv
   garden count day | "14" -> display_and_handle_recipe print_recipe_helper
   "Hamburger" Recipe.hamburger_recipe func n inv garden count day | "15" ->
   display_and_handle_recipe print_recipe_helper "Smoothie"
   Recipe.smoothie_recipe func n inv garden count day | _ -> print_endline
   "Invalid selection, please enter a valid number."; print_recipe_helper func n
   inv garden count day *)

let create_recipe_helper func n inv garden =
  print_recipe_helper func (n + 1) inv garden

let count = ref 0
let day = ref 1

let rec func n inv garden count day =
  match !day with
  | 50 -> print_endline "\nEnd of Garden Game"
  | _ ->
      (*if count = get_action_limit (Garden.get_plant_count garden) *)
      if !count = 10 then (
        let garden = Garden.night_change day garden in
        print_endline ("\nIt's Day " ^ string_of_int (!day + 1) ^ "!");
        count := 0;
        day := !day + 1;
        func 0 inv garden count day)
      else
        let print_menu = print_endline menu_options in
        print_menu;
        let choice = read_line () in
        print_string "";
        count := !count + 1;
        if choice = "2" then feed_garden_helper func n inv garden count day
        else if choice = "1" then buy_plant_helper func n inv garden count day
        else if choice = "3" then
          water_garden_helper func n inv garden count day
        else if choice = "4" then
          neglect_garden_helper func n inv garden count day
        else if choice = "5" then harvest_helper func n inv garden count day
        else if choice = "6" then
          observe_garden_helper func n inv garden count day
        else if choice = "7" then view_inv_helper func n inv garden count day
        else if choice = "8" then sell_helper func n inv garden count day
        else if choice = "10" then
          create_recipe_helper func n inv garden count day
        else (
          print_endline "Invalid option";
          func (n + 1) inv garden count day)

let main () = func 0 my_inventory my_garden count day
let () = main ()

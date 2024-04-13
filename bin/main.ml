open CS3110FinalProject

let my_garden = Garden.create_garden ()
let my_inventory = Inventory.create_inventory

let menu_options =
  "\n\
  \ 1. Buy Plant/Item\n\
  \ 2. Feed Plants\n\
  \ 3. Water Plants\n\
  \ 4. Neglect Plants\n\
  \ 5. Harvest\n\
  \ 6. Observe Garden\n\
  \ 7. View Inventory\n\
  \ 8. Sell \n\
  \ 9. Recipes "

(* let feed_garden_helper func n inv garden = print_endline "What category of
   plants do you want to feed?\n\ Flowers / Fruits / Vegetables / Grains /
   Defensive Items"; let plant_cat = read_line () in let new_garden = match
   plant_cat with | "Flowers" -> Garden.get_flowers garden | "Fruits" ->
   Garden.get_fruits garden | "Vegetables" -> Garden.get_vegetables garden |
   "Grains" -> Garden.get_grains garden | "Defensive Items" ->
   Garden.get_defensive_items garden | _ -> print_endline "Invalid category.
   Please note that this cost you an action"; garden in print_endline "What is
   the name of the plant you want to feed: "; let name = read_line () in let
   new_garden = Garden.feed_plants new_garden name in Garden.print new_garden;
   func (n + 1) inv new_garden *)

let feed_garden_helper func n inv garden =
  print_endline
    "What category of plants do you want to feed?\n\
     Flowers / Fruits / Vegetables / Grains / Defensive Items";
  let plant_cat = read_line () in
  let _ = Garden.print_plants_in_category plant_cat garden in
  print_endline "What is the name of the plant you want to feed: ";
  let name = read_line () in
  let new_garden = Garden.feed_plants garden name in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let water_garden_helper func n inv garden =
  print_endline
    "What category of plants do you want to water?\n\
     Flowers / Fruits / Vegetables / Grains / Defensive Items";
  let plant_cat = read_line () in
  let _ = Garden.print_plants_in_category plant_cat garden in
  print_endline "What is the name of the plant you want to feed: ";
  let name = read_line () in
  let new_garden = Garden.water_plants garden name in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let neglect_garden_helper func n inv garden =
  print_endline
    "What category of plants do you want to neglect?\n\
     Flowers / Fruits / Vegetables / Grains / Defensive Items";
  let plant_cat = read_line () in
  let _ = Garden.print_plants_in_category plant_cat garden in
  print_endline "What is the name of the plant you want to feed: ";
  let name = read_line () in
  let new_garden = Garden.neglect_plants garden name in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let buy_plant_helper func n inv garden =
  let new_store = Store.create_store in
  let () = print_endline (Store.print_store new_store) in
  let () = print_endline "Name of Item to buy?" in
  let () =
    print_endline
      "Plants\n\
      \ will\n\
      \  be added to garden, and other items will be added to the inventory"
  in
  let item = read_line () in
  let new_inv, new_garden = Store.buy_item item new_store inv garden in
  Garden.print new_garden;
  func (n + 1) new_inv new_garden
(* let () = print_endline "Type of plant? Daisy / Sunflower / Rose / Tulip\n\
   Tomato / Corn / Carrot / Onion / Potato / Wheat\n\ Apple / Peach / Strawberry
   / Orange / Cactus" in let plant_name = read_line () in let () = print_endline
   "Name your type of plant: " in let name = read_line () in let new_garden =
   Garden.add_plant plant_name name garden in Garden.print new_garden; func (n +
   1) inv new_garden *)

let harvest_helper func n inv garden =
  let () =
    print_endline
      "Type of plant\n\
      \   to harvest? Daisy / Sunflower / Rose / Tulip\n\
      \ Tomato / Corn / Carrot /\n\
      \   Onion / Potato / Wheat\n\
      \ Apple / Peach / Strawberry / Orange / Cactus"
  in
  let plant_name = read_line () in
  let new_inv, new_garden = Inventory.harvest plant_name inv garden in
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let sell_helper func n inv garden =
  let () =
    print_endline
      "Type of item to\n\
      \   sell? Daisy / Sunflower / Rose / Tulip\n\
      \ Tomato / Corn / Carrot / Onion /\n\
      \   Potato / Wheat\n\
      \ Apple / Peach / Strawberry / Orange / Cactus / Eggs / Milk\n\
      \   / Cheese"
  in
  let plant_name = read_line () in
  let () = print_endline "Quantity to sell?" in
  let qty = int_of_string (read_line ()) in
  let new_inv, new_garden = Inventory.sell plant_name qty inv garden in
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let view_inv_helper func n inv garden =
  let () = Inventory.print inv in
  func (n + 1) inv garden

let observe_menu_options =
  "\n\
  \ 1. View Flowers\n\
  \ 2. View Fruits\n\
  \ 3. View\n\
  \   Vegetables\n\
  \ 4. View Grains\n\
  \ 5. View Defensive Items\n\
  \ 6.\n\
  \   View Garden"

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
  \  1.\n\
  \   Tomato Soup 🥫 \n\
  \  2. Bread 🍞 \n\
  \  3. Apple Pie 🥧 \n\
  \  4. Apple Juice 🧃\n\
  \   \n\
  \  5. Popcorn 🍿 \n\
  \  6. French Fries 🍟 \n\
  \  7. Carrot Bread? 🥕🍞 \n\
  \  8.\n\
  \   Chocolate Chip Cookie 🍪 \n\
  \  9. Flower Bouqet 💐"

let recipe_or_menu_message recipe_func func n inv garden count day =
  let print_message =
    print_endline
      "\n\
      \ Press 1 to go back to your Recipe Book or Press 2 to go\n\
      \   back to the  menu."
  in
  print_message;
  let recipe_or_menu = read_line () in
  print_string "";
  if recipe_or_menu = "1" then recipe_func func n inv garden count day
  else if recipe_or_menu = "2" then func n inv garden count day
  else func (n + 1) inv garden count day

let rec print_recipe_helper func n inv garden count day =
  let print_recipes = print_endline recipe_menu in
  print_recipes;
  let choice = read_line () in
  print_string "";
  if choice = "1" then (
    print_endline Recipe.tomato_soup_recipe;
    recipe_or_menu_message print_recipe_helper func n inv garden count day)
  else if choice = "2" then (
    print_endline Recipe.bread_recipe;
    recipe_or_menu_message print_recipe_helper func n inv garden count day)
  else if choice = "3" then (
    print_endline Recipe.apple_pie_recipe;
    recipe_or_menu_message print_recipe_helper func n inv garden count day)
  else func (n + 1) inv garden count day

(* let view_inventory_helper func n inv garden = let inventory = Inventory.print
   in print_endline inventory *)

let count = ref 0
let day = ref 1

let rec func n inv garden count day =
  match !day with
  | 50 -> print_endline "\nEnd of Garden Game"
  | _ ->
      if !count = 10 then (
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
        else if choice = "9" then
          print_recipe_helper func n inv garden count day
        else (
          print_endline "Invalid option";
          func (n + 1) inv garden count day)

let main () = func 0 my_inventory my_garden count day
let () = main ()

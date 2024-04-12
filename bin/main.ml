(* let () = print_endline "Hello, World!" *)

(* let gameGarden = A800.Garden.create_garden in  *)
(**)
open CS3110FinalProject

let my_garden = Garden.create_garden ()
let my_inventory = Inventory.create_inventory
(* let my_garden = A800.Garden.add_plant "Flower" my_garden let my_garden =
   A800.Garden.add_plant "Peach" my_garden

   let my_garden = A800.Garden.add_plant "Peach" my_garden *)

let menu_options =
  "\n\
   1. Feed Garden\n\
   2. Buy Plant/Item\n\
   3. Harvest\n\
   4. Observe Garden\n\
   5. View Inventory\n\
   6. Sell \n\
   7. Recipes "

let feed_garden_helper func n inv garden =
  let new_garden = Garden.feed_plants garden in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let buy_plant_helper func n inv garden =
  let new_store = Store.create_store in
  let () = print_endline (Store.print_store new_store) in
  let () = print_endline "Name of Item to buy?" in
  let () =
    print_endline
      "Plants will be added to garden, and other items will be added to the \
       inventory"
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
      "Type of plant to harvest? Daisy / Sunflower / Rose / Tulip\n\
       Tomato / Corn / Carrot / Onion / Potato / Wheat\n\
       Apple / Peach / Strawberry / Orange / Cactus"
  in
  let plant_name = read_line () in
  let new_inv, new_garden = Inventory.harvest plant_name inv garden in
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let sell_helper func n inv garden =
  let () =
    print_endline
      "Type of item to sell? Daisy / Sunflower / Rose / Tulip\n\
       Tomato / Corn / Carrot / Onion / Potato / Wheat\n\
       Apple / Peach / Strawberry / Orange / Cactus / Eggs / Milk / Cheese"
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
   1. View Flowers\n\
   2. View Fruits\n\
   3. View Vegetables\n\
   4. View Grains\n\
   5. View Trees\n\
   6. View Other Plants\n\
   7. View Garden"

let observe_garden_helper func n inv garden =
  let print_menu = print_endline observe_menu_options in
  print_menu;
  let choice = read_line () in
  print_string "";
  if choice = "1" then (
    let new_garden = Garden.get_flowers garden in
    Garden.print new_garden;
    func (n + 1) inv garden)
  else if choice = "2" then (
    let new_garden = Garden.get_fruits garden in
    Garden.print new_garden;
    func (n + 1) inv garden)
  else if choice = "3" then (
    let new_garden = Garden.get_vegetables garden in
    Garden.print new_garden;
    func (n + 1) inv garden)
  else if choice = "4" then (
    let new_garden = Garden.get_grains garden in
    Garden.print new_garden;
    func (n + 1) inv garden)
  else if choice = "5" then (
    let new_garden = Garden.get_trees garden in
    Garden.print new_garden;
    func (n + 1) inv garden)
  else if choice = "6" then (
    let new_garden = Garden.get_other_plants garden in
    Garden.print new_garden;
    func (n + 1) inv garden)
  else if choice = "7" then (
    let new_garden = garden in
    Garden.print new_garden;
    func (n + 1) inv garden)

let recipe_menu =
  "\n\
  \  Select which recipe you would like to view:\n\
  \  1. Tomato Soup 🥫 \n\
  \  2. Bread 🍞 \n\
  \  3. Apple Pie 🥧 \n\
  \  4. Apple Juice 🧃 \n\
  \  5. Popcorn 🍿 \n\
  \  6. French Fries 🍟 \n\
  \  7. Carrot Bread? 🥕🍞 \n\
  \  8. Chocolate Chip Cookie 🍪 \n\
  \  9. Flower Bouqet 💐"

let recipe_or_menu_message recipe_func func n inv garden =
  let print_message =
    print_endline
      "\n\
       Press 1 to go back to your Recipe Book or Press 2 to go back to the \
       menu."
  in
  print_message;
  let recipe_or_menu = read_line () in
  print_string "";
  if recipe_or_menu = "1" then recipe_func func n inv garden
  else if recipe_or_menu = "2" then func n inv garden;
  func (n + 1) inv garden

let rec print_recipe_helper func n inv garden =
  let print_recipes = print_endline recipe_menu in
  print_recipes;
  let choice = read_line () in
  print_string "";
  if choice = "1" then (
    print_endline Recipe.tomato_soup_recipe;
    recipe_or_menu_message print_recipe_helper func n inv garden)
  else if choice = "2" then (
    print_endline Recipe.bread_recipe;
    recipe_or_menu_message print_recipe_helper func n inv garden)
  else if choice = "3" then (
    print_endline Recipe.apple_pie_recipe;
    recipe_or_menu_message print_recipe_helper func n inv garden)

(* let view_inventory_helper func n inv garden = let inventory = Inventory.print
   in print_endline inventory *)

let rec func n inv garden =
  match n with
  | 20 -> print_endline "\nEnd of Garden Game"
  | _ ->
      (* if Garden.broke garden then func 6 inv garden else *)
      let print_menu = print_endline menu_options in
      print_menu;
      let choice = read_line () in
      print_string "";
      if choice = "1" then feed_garden_helper func n inv garden
      else if choice = "2" then buy_plant_helper func n inv garden
      else if choice = "3" then harvest_helper func n inv garden
      else if choice = "4" then observe_garden_helper func n inv garden
      else if choice = "5" then view_inv_helper func n inv garden
      else if choice = "6" then sell_helper func n inv garden
      else if choice = "7" then print_recipe_helper func n inv garden

(* let new_garden = garden in A800.Garden.list_view garden; func (n + 1)
   new_garden *)

(* let rec fun2 m = match m with | -3 -> print_endline "\nYou passed 6 guesses"
   | _ -> ( print_endline "\nWhat is your guess?"; let guess = read_line () in
   print_string ""; if check_correct guess true_word (initialize ()) <> "Invalid
   input" then display_guess 0 guess true_word else (); match check_correct
   guess true_word (initialize ()) with | "Right" -> print_endline "\ngame
   ended" | "Invalid input" -> print_endline "\nInput must have length of 5 and
   be from the word dictionary"; func m | _ -> func (m + 1)) *)

let main () = func 0 my_inventory my_garden
let () = main ()

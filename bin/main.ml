(* @author Joanna Andrews(jda242), Jessica Andrews(jda375), Jeana Han (jjh358),
   Rishika Cheivirala (rrc87), Mishita Kaja (mhk225) *)

open CS3110FinalProject

let count = ref 0
let my_garden = Garden.create_garden ()
let my_inventory = Inventory.create_inventory

let menu_options =
  "  Menu:\n\
  \  [1] Store\n\
  \  [2] Tend Plants \n\
  \  [3] Harvest\n\
  \  [4] Observe Garden\n\
  \  [5] View Inventory\n\
  \  [6] Sell \n\
  \  [7] Create Recipe\n\
  \  [8] View Menu Guide and Explanation \n\
  \  [9] View Items Explanation\n\
  \  Enter your option number: "

let check_attempts attempts =
  if !attempts >= 3 then begin
    print_endline "You've exceeded the maximum number of attempts.";
    print_string menu_options;
    true
  end
  else false

let exit count func n inv garden =
  let () = count := !count - 1 in
  func n inv garden

let rec get_valid_category plant_cat =
  match plant_cat with
  | "1" | "2" | "3" | "4" | "5" | "6" -> plant_cat
  | _ ->
      print_string
        "You have entered an invalid category. Please enter a valid category \
         here: ";
      let plant_cat_1 = read_line () in
      get_valid_category plant_cat_1

let rec get_valid_custom garden plant =
  if Garden.has_plant garden plant then plant
  else begin
    print_string "Invalid plant name. Please enter a valid plant name: ";
    let item = read_line () in
    get_valid_custom garden item
  end

let rec get_valid_item item =
  if Store.has_item item then item
  else begin
    print_string "Invalid item. Please enter a valid item: ";
    let item = read_line () in
    get_valid_item item
  end

let rec get_valid_item_with_attempts item attempts =
  attempts := !attempts + 1;
  if Store.has_item item then item
  else if check_attempts attempts then item
  else begin
    print_string "Invalid item. Please enter a valid item: ";
    let item = read_line () in
    get_valid_item_with_attempts item attempts
  end

let number_to_name number =
  match number with
  | "1" -> "Flowers"
  | "2" -> "Fruits"
  | "3" -> "Vegetables"
  | "4" -> "Grains"
  | "5" -> "Defensive Items"
  | _ -> "Invalid"

let water_garden_helper func n inv garden plant_name =
  let new_garden = Garden.water_plants garden plant_name in
  let my_int = !count in
  print_endline "";
  print_endline ("You have " ^ string_of_int (10 - my_int) ^ " actions left!");
  Garden.print new_garden;
  func (n + 1) inv new_garden

let feed_garden_helper func n inv garden plant_name =
  let new_garden = Garden.feed_plants garden plant_name in
  let my_int = !count in
  print_endline "";
  print_endline ("You have " ^ string_of_int (10 - my_int) ^ " actions left!");
  Garden.print new_garden;
  func (n + 1) inv new_garden

let neglect_garden_helper func n inv garden plant_name =
  let new_garden = Garden.neglect_plants garden plant_name in
  let my_int = !count in
  print_endline "";
  print_endline ("You have " ^ string_of_int (10 - my_int) ^ " actions left!");
  Garden.print new_garden;
  func (n + 1) inv new_garden

let tend_start_message () =
  print_endline
    "What category of plants do you want to tend to? Choose a number:\n\
    \ [1] Flowers           [2] Fruits \n\
    \ [3] Vegetables        [4] Grains \n\
    \ [5] Defensive Items   [X] Main Menu"

let tend_question () =
  print_endline
    "\n\
     What is the name of the plant do you want to tend?\n\
     (Press X to go back to the Main Menu)"

let tend_options () =
  print_endline
    "How do you want to tend this plant: Water(-$0.1) / Feed(-$0.1) / Neglect?"

let tend_actions tend_func action func n inv garden plant_name =
  match action with
  | "water" -> water_garden_helper func n inv garden plant_name
  | "Water" -> water_garden_helper func n inv garden plant_name
  | "feed" -> feed_garden_helper func n inv garden plant_name
  | "Feed" -> feed_garden_helper func n inv garden plant_name
  | "neglect" -> neglect_garden_helper func n inv garden plant_name
  | "Neglect" -> neglect_garden_helper func n inv garden plant_name
  | _ ->
      print_endline "Invalid action. Please try again.";
      tend_func func n inv garden

let rec tend_garden_helper func n inv garden =
  tend_start_message ();
  let plant_numb = read_line () in
  let plant_cat = String.trim plant_numb in
  if plant_cat = "X" || plant_cat = "x" then exit count func n inv garden
  else
    let name_from_numb = number_to_name plant_cat in
    if name_from_numb = "Invalid" then begin
      print_endline "Invalid category number. Please try again.";
      tend_garden_helper func n inv garden
    end
    else begin
      let _ = Garden.print_plants_in_category name_from_numb garden in
      tend_question ();
      let item = read_line () in
      if item = "X" || item = "x" then exit count func n inv garden
      else
        let plant_name = get_valid_custom garden item in
        tend_options ();
        let action = read_line () |> String.trim in
        tend_actions tend_garden_helper action func n inv garden plant_name
    end

let display_store_and_prompt_purchase new_store acc_plant_cat =
  print_endline (Store.print_store new_store acc_plant_cat);
  print_endline
    "Plants will be added to garden, and other items will be added to the \
     inventory. \n\n\
     What is the type of item you want to buy (Press X to go back to the \
     Menu): "

let buy_start_message () =
  print_endline
    "What category of plants do you want to tend to? Choose a number:\n\
    \ [1] Flowers           [2] Fruits \n\
    \ [3] Vegetables        [4] Grains \n\
    \ [5] Defensive Items   [6] Other   [X] Main Menu"

let buy_plant_helper func n inv garden =
  let new_store = Store.create_store in
  buy_start_message ();
  let plant_cat = read_line () in
  if plant_cat = "X" || plant_cat = "x" then exit count func n inv garden
  else
    let acc_plant_cat = get_valid_category plant_cat in
    display_store_and_prompt_purchase new_store acc_plant_cat;
    let item = read_line () in
    if item = "X" || item = "x" then exit count func n inv garden
    else
      let acc_item = get_valid_item item in
      let new_inv, new_garden = Store.buy_item acc_item new_store inv garden in
      let my_int = !count in
      print_endline "";
      print_endline
        ("You have " ^ string_of_int (10 - my_int) ^ " actions left!");
      Garden.print new_garden;
      func (n + 1) new_inv new_garden

let harvest_numb_to_name number =
  match number with
  | "1" -> "Daisy"
  | "2" -> "Sunflower"
  | "3" -> "Rose"
  | "4" -> "Tulip"
  | "5" -> "Lemon"
  | "6" -> "Pineapple"
  | "7" -> "Apple"
  | "8" -> "Peach"
  | "9" -> "Strawberry"
  | "10" -> "Mango"
  | "11" -> "Tomato"
  | "12" -> "Lettuce"
  | "13" -> "Bell Pepper"
  | "14" -> "Onion"
  | "15" -> "Potato"
  | "16" -> "Rice"
  | "17" -> "Wheat"
  | "18" -> "Corn"
  | "19" -> "Clover"
  | "20" -> "Cactus"
  | _ -> "Invalid"

let harvest_message () =
  print_endline
    "\n\
     What type of plant do you want to Harvest? ⛏ (Please select the number): \n\
    \ [1] Daisy        [2] Sunflower   [3] Rose        [4] Tulip \n\
    \ [5] Lemon        [6] Pineapple   [7] Apple       [8] Peach \n\
    \ [9] Strawberry   [10] Mango      [11] Tomato     [12] Lettuce \n\
    \ [13] Bell Pepper [14] Onion      [15] Potato     [16] Rice \n\
    \ [17] Wheat       [18] Corn       [19] Clover     [20] Cactus\n\
    \ [X] Main Menu"

let harvest_helper func n inv garden =
  print_endline "You only have 3 chances to type a valid plant";
  Garden.print garden;
  print_endline "You can harvest any fully grown plant";
  let () = harvest_message () in
  let attempts = ref 0 in
  let item = read_line () in
  if item = "X" || item = "x" then exit count func n inv garden
  else
    let real_item = harvest_numb_to_name item in
    let item_name = get_valid_item_with_attempts real_item attempts in
    let new_inv, new_garden = Inventory.harvest item_name inv garden in
    let my_int = !count in
    print_endline "";
    print_endline ("You have " ^ string_of_int (10 - my_int) ^ " actions left!");
    Garden.print new_garden;
    func (n + 1) new_inv new_garden

let first_numb_to_name numb =
  match numb with
  | "1" -> "Daisy"
  | "2" -> "Sunflower"
  | "3" -> "Rose"
  | "4" -> "Tulip"
  | "5" -> "Lemon"
  | "6" -> "Pineapple"
  | "7" -> "Apple"
  | "8" -> "Peach"
  | "9" -> "Strawberry"
  | "10" -> "Mango"
  | "11" -> "Tomato"
  | "12" -> "Lettuce"
  | "13" -> "Bell Pepper"
  | "14" -> "Onion"
  | "15" -> "Potato"
  | _ -> "Invalid"

let second_numb_to_name numb =
  match numb with
  | "16" -> "Rice"
  | "17" -> "Wheat"
  | "18" -> "Corn"
  | "19" -> "Clover"
  | "20" -> "Cactus"
  | "21" -> "Cheese"
  | "22" -> "Eggs"
  | "23" -> "Milk"
  | "24" -> "Water"
  | "25" -> "Butter"
  | "26" -> "Sugar"
  | "27" -> "Chocolate"
  | "28" -> "Plant Food"
  | "29" -> "Ladybug"
  | "30" -> "Beef"
  | _ -> "Invalid"

let third_numb_to_name numb =
  match numb with
  | "31" -> "Chicken"
  | "32" -> "Apple Pie"
  | "33" -> "Tomato Soup"
  | "34" -> "Bread"
  | "35" -> "Apple Juice"
  | "36" -> "Popcorn"
  | "37" -> "French Fries"
  | "38" -> "Chocolate Chip Cookie"
  | "39" -> "Sandwich"
  | "40" -> "Salad"
  | "41" -> "Strawberry Cake"
  | "42" -> "Bouquet"
  | "43" -> "Curry"
  | "44" -> "Chicken Soup"
  | "45" -> "Hamburger"
  | "46" -> "Smoothie"
  | _ -> "Invalid"

let sell_numb_to_name numb =
  if int_of_string numb < 16 then first_numb_to_name numb
  else if int_of_string numb >= 16 && int_of_string numb < 31 then
    second_numb_to_name numb
  else if int_of_string numb >= 31 && int_of_string numb <= 46 then
    third_numb_to_name numb
  else "Invalid item number"

let sell_items () =
  print_endline
    "\n\
    \ What type of item do you want to sell from your inventory: \n\
    \ [1] Daisy         [2] Sunflower     [3] Rose        [4] Tulip \n\
    \ [5] Lemon         [6] Pineapple     [7] Apple       [8] Peach \n\
    \ [9] Strawberry    [10] Mango        [11] Tomato     [12] Lettuce \n\
    \ [13] Bell Pepper  [14] Onion        [15] Potato     [16] Rice       \n\
    \ [17] Wheat        [18] Corn         [19] Clover     [20] Cactus     \n\
    \ [21] Cheese       [22] Eggs         [23] Milk       [24] Water      \n\
    \ [25] Butter       [26] Sugar        [27] Chocolate  [28] Plant Food \n\
    \ [29] LadyBug      [30] Beef         [31] Chicken    [32] Apple Pie \n\
    \ [33] Tomato Soup  [34] Bread        [35] Apple Juice \n\
    \ [36] Popcorn      [37] French Fries [38] Chocolate Chip Cookie \n\
    \ [39] Sandwich     [40] Salad        [41] Strawberry Cake \n\
    \ [42] Bouquet      [43] Curry        [44] Chicken Soup \n\
    \ [45] Hamburger    [46] Smoothie     [X] Main Menu"

let sell_and_print inv garden acc_item func n =
  print_string ("How many " ^ acc_item ^ " do you want to sell: ");
  let qty = int_of_string (read_line ()) in
  let new_inv, new_garden = Inventory.sell acc_item qty inv garden in
  let my_int = !count in
  print_endline "";
  print_endline ("You have " ^ string_of_int (10 - my_int) ^ " actions left!");
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let sell_item_recipe acc_item inv garden func n =
  print_string ("How many " ^ acc_item ^ " do you want to sell: ");
  let qty = int_of_string (read_line ()) in
  let new_inv, new_garden = Recipe.sell_recipe acc_item qty inv garden in
  let my_int = !count in
  print_endline "";
  print_endline ("You have " ^ string_of_int (10 - my_int) ^ " actions left!");
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let sell_helper func n inv garden =
  print_string "You only have 3 chances to type a valid item\n";
  print_endline "These are the items in your inventory: ";
  Inventory.print inv;
  sell_items ();
  let attempts = ref 0 in
  let item = read_line () in
  if item = "X" || item = "x" then exit count func n inv garden
  else
    let name = sell_numb_to_name item in
    let acc_item = get_valid_item_with_attempts name attempts in
    if int_of_string item >= 32 && int_of_string item <= 46 then
      sell_item_recipe acc_item inv garden func n
    else sell_and_print inv garden acc_item func n

let view_inv_helper func n inv garden =
  let () = Inventory.print inv in
  func (n + 1) inv garden

let observe_menu_options =
  "\n\
  \  [1] View Flowers \n\
  \  [2] View Fruits \n\
  \  [3] View Vegetables \n\
  \  [4] View Grains \n\
  \  [5] View Defensive Items \n\
  \  [6] View Garden"

let print_menu () = print_endline observe_menu_options

let print_remaining_actions count =
  let action_left = 10 - count in
  print_endline "";
  print_endline ("You have " ^ string_of_int action_left ^ "\n   actions left!")

let handle_choice garden_getter garden count =
  let new_garden = garden_getter garden in
  print_remaining_actions count;
  Garden.print new_garden

let observe_garden_helper func n inv garden count day =
  let choice =
    print_menu ();
    read_line ()
  in
  count := !count + 1;
  let _ =
    match choice with
    | "1" -> handle_choice Garden.get_flowers garden !count
    | "2" -> handle_choice Garden.get_fruits garden !count
    | "3" -> handle_choice Garden.get_vegetables garden !count
    | "4" -> handle_choice Garden.get_grains garden !count
    | "5" -> handle_choice Garden.get_defensive_items garden !count
    | "6" -> Garden.print garden
    | _ -> func (n + 1) inv garden count day
  in
  func (n + 1) inv garden count day

let recipe_menu =
  "\n\
  \  Select a number to view/create a recipe:\n\
  \  [1] Tomato Soup 🥫 \n\
  \  [2] Bread 🍞 \n\
  \  [3] Apple Pie 🥧 \n\
  \  [4] Apple Juice 🧃\n\
  \  [5] Popcorn 🍿 \n\
  \  [6] French Fries 🍟 \n\
  \  [7] Chocolate Chip Cookie 🍪 \n\
  \  [8] Sandwich 🥪 \n\
  \  [9] Salad 🥗 \n\
  \  [10] Strawberry Cake 🍰 \n\
  \  [11] Bouquet 💐\n\
  \  [12] Curry 🍛 \n\
  \  [13] Chicken Soup 🥘 \n\
  \  [14] Hamburger 🍔 \n\
  \  [15] Smoothie 🍹"

let missing_ingredient_message recipe_function func n inv garden count day =
  print_endline
    "Choose the following number to proceed: \n\
    \  [1] Go back to Recipes \n\
    \  [X] Go back to Menu";
  match read_line () with
  | "1" -> recipe_function func n inv garden count day
  | "X" | "x" -> func n inv garden count day
  | _ -> print_endline "Invalid option"

let rec display_and_handle_recipe recipe_func food_name recipe_name func n inv
    garden count day =
  print_endline recipe_name;
  print_endline
    "\n\
    \  Choose the following options to proceed: \n\
    \  [1] Create this Recipe \n\
    \  [2] Go back to Recipes \n\
    \  [X] Go back to Main Menu ";
  match read_line () with
  | "1" ->
      let updated_inv = Recipe.create_recipe food_name inv in
      missing_ingredient_message recipe_func func n updated_inv garden count day
  | "2" -> recipe_func func n inv garden count day
  | "X" | "x" -> func n inv garden count day
  | _ ->
      print_endline "Invalid option";
      display_and_handle_recipe recipe_func food_name recipe_name func n inv
        garden count day

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
    (10, "Strawberry\n   Cake", Recipe.strawberry_cake_recipe);
    (11, "Flower Bouquet", Recipe.bouquet_recipe);
    (12, "Curry", Recipe.curry_recipe);
    (13, "Chicken\n   Soup", Recipe.chicken_soup_recipe);
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
  | Failure _ ->
      print_endline "Please enter a number.";
      print_recipe_helper func n inv garden count day
  | Not_found ->
      print_endline "Invalid selection, please enter a valid number.";
      print_recipe_helper func n inv garden count day

let create_recipe_helper func n inv garden =
  print_recipe_helper func (n + 1) inv garden

let day = ref 1
let check_terminated garden = Garden.get_money garden <= 0.0

let print_in_box text padding =
  let lines = String.split_on_char '\n' text in
  let max_length =
    List.fold_left (fun acc line -> max acc (String.length line)) 0 lines
  in
  let box_length = max_length + (2 * padding) in
  let thick_border = "+" ^ String.make box_length '=' ^ "+" in
  Printf.printf "%s\n" thick_border;
  for _ = 1 to padding do
    Printf.printf "%s\n" (String.make box_length ' ')
  done;
  List.iter
    (fun line ->
      let padding_left = String.make padding ' ' in
      let padding_right =
        String.make (box_length - String.length line - padding) ' '
      in
      Printf.printf "%s%s%s\n" padding_left line padding_right)
    lines;
  for _ = 1 to padding do
    Printf.printf "%s\n" (String.make box_length ' ')
  done;
  Printf.printf "%s\n" thick_border

let string_menu_guide () =
  print_endline
    "Menu Option Explanation:\n\
     [1] Store In order to start your garden you need to buy a plant seed. You \
     can also buy other objects such as defensive items for your garden that \
     protect your garden from random events, and special ingredients to use in \
     recipes.\n\n\
     [2] Tend Plants: To take care of your plants you can choose to feed, \
     water, or neglect them. Feeding increases their height, and watering \
     increases their hydration but they both cost some amount of money. If you \
     are low on funds you can choose to neglect your plant.\n\n\
     [3] Harvest: Once a plant has reached its maximum height you can harvest \
     it and it will be added to your inventory\n\n\
     [4] Observe Garden: View the garden and see what your plants are looking \
     like right now\n\n\
     [5] View Inventory: view all the items you have bought or harvested\n\n\
     [6] Sell: Sell off plants and recipe items to make more money!\n\n\
     [7] Create Recipe: Check out all the available recipes, and craft your \
     own which will be added to your inventory."

let string_view_items () =
  print_endline
    "You will have the opportunity to buy plants for your garden and add items \
     to your inventory! \n\n\
    \ Here are plants you can add do your garden: \n\
    \    [1] Daisy        [2] Sunflower   [3] Rose        [4] Tulip \n\
    \    [5] Lemon        [6] Pineapple   [7] Apple       [8] Peach \n\
    \    [9] Strawberry   [10] Mango      [11] Tomato     [12] Lettuce \n\
    \    [13] Bell Pepper [14] Onion      [15] Potato     [16] Rice \n\
    \    [17] Wheat       [18] Corn       [19] Clover     [20] Cactus\n\n\
    \ You can harvest these plants when they reach a height of 5! \n\
    \ Once a plant is harvested, it is placed in the inventory!\n\n\
     --------------------------------------------------------------------------------\n\
    \ Here are items you can buy and put directly in your inventory: \n\
    \    [1] Cheese        [2] Eggs      [3] Milk        [4] Water \n\
    \    [5] Butter        [6] Sugar     [7] Chocolate   [8] Plant Food \n\
    \    [9] Beef          [10] Chicken\n\
     --------------------------------------------------------------------------------\n\
    \ Here are defensive items that you can buy to defend your garden from \
     tragedies:\n\
    \    [1] Cactus        [2] Clover       [3] Ladybug\n\
     --------------------------------------------------------------------------------\n\
    \  Select the following number to continue: \n\
    \    [1] Back to Homepage\n\
    \    [2] Play the game!"

let continue_message () =
  print_endline "Scroll up to go back to the Menu and continue playing!"

let func_helper choice func n inv garden count day =
  if choice = "2" then tend_garden_helper func n inv garden count day
  else if choice = "1" then buy_plant_helper func n inv garden count day
  else if choice = "3" then harvest_helper func n inv garden count day
  else if choice = "4" then observe_garden_helper func n inv garden count day
  else if choice = "5" then view_inv_helper func n inv garden count day
  else if choice = "6" then sell_helper func n inv garden count day
  else if choice = "7" then create_recipe_helper func n inv garden count day
  else if choice = "8" then (
    string_menu_guide ();
    func n inv garden count day)
  else if choice = "9" then (
    string_view_items ();
    continue_message ();
    func n inv garden count day)
  else (
    print_endline "Invalid option";
    func (n + 1) inv garden count day)

let rec func n inv garden count day =
  match !day with
  | 50 -> print_endline "\nEnd of Garden Game"
  | _ ->
      if check_terminated garden = true then
        let () = print_endline "You ran out of money!!" in
        let last_day = ref 50 in
        func (n + 1) inv garden count last_day
      else if !count = 10 then (
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
        func_helper choice func n inv garden count day

let print_title =
  let text =
    " 🌷🌷🌷  Garden Gameplay  🌷🌷🌷 \nPress 'P' to Play or 'R' to View Rules"
  in
  print_in_box text 5

let rec string_rules_menu () =
  print_endline
    "Hello, welcome to our garden game! The goal of this game is to keep all \
     your plants alive, and make as much money as you can in the next 50 days. \n\
    \ Select the following number to continue: \n\
    \     [1] Main Menu Guide and Explanation\n\
    \     [2] View items Explanations\n\
    \     [3] Play the game!";
  let input = read_line () in
  match input with
  | "1" -> menu_guide_rules ()
  | "2" -> items_rules ()
  | "3" -> func 0 my_inventory my_garden count day
  | _ ->
      print_endline "Invalid Input. Please select a valid number";
      string_rules_menu ()

and menu_guide_rules () =
  string_menu_guide ();
  print_endline
    " \n\
    \  Select the following number to continue: \n\
    \     [1] Back to Homepage \n\
    \     [2] Play the game!";
  let input = read_line () in
  match input with
  | "1" -> string_rules_menu ()
  | "2" -> func 0 my_inventory my_garden count day
  | _ ->
      print_endline "Invalid Input. Please select a valid number";
      menu_guide_rules ()

and rules_menu_helper () =
  string_rules_menu ();
  let input = read_line () in
  match input with
  | "1" -> menu_guide_rules ()
  | "2" -> items_rules ()
  | "3" -> func 0 my_inventory my_garden count day
  | _ -> print_endline "Invalid"

and items_rules () =
  string_view_items ();
  let input = read_line () in
  match input with
  | "1" -> rules_menu_helper ()
  | "2" -> func 0 my_inventory my_garden count day
  | _ -> print_endline "Invalid Input. Please select a valid number"

let rec start_game () =
  string_rules_menu ();
  let input = read_line () in
  match input with
  | "1" -> menu_guide_rules ()
  | "2" -> items_rules ()
  | "3" -> func 0 my_inventory my_garden count day
  | _ -> start_game ()

let opening () =
  print_title;
  let user_input = read_line () in
  if user_input = "P" || user_input = "p" then
    func 0 my_inventory my_garden count day
  else if user_input = "R" || user_input = "r" then start_game ()

let main () = opening ()
let () = main ()

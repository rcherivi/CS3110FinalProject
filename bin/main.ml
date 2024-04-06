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
   2. Add Plant\n\
   3. Harvest\n\
   4. Observe Garden\n\
   5. View Inventory"

let feed_garden_helper func n inv garden =
  let new_garden = Garden.feed_plants garden in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let add_plant_helper func n inv garden =
  let () =
    print_endline "Type of plant? Flower / Peach / Strawberry / Cactus"
  in
  let plant_name = read_line () in
  let () = print_endline "Name your type of plant: " in
  let name = read_line () in
  let new_garden = Garden.add_plant plant_name name garden in
  Garden.print new_garden;
  func (n + 1) inv new_garden

let harvest_helper func n inv garden =
  let () =
    print_endline
      "Type of plant to harvest? Flower / Peach / Strawberry / Cactus"
  in
  let plant_name = read_line () in
  let new_inv, new_garden = Inventory.harvest plant_name inv garden in
  Garden.print new_garden;
  func (n + 1) new_inv new_garden

let observe_menu_options = "\n1. View Flowers\n2. View Fruits\n3. View Garden"

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
    let new_garden = garden in
    Garden.print new_garden;
    func (n + 1) inv garden)

(* let view_inventory_helper func n inv garden = let inventory = Inventory.print
   in print_endline inventory *)

let rec func n inv garden =
  match n with
  | 15 -> print_endline "\nEnd of Garden Game"
  | _ ->
      (* if Garden.broke garden then func 6 inv garden else *)
      let print_menu = print_endline menu_options in
      print_menu;
      let choice = read_line () in
      print_string "";
      if choice = "1" then feed_garden_helper func n inv garden
      else if choice = "2" then add_plant_helper func n inv garden
      else if choice = "3" then harvest_helper func n inv garden
      else if choice = "4" then observe_garden_helper func n inv garden

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

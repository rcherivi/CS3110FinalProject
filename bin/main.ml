(* let () = print_endline "Hello, World!" *)

(* let gameGarden = A800.Garden.create_garden in  *)
(**)
open CS3110FinalProject

let my_garden = Garden.create_garden
let my_inventory = Inventory.create_inventory
(* let my_garden = A800.Garden.add_plant "Flower" my_garden let my_garden =
   A800.Garden.add_plant "Peach" my_garden

   let my_garden = A800.Garden.add_plant "Peach" my_garden *)

let rec func n inv garden =
  match n with
  | 6 -> print_endline "\nEnd of Garden Game"
  | _ ->
      (* if Garden.broke garden then func 6 inv garden else *)
      let print_menu =
        print_endline
          "\n1. Feed Garden\n2. Add Plant\n3. Harvest\n4. Observe Garden"
      in
      print_menu;
      let choice = read_line () in
      print_string "";
      if choice = "1" then (
        let new_garden = Garden.feed_plants garden in
        Garden.print new_garden;
        func (n + 1) inv new_garden)
      else if choice = "2" then (
        let () =
          print_endline "Type of plant? Flower / Peach / Strawberry / Cactus"
        in
        let plant_name = read_line () in
        let new_garden = Garden.add_plant plant_name garden in
        Garden.print new_garden;
        func (n + 1) inv new_garden)
      else if choice = "3" then (
        let () =
          print_endline
            "Type of plant to harvest? Flower / Peach / Strawberry / Cactus"
        in
        let plant_name = read_line () in
        let new_inv, new_garden = Inventory.harvest plant_name inv garden in
        Garden.print new_garden;
        func (n + 1) new_inv new_garden)
      else
        let new_garden = garden in
        Garden.print new_garden;
        func (n + 1) inv new_garden
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

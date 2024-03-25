open CS3110FinalProject

let my_garden = Garden.create_garden
let my_garden = Garden.add_plant "Flower" my_garden
let my_garden = Garden.add_plant "Peach" my_garden
let my_garden = Garden.add_plant "Peach" my_garden

let rec func n garden =
  match n with
  | 6 -> print_endline "\nEnd of Garden Game"
  | _ ->
      let print_menu = print_endline "\n1. Feed Garden\n2. Observe Garden" in
      print_menu;
      let choice = read_line () in
      print_string "";
      if choice = "1" then (
        let new_garden = Garden.feed_plants garden in
        Garden.print (Garden.feed_plants garden);
        func (n + 1) new_garden)
      else
        let new_garden = garden in
        Garden.print garden;
        func (n + 1) new_garden

let main () = func 0 my_garden
let () = main ()

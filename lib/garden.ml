type  t = Garden of Plant.t list

let create_garden = Garden []

let add_plant plant_name garden = 
  match garden with
  | Garden lst -> Garden ((Plant.create_plant plant_name) :: lst)

let feed_plants garden = match garden with
| Garden lst -> Garden (List.map Plant.feed lst) 


let printable garden = 
match garden with
| Garden lst -> List.fold_left (fun x y -> x ^ " " ^ y) "" (List.map (fun plant -> Plant.print_plant plant) lst) 

let print garden = print_string (printable garden)


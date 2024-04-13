type cell =
  | Empty
  | Plant of Plant.t

type t = {
  cells : cell array array;
  money : float;
}

let create_garden () = { cells = Array.make_matrix 5 10 Empty; money = 50.00 }

(**WROTE NEW ONE BELOW*)
let inc_money plant_type garden =
  match garden with
  | { cells; money = m } -> (
      match plant_type with
      | "Flower" -> { cells; money = m +. 2.00 }
      | "Peach" -> { cells; money = m +. 7.00 }
      | "Strawberry" -> { cells; money = m +. 5.00 }
      | "Cactus" -> { cells; money = m +. 3.00 }
      | _ -> { cells; money = m +. 7.00 })

(* let inc_money (plant_type : Plant.t) garden = match garden with | { cells;
   money = m } -> if plant_type = Plant.get_type plant_type then { cells; money
   = m +. Plant.get_sale_price plant_type +. 2.00 } else { cells; money = m +.
   7.00 } *)

let inc_money_amt amt garden =
  match garden with
  | { cells; money = m } -> { cells; money = m +. amt }

let show_money garden =
  match garden with
  | { cells = _; money = m } -> print_endline ("$ " ^ string_of_float m)

(*adapted from ChatGPT*)
let add_plant plant_name name garden =
  match garden with
  | { cells; money = m } -> (
      let rec find_next_empty_cell i j =
        if i >= Array.length cells then None
        else if j >= Array.length cells.(0) then find_next_empty_cell (i + 1) 0
        else
          match cells.(i).(j) with
          | Empty -> Some (i, j)
          | _ -> find_next_empty_cell i (j + 1)
      in
      match find_next_empty_cell 0 0 with
      | None -> garden
      | Some (i, j) ->
          let new_cells = Array.copy cells in
          new_cells.(i).(j) <- Plant (Plant.create_plant plant_name name);
          { cells = new_cells; money = m })

(* let feed_plants garden plant_name = match garden with | { cells; money = m }
   -> let new_cells = Array.map (Array.map (fun x -> match x with | Plant plant
   -> Plant (Plant.feed plant plant_name) | Empty -> Empty)) cells in { cells =
   new_cells; money = m -. 0.1 } *)

let feed_plants garden name =
  match garden with
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun x ->
               match x with
               | Plant plant -> Plant (Plant.feed plant name)
               | Empty -> Empty))
          cells
      in
      { cells = new_cells; money = m -. 0.1 }

(**idk money cost for water*)
let water_plants garden plant_name =
  match garden with
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun x ->
               match x with
               | Plant plant -> Plant (Plant.water plant plant_name)
               | Empty -> Empty))
          cells
      in
      { cells = new_cells; money = m -. 0.1 }

(**idk money cost for water*)
let neglect_plants garden plant_name =
  match garden with
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun x ->
               match x with
               | Plant plant -> Plant (Plant.neglect plant plant_name)
               | Empty -> Empty))
          cells
      in
      { cells = new_cells; money = m -. 0.1 }

(*adapted from ChatGPT*)
let count_plant plant_type garden =
  match garden with
  | { cells; money = _ } ->
      let count = ref 0 in
      Array.iter
        (fun row ->
          Array.iter
            (fun x ->
              match x with
              | Plant plant
                when Plant.get_type plant = plant_type
                     && Plant.get_height plant > 5 -> count := !count + 1
              | _ -> ())
            row)
        cells;
      !count

let remove_plant plant_type garden =
  match garden with
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun x ->
               match x with
               | Plant plant
                 when Plant.get_type plant = plant_type
                      && Plant.get_height plant > 5 -> Empty
               | _ -> x))
          cells
      in
      { cells = new_cells; money = m }

let get_flowers = function
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun cell ->
               match cell with
               | Plant plant ->
                   let plant_type = Plant.get_type plant in
                   if
                     plant_type = "Daisy" || plant_type = "Sunflower"
                     || plant_type = "Rose" || plant_type = "Tulip"
                   then cell
                   else Empty
               | _ -> Empty))
          cells
      in
      { cells = new_cells; money = m }

let get_fruits = function
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun cell ->
               match cell with
               | Plant plant ->
                   let plant_type = Plant.get_type plant in
                   if
                     plant_type = "Peach" || plant_type = "Strawberry"
                     || plant_type = "Apple" || plant_type = "Lemon"
                     || plant_type = "Mango" || plant_type = "Pineapple"
                   then cell
                   else Empty
               | _ -> Empty))
          cells
      in
      { cells = new_cells; money = m }

let get_vegetables = function
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun cell ->
               match cell with
               | Plant plant ->
                   let plant_type = Plant.get_type plant in
                   if
                     plant_type = "Onion" || plant_type = "Potato"
                     || plant_type = "Bell Pepper" || plant_type = "Lettuce"
                     || plant_type = "Tomato"
                   then cell
                   else Empty
               | _ -> Empty))
          cells
      in
      { cells = new_cells; money = m }

let get_grains = function
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun cell ->
               match cell with
               | Plant plant ->
                   let plant_type = Plant.get_type plant in
                   if
                     plant_type = "Wheat" || plant_type = "Corn"
                     || plant_type = "Rice"
                   then cell
                   else Empty
               | _ -> Empty))
          cells
      in
      { cells = new_cells; money = m }

let get_defensive_items = function
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun cell ->
               match cell with
               | Plant plant ->
                   let plant_type = Plant.get_type plant in
                   if plant_type = "Cactus" || plant_type = "Clover" then cell
                   else Empty
               | _ -> Empty))
          cells
      in
      { cells = new_cells; money = m }

let print garden =
  show_money garden;
  for i = 0 to 4 do
    print_string "| ";
    for j = 0 to 9 do
      match garden.cells.(i).(j) with
      | Empty -> print_string " "
      | Plant plant -> print_string (Plant.print_plant plant)
    done;
    print_endline "|"
  done

(*adapted from ChatGPT*)
let filter_plants_by_category category garden =
  let rec filter_row row =
    match row with
    | [] -> []
    | cell :: rest -> (
        match cell with
        | Plant plant ->
            let plant_category = Plant.get_category plant in
            if plant_category = category then [ plant ] @ filter_row rest
            else filter_row rest
        | _ -> filter_row rest)
  in
  let rec filter_rows rows =
    match rows with
    | [] -> []
    | row :: rest -> filter_row row :: filter_rows rest
  in
  match garden with
  | { cells; _ } -> filter_rows (Array.to_list (Array.map Array.to_list cells))

let print_plants_in_category category garden =
  let filtered_plants = filter_plants_by_category category garden in
  let print_header () =
    Printf.printf
      "+------------+---------+------------+--------+---------+-----------+\n";
    Printf.printf
      "|    Name    | Visual  |   Type     | Height |  Price  | Hydration |\n";
    Printf.printf
      "+------------+---------+------------+--------+---------+-----------+\n"
  in
  let print_plant plant =
    let name = Plant.get_name plant in
    let visual_rep = Plant.print_plant plant in
    let ptype = Plant.get_type plant in
    let height = Plant.get_height plant in
    let price = Plant.get_price plant in
    let hydration = Plant.get_hydration plant in
    Printf.printf "| %-10s| %-8s| %-11s| %-7d| %-8.2f| %-9d |\n" name visual_rep
      ptype height price hydration;
    ()
  in
  let print_footer () =
    Printf.printf
      "+------------+---------+------------+--------+---------+-----------+\n"
  in
  let rec print_plants plants =
    match plants with
    | [] -> ()
    | row :: rest ->
        List.iter (fun plant -> print_plant plant) row;
        print_plants rest
  in
  print_header ();
  print_plants filtered_plants;
  print_footer ()

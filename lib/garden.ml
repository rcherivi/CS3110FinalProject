type cell =
  | Empty
  | Plant of Plant.t

type t = {
  cells : cell array array;
  money : float;
  plant_count : int;
  lucky : int;
  defense : int;
}

let create_garden () =
  {
    cells = Array.make_matrix 5 10 Empty;
    money = 50.00;
    plant_count = 0;
    lucky = 0;
    defense = 0;
  }

let get_store_price item_name =
  match item_name with
  | "Daisy"
  | "Strawberry"
  | "Sunflower"
  | "Rose"
  | "Tulip"
  | "Tomato"
  | "Lemon"
  | "Pineapple"
  | "Onion"
  | "Potato"
  | "Wheat"
  | "Apple"
  | "Corn"
  | "Peach"
  | "Cactus"
  | "Clover"
  | "Rice"
  | "Lettuce"
  | "Bell Pepper"
  | "Mango" -> Plant.get_price (Plant.create_plant item_name "")
  | "Cheese" -> 5.0
  | "Eggs" -> 3.0
  | "Milk" -> 5.0
  | "Water" -> 1.0
  | "Butter" -> 2.0
  | "Chicken" -> 5.50
  | "Sugar" -> 1.5
  | "Chocolate" -> 3.20
  | "Plant Food" -> 2.0
  | "Lady Bug" -> 8.0
  | "Beef" -> 6.0
  | _ -> 0.0

let inc_money (plant_type : string) (garden : t) : t =
  let price = get_store_price plant_type in
  match garden with
  | { cells; money = m; plant_count; lucky; defense } ->
      { cells; money = m +. price +. 2.0; plant_count; lucky; defense }

let get_garden_cell garden = garden.cells
let get_garden_lucky garden = garden.lucky
let get_garden_defense garden = garden.defense

let get_plant_count garden =
  match garden with
  | { plant_count = pc; _ } -> pc

let incr_luck garden =
  match garden with
  | { cells; money; plant_count; lucky = l; defense } ->
      { cells; money; plant_count; lucky = l + 1; defense }

let incr_defense garden =
  match garden with
  | { cells; money; plant_count; lucky; defense = d } ->
      { cells; money; plant_count; lucky; defense = d + 1 }

let get_luck garden =
  match garden with
  | { lucky = l; _ } -> l

let determine_threshold luck_value =
  if luck_value < 5 then 0.6
  else if luck_value < 10 then 0.5
  else if luck_value < 15 then 0.4
  else if luck_value < 20 then 0.3
  else 0.2

let determine_tragedy_threshold defense_value =
  if defense_value < 5 then 0.5
  else if defense_value < 10 then 0.4
  else if defense_value < 15 then 0.3
  else if defense_value < 20 then 0.2
  else 0.1

let inc_money_amt amt garden =
  match garden with
  | { cells; money = m; plant_count; lucky; defense } ->
      { cells; money = m +. amt; plant_count; lucky; defense }

let show_money garden =
  match garden with
  | { cells = _; money = m; plant_count = _; lucky = _; defense = _ } ->
      print_endline ("$ " ^ string_of_float m)

let get_money garden =
  match garden with
  | { cells = _; money = m; plant_count = _; lucky = _; defense = _ } -> m

(*adapted from ChatGPT*)
let add_plant plant_name name garden =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } -> (
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
          {
            cells = new_cells;
            money = m;
            plant_count = p + 1;
            lucky = l;
            defense = d;
          })

(* let feed_plants garden plant_name = match garden with | { cells; money = m }
   -> let new_cells = Array.map (Array.map (fun x -> match x with | Plant plant
   -> Plant (Plant.feed plant plant_name) | Empty -> Empty)) cells in { cells =
   new_cells; money = m -. 0.1 } *)

let feed_plants garden name =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
      let new_cells =
        Array.map
          (Array.map (fun x ->
               match x with
               | Plant plant -> Plant (Plant.feed plant name)
               | Empty -> Empty))
          cells
      in
      {
        cells = new_cells;
        money = m -. 0.1;
        plant_count = p;
        lucky = l;
        defense = d;
      }

(**idk money cost for water*)
let water_plants garden plant_name =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
      let new_cells =
        Array.map
          (Array.map (fun x ->
               match x with
               | Plant plant -> Plant (Plant.water plant plant_name)
               | Empty -> Empty))
          cells
      in
      {
        cells = new_cells;
        money = m -. 0.1;
        plant_count = p;
        lucky = l;
        defense = d;
      }

(**idk money cost for water*)
let neglect_plants garden plant_name =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
      let new_cells =
        Array.map
          (Array.map (fun x ->
               match x with
               | Plant plant -> Plant (Plant.neglect plant plant_name)
               | Empty -> Empty))
          cells
      in
      {
        cells = new_cells;
        money = m -. 0.1;
        plant_count = p;
        lucky = l;
        defense = d;
      }

(*adapted from ChatGPT*)
let count_plant plant_type garden =
  match garden with
  | { cells; money = _; plant_count = _; lucky = _; defense = _ } ->
      let count = ref 0 in
      Array.iter
        (fun row ->
          Array.iter
            (fun x ->
              match x with
              | Plant plant ->
                  if
                    Plant.get_type plant = plant_type
                    && Plant.get_height plant >= Plant.max_height plant_type
                  then (* let () = print_endline "hi" in *) count := !count + 1
                  else ()
              | _ -> ())
            row)
        cells;
      (* print_endline (string_of_int !count); *) !count

let remove_plant plant_type garden =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
      let new_cells =
        Array.map
          (Array.map (function
            | Plant plant
              when Plant.get_type plant = plant_type
                   && Plant.get_height plant >= Plant.max_height plant_type ->
                Empty
            | x -> x))
          cells
      in
      { cells = new_cells; money = m; plant_count = p; lucky = l; defense = d }

let get_flowers garden =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
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
      { cells = new_cells; money = m; plant_count = p; lucky = l; defense = d }

let get_fruits garden =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
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
      { cells = new_cells; money = m; plant_count = p; lucky = l; defense = d }

let get_vegetables garden =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
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
      { cells = new_cells; money = m; plant_count = p; lucky = l; defense = d }

let get_grains garden =
  match garden with
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
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
      { cells = new_cells; money = m; plant_count = p; lucky = l; defense = d }

let get_defensive_items = function
  | { cells; money = m; plant_count = p; lucky = l; defense = d } ->
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
      { cells = new_cells; money = m; plant_count = p; lucky = l; defense = d }

let print garden =
  show_money garden;
  for i = 0 to 4 do
    print_string "| ";
    for j = 0 to 9 do
      match garden.cells.(i).(j) with
      | Empty -> print_string "  "
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

let print_footer () =
  Printf.printf
    "+------------+---------+------------+--------+---------+-----------+\n"

let rec print_plants plants =
  match plants with
  | [] -> ()
  | row :: rest ->
      List.iter (fun plant -> print_plant plant) row;
      print_plants rest

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
  print_header ();
  print_plants filtered_plants;
  print_footer ()

let filter_dead garden =
  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant ->
          if Plant.is_alive plant then () else garden.cells.(i).(j) <- Empty
    done
  done;
  garden

let pollinate garden =
  let () =
    print_endline
      "\n\
      \ Some bees came through and\n\
      \   pollinated your plants, helping them grow. 🐝  🐝"
  in
  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant -> garden.cells.(i).(j) <- Plant (Plant.pollinate plant)
    done
  done;
  filter_dead garden

let nothing (garden : t) = garden

let rain garden =
  let () = print_endline "\n There was a rain storm last night!\n 🌧️🌧️🌧️" in
  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant -> garden.cells.(i).(j) <- Plant (Plant.rain plant)
    done
  done;
  filter_dead garden

let drought garden =
  let () =
    print_endline "\n☀️ There has been a drought!!\n   Plants have lost water. ☀️"
  in

  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant -> garden.cells.(i).(j) <- Plant (Plant.drought plant)
    done
  done;
  filter_dead garden

let stampede garden =
  let () =
    print_endline
      "\n\
      \ There has been a stampede\n\
      \   of buffallo! Some plants may have been  trampled. 🦬 🦬"
  in

  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant ->
          garden.cells.(i).(j) <-
            Plant
              (Plant.stampede
                 (determine_tragedy_threshold garden.defense)
                 plant)
    done
  done;
  filter_dead garden

let dragon garden =
  let () =
    print_endline
      "\n\
      \ An angry dragon has swept\n\
      \   through your garden! Some of your plants have  been torched. 🐉"
  in

  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant -> garden.cells.(i).(j) <- Plant (Plant.dragon plant)
    done
  done;
  filter_dead garden

let unicorn garden =
  let () =
    print_endline
      "\n\
      \ Your garden was visited by a\n\
      \   friendly unicorn! The unicorn added some \n\
      \  \n\
      \  magic to the soil to\n\
      \   help your plants grow! 🦄₊˚⊹"
  in

  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant -> garden.cells.(i).(j) <- Plant (Plant.unicorn plant)
    done
  done;
  filter_dead garden

let ice garden =
  let () =
    print_endline
      "\n\
      \ ☃️ Elsa stormed through your\n\
      \   garden, your plants have shriveled in the  cold ☃️"
  in

  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant -> garden.cells.(i).(j) <- Plant (Plant.ice plant)
    done
  done;
  filter_dead garden

let fairies garden =
  let () =
    print_endline
      "\n\
      \ .｡*ﾟ+.*.｡ Some fairies\n\
      \   floated through and scattered some pixie dust on  your plants, \n\
      \  \n\
      \ making them more valuable! ﾟ+..｡*ﾟ+ ."
  in
  for i = 0 to Array.length garden.cells - 1 do
    for j = 0 to Array.length garden.cells.(0) - 1 do
      match garden.cells.(i).(j) with
      | Empty -> ()
      | Plant plant -> garden.cells.(i).(j) <- Plant (Plant.fairies plant)
    done
  done;
  filter_dead garden

let apply_event event_name garden =
  match event_name with
  | "stampede" -> stampede garden
  | "pollinate" -> pollinate garden
  | "unicorn" -> unicorn garden
  | "drought" -> drought garden
  | "ice" -> ice garden
  | "dragon" -> dragon garden
  | "fairies" -> fairies garden
  | "rain" -> rain garden
  | _ -> nothing garden

let night_change n garden =
  if !n = 0 then apply_event "nothing" garden
  else
    let () = print_endline "⋆⁺₊⋆ ☾⋆⁺₊⋆ Overnight ⋆⁺₊⋆ ☾⋆⁺₊⋆ " in
    let rand_val = Random.float 1.0 in
    if rand_val < 0.6 then apply_event "nothing" garden
    else
      let bad_events_lst = [ "stampede"; "drought"; "ice"; "dragon"; "rain" ] in
      let good_events_lst = [ "pollinate"; "unicorn"; "fairies" ] in
      let event_type = Random.float 1.0 in
      if event_type < determine_threshold (get_luck garden) then
        let rand_int = Random.int (List.length bad_events_lst) in
        let chosen_event = List.nth bad_events_lst rand_int in
        apply_event chosen_event garden
      else
        let rand_int = Random.int (List.length good_events_lst) in
        let chosen_event = List.nth good_events_lst rand_int in
        apply_event chosen_event garden

let has_plant garden name =
  match garden with
  | { cells; _ } ->
      Array.exists
        (fun row ->
          Array.exists
            (fun cell ->
              match cell with
              | Plant plant -> Plant.get_name plant = name
              | _ -> false)
            row)
        cells

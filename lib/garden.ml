type cell =
  | Empty
  | Plant of Plant.t

type t = {
  cells : cell array array;
  money : float;
}

let create_garden () = { cells = Array.make_matrix 5 10 Empty; money = 50.00 }

let inc_money plant_type garden =
  match garden with
  | { cells; money = m } -> (
      match plant_type with
      | "Flower" -> { cells; money = m +. 2.00 }
      | "Peach" -> { cells; money = m +. 7.00 }
      | "Strawberry" -> { cells; money = m +. 5.00 }
      | "Cactus" -> { cells; money = m +. 3.00 }
      | _ -> { cells; money = m +. 0.00 })

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

let feed_plants garden =
  match garden with
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun x ->
               match x with
               | Plant plant -> Plant (Plant.feed plant)
               | Empty -> Empty))
          cells
      in
      { cells = new_cells; money = m -. 10.0 }

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

let get_flowers garden =
  match garden with
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

let get_fruits garden =
  match garden with
  | { cells; money = m } ->
      let new_cells =
        Array.map
          (Array.map (fun cell ->
               match cell with
               | Plant plant ->
                   let plant_type = Plant.get_type plant in
                   if
                     plant_type = "Peach" || plant_type = "Strawberry"
                     || plant_type = "Apple"
                   then cell
                   else Empty
               | _ -> Empty))
          cells
      in
      { cells = new_cells; money = m }

let print garden =
  for i = 0 to 4 do
    print_string "| ";
    for j = 0 to 9 do
      match garden.cells.(i).(j) with
      | Empty -> print_string "  "
      | Plant plant -> print_string (Plant.print_plant plant)
    done;
    print_endline "|"
  done

type t =
  | Garden of {
      plants : Plant.t list;
      money : float;
    }

let create_garden = Garden { plants = []; money = 50.00 }

let inc_money plant_type garden =
  match garden with
  | Garden { plants = p; money = m } -> (
      match plant_type with
      | "Flower" -> Garden { plants = p; money = m +. 2.00 }
      | "Peach" -> Garden { plants = p; money = m +. 7.00 }
      | "Strawberry" -> Garden { plants = p; money = m +. 5.00 }
      | "Cactus" -> Garden { plants = p; money = m +. 3.00 }
      | _ -> Garden { plants = p; money = m +. 0.00 })

let add_plant plant_name garden =
  match garden with
  | Garden { plants = lst; money = m } ->
      Garden { plants = Plant.create_plant plant_name :: lst; money = m }

let feed_plants garden =
  match garden with
  | Garden { plants = lst; money = m } ->
      if lst = [] then
        let () = print_endline "~No plants to feed~ " in
        Garden { plants = List.map Plant.feed lst; money = m }
      else Garden { plants = List.map Plant.feed lst; money = m -. 10.0 }

(*adapted from ChatGPT*)
let count_plant plant_type garden =
  match garden with
  | Garden { plants = lst; money = _ } ->
      List.fold_left
        (fun count elem ->
          if Plant.get_type elem = plant_type && Plant.get_height elem > 5 then
            count + 1
          else count)
        0 lst

let remove_plant plant_type garden =
  match garden with
  | Garden { plants = lst; money = m } ->
      Garden
        {
          plants =
            List.filter
              (fun x ->
                Plant.get_type x <> plant_type && Plant.get_height x < 5)
              lst;
          money = m;
        }

let printable garden =
  match garden with
  | Garden { plants = lst; money = _ } ->
      List.fold_left
        (fun x y -> x ^ " " ^ y)
        ""
        (List.map (fun plant -> Plant.print_plant plant) lst)

let print garden = print_string (printable garden)

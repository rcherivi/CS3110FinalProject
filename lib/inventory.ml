type t = Inv of (string * int) list

let insert key value (assoc_list : t) =
  match assoc_list with
  | Inv alist -> Inv ((key, value) :: alist)

let rec lookup key assoc_list =
  match assoc_list with
  | Inv [] -> failwith "Not found"
  | Inv ((k, v) :: lst) -> if key = k then v else lookup key (Inv lst)

let rec lookup_option key assoc_list =
  match assoc_list with
  | Inv [] -> None
  | Inv ((k, v) :: lst) ->
      if key = k then Some v else lookup_option key (Inv lst)

let create_inventory = Inv []

let harvest plant_type inv garden =
  match inv with
  | Inv lst ->
      if List.mem_assoc plant_type lst then
        ( Inv
            (( plant_type,
               lookup plant_type (Inv lst)
               + Garden.count_plant plant_type garden )
            :: List.remove_assoc plant_type lst),
          Garden.remove_plant plant_type garden )
      else
        ( insert plant_type (Garden.count_plant plant_type garden) inv,
          Garden.remove_plant plant_type garden )

(* else ( insert plant_type (Garden.count_plant plant_type garden) inv,
   Garden.remove_plant plant_type garden ) *)

(* let print inv = *)

(* string -> t -> t *)

(*control how much you sell*)
(*sell more than what you have *)
let sell plant_type quantity inv garden =
  match inv with
  | Inv lst ->
      if List.mem_assoc plant_type lst then
        let current_quantity = lookup plant_type inv in
        if current_quantity >= quantity then
          let new_quantity = current_quantity - quantity in
          let updated_inventory =
            Inv ((plant_type, new_quantity) :: List.remove_assoc plant_type lst)
          in
          let updated_garden = Garden.inc_money plant_type garden in
          (updated_inventory, updated_garden)
        else
          let () = print_endline "Not enough items in inventory" in
          (inv, garden)
      else
        let () = print_endline "This plant is not in inventory" in
        (inv, garden)

let print inv =
  match inv with
  | Inv lst ->
      List.iter (fun (x, y) -> print_endline (x ^ ": " ^ string_of_int y)) lst

(* let sell inv = *)
let add item inv =
  match inv with
  | Inv lst ->
      if List.mem_assoc item lst then
        Inv ((item, lookup item (Inv lst) + 1) :: List.remove_assoc item lst)
      else insert item 1 inv

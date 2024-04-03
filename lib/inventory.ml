type t = Inv of (string * int) list

let insert key value (assoc_list : t) =
  match assoc_list with
  | Inv alist -> Inv ((key, value) :: alist)

let rec lookup key assoc_list =
  match assoc_list with
  | Inv [] -> failwith "Not found"
  | Inv ((k, v) :: lst) -> if key = k then v else lookup key (Inv lst)

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

(* let print inv = *)

(* string -> t -> t *)

(*control how much you sell*)
(*sell more than what you have *)
let sell plant_type inv garden =
  match inv with
  | Inv lst ->
      if List.mem_assoc plant_type lst then
        ( Inv ((plant_type, 0) :: List.remove_assoc plant_type lst),
          Garden.inc_money plant_type garden )
      else
        let () = print_endline "This plant is not in inventory" in
        (inv, garden)

let print inv =
  match inv with
  | Inv lst ->
      List.iter (fun (x, y) -> print_string (x ^ ": " ^ string_of_int y)) lst
(* let sell inv = *)

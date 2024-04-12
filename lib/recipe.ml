type t = (string * int) list

let rec get_missing_ingredients missing (recipe : t) (inventory : Inventory.t) =
  match recipe with
  | [] -> missing
  | (ingredient, recipe_qty) :: tl ->
      let m =
        match Inventory.lookup_option ingredient inventory with
        | Some inv_qty when inv_qty >= recipe_qty -> missing
        | _ -> (ingredient, recipe_qty) :: missing
      in
      get_missing_ingredients m tl inventory

let have_ingredients (inventory : Inventory.t) (recipe : t) =
  let missing_ingredients = get_missing_ingredients [] recipe inventory in
  if missing_ingredients = [] then
    print_endline "We have all the ingredients necessary for the recipe."
  else
    print_endline
      ("Do not have enough ingredients. Missing ingredients: "
      ^ String.concat ", "
          (List.map
             (fun (ingredient, qty) -> ingredient ^ ": " ^ string_of_int qty)
             missing_ingredients))

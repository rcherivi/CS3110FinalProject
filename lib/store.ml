(* let print_store = "Flower: ($2.0)\n\ Peach: ($3.0)\n\ Strawberry: ($5.0)\n\
   Cactus: ($4.0)\n\ Cheese: ($5.0)\n\ Eggs: ($3.0)\n\ Milk: ($5.0)" *)
let () = Random.self_init ()

type t = {
  daisy_price : float;
  strawberry_price : float;
  sunflower_price : float;
  rose_price : float;
  tulip_price : float;
  tomato_price : float;
  carrot_price : float;
  potato_price : float;
  onion_price : float;
  wheat_price : float;
  apple_price : float;
  corn_price : float;
  peach_price : float;
  orange_price : float;
  cactus_price : float;
  cheese_price : float;
  eggs_price : float;
  milk_price : float;
}

type prices = {
  cheese_price : float;
  eggs_price : float;
  milk_price : float;
}

let create_store =
  let rand_val = Random.float 1.0 in
  let usual = { cheese_price = 5.0; eggs_price = 3.0; milk_price = 5.0 } in
  {
    daisy_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Daisy" ""));
    strawberry_price =
      Plant.get_price
        (Plant.apply_discount (Plant.create_plant "Strawberry" ""));
    sunflower_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Sunflower" ""));
    rose_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Rose" ""));
    tulip_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Tulip" ""));
    tomato_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Tomato" ""));
    orange_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Orange" ""));
    carrot_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Carrot" ""));
    onion_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Onion" ""));
    potato_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Potato" ""));
    wheat_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Wheat" ""));
    apple_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Apple" ""));
    corn_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Corn" ""));
    peach_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Peach" ""));
    cactus_price =
      Plant.get_price (Plant.apply_discount (Plant.create_plant "Cactus" ""));
    cheese_price =
      (if rand_val < 0.5 then usual.cheese_price else usual.cheese_price *. 0.5);
    eggs_price =
      (if rand_val < 0.5 then usual.eggs_price else usual.eggs_price *. 0.5);
    milk_price =
      (if rand_val < 0.5 then usual.milk_price else usual.milk_price *. 0.5);
  }

let discount_plant price plant_name =
  if price < Plant.get_price (Plant.create_plant plant_name "") then
    "[DISCOUNT!!]"
  else ""

let discount_general price name =
  let usual = { cheese_price = 5.0; eggs_price = 3.0; milk_price = 5.0 } in
  match name with
  | "Cheese" -> if price < usual.cheese_price then "[DISCOUNT!!]" else ""
  | "Eggs" -> if price < usual.eggs_price then "[DISCOUNT!!]" else ""
  | "Milk" -> if price < usual.milk_price then "[DISCOUNT!!]" else ""
  | _ -> ""

let print_store store =
  "\n" ^ "Daisy: ($"
  ^ string_of_float store.daisy_price
  ^ ") "
  ^ discount_plant store.daisy_price "Daisy"
  ^ "\n" ^ "Sunflower: ($"
  ^ string_of_float store.sunflower_price
  ^ ") "
  ^ discount_plant store.sunflower_price "Sunflower"
  ^ "\n" ^ "Rose: ($"
  ^ string_of_float store.rose_price
  ^ ") "
  ^ discount_plant store.rose_price "Rose"
  ^ "\n" ^ "Tulip: ($"
  ^ string_of_float store.tulip_price
  ^ ") "
  ^ discount_plant store.tulip_price "Tulip"
  ^ "\n" ^ "Tomato: ($"
  ^ string_of_float store.tomato_price
  ^ ") "
  ^ discount_plant store.tomato_price "Tomato"
  ^ "\n" ^ "Corn: ($"
  ^ string_of_float store.corn_price
  ^ ") "
  ^ discount_plant store.corn_price "Corn"
  ^ "\n" ^ "Carrot: ($"
  ^ string_of_float store.carrot_price
  ^ ") "
  ^ discount_plant store.carrot_price "Carrot"
  ^ "\n" ^ "Onion: ($"
  ^ string_of_float store.onion_price
  ^ ") "
  ^ discount_plant store.onion_price "Onion"
  ^ "\n" ^ "Potato: ($"
  ^ string_of_float store.potato_price
  ^ ") "
  ^ discount_plant store.potato_price "Potato"
  ^ "\n" ^ "Wheat: ($"
  ^ string_of_float store.wheat_price
  ^ ") "
  ^ discount_plant store.wheat_price "Wheat"
  ^ "\n" ^ "Apple: ($"
  ^ string_of_float store.apple_price
  ^ ") "
  ^ discount_plant store.apple_price "Apple"
  ^ "\n" ^ "Peach: ($"
  ^ string_of_float store.peach_price
  ^ ") "
  ^ discount_plant store.peach_price "Peach"
  ^ "\n" ^ "Orange: ($"
  ^ string_of_float store.orange_price
  ^ ") "
  ^ discount_plant store.orange_price "Orange"
  ^ "\n" ^ "Strawberry: ($"
  ^ string_of_float store.strawberry_price
  ^ ") "
  ^ discount_plant store.strawberry_price "Strawberry"
  ^ "\n" ^ "Cactus: ($"
  ^ string_of_float store.cactus_price
  ^ ") "
  ^ discount_plant store.cactus_price "Cactus"
  ^ "\n" ^ "Cheese: ($"
  ^ string_of_float store.cheese_price
  ^ ") "
  ^ discount_general store.cheese_price "Cheese"
  ^ "\n" ^ "Eggs: ($"
  ^ string_of_float store.eggs_price
  ^ ") "
  ^ discount_general store.eggs_price "Eggs"
  ^ "\n" ^ "Cheese: ($"
  ^ string_of_float store.cheese_price
  ^ ") "
  ^ discount_general store.milk_price "Milk"

(* add seedling -> goes into garden *)
(* add other item -> goes into inventory *)

(*discounts / item of the day *)
(* let buy_item name = Inventory. *)

let buy_item item_name store inv garden =
  match item_name with
  | "Daisy" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.daisy_price) garden) )
  | "Sunflower" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.sunflower_price) garden) )
  | "Rose" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.rose_price) garden) )
  | "Tulip" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.tulip_price) garden) )
  | "Tomato" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.tomato_price) garden) )
  | "Corn" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.corn_price) garden) )
  | "Carrot" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.carrot_price) garden) )
  | "Onion" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.onion_price) garden) )
  | "Potato" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.potato_price) garden) )
  | "Wheat" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.wheat_price) garden) )
  | "Apple" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.apple_price) garden) )
  | "Orange" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.orange_price) garden) )
  | "Peach" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.peach_price) garden) )
  | "Strawberry" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.strawberry_price) garden) )
  | "Cactus" ->
      let () = print_endline "Name of Plant? (i.e. Benjamin)" in
      let name = read_line () in
      ( inv,
        Garden.add_plant item_name name
          (Garden.inc_money_amt (-1.0 *. store.cactus_price) garden) )
  | "Eggs" ->
      ( Inventory.add item_name inv,
        Garden.inc_money_amt (-1.0 *. store.eggs_price) garden )
  | "Milk" ->
      ( Inventory.add item_name inv,
        Garden.inc_money_amt (-1.0 *. store.milk_price) garden )
  | "Cheese" ->
      ( Inventory.add item_name inv,
        Garden.inc_money_amt (-1.0 *. store.cheese_price) garden )
  | _ -> (Inventory.add item_name inv, garden)

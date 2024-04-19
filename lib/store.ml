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
  lemon_price : float;
  potato_price : float;
  onion_price : float;
  wheat_price : float;
  apple_price : float;
  corn_price : float;
  peach_price : float;
  pineapple_price : float;
  cactus_price : float;
  cheese_price : float;
  eggs_price : float;
  milk_price : float;
  water_price : float;
  clover_price : float;
  rice_price : float;
  lettuce_price : float;
  mango_price : float;
  bell_pepper_price : float;
  butter_price : float;
  sugar_price : float;
  chocolate_price : float;
  plant_food_price : float;
  ladybug_price : float;
  beef_price : float;
  chicken_price : float;
}

type prices = {
  cheese_price : float;
  eggs_price : float;
  milk_price : float;
  water_price : float;
  butter_price : float;
  sugar_price : float;
  chocolate_price : float;
  plant_food_price : float;
  ladybug_price : float;
  beef_price : float;
  chicken_price : float;
}

let apply_discount price =
  let rand_val = Random.float 1.0 in
  if rand_val < 0.5 then price else price *. 0.5

let usual =
  {
    cheese_price = 5.0;
    eggs_price = 3.0;
    milk_price = 5.0;
    water_price = 1.0;
    butter_price = 2.0;
    sugar_price = 1.5;
    chocolate_price = 3.20;
    plant_food_price = 2.0;
    ladybug_price = 8.0;
    beef_price = 6.0;
    chicken_price = 5.50;
  }

let create_store =
  let create_plant_and_get_price name =
    Plant.get_price (Plant.apply_discount (Plant.create_plant name ""))
  in
  {
    daisy_price = create_plant_and_get_price "Daisy";
    strawberry_price = create_plant_and_get_price "Strawberry";
    sunflower_price = create_plant_and_get_price "Sunflower";
    rose_price = create_plant_and_get_price "Rose";
    tulip_price = create_plant_and_get_price "Tulip";
    tomato_price = create_plant_and_get_price "Tomato";
    lemon_price = create_plant_and_get_price "Lemon";
    pineapple_price = create_plant_and_get_price "Pineapple";
    onion_price = create_plant_and_get_price "Onion";
    potato_price = create_plant_and_get_price "Potato";
    wheat_price = create_plant_and_get_price "Wheat";
    apple_price = create_plant_and_get_price "Apple";
    corn_price = create_plant_and_get_price "Corn";
    peach_price = create_plant_and_get_price "Peach";
    cactus_price = create_plant_and_get_price "Cactus";
    clover_price = create_plant_and_get_price "Clover";
    rice_price = create_plant_and_get_price "Rice";
    lettuce_price = create_plant_and_get_price "Lettuce";
    mango_price = create_plant_and_get_price "Mango";
    cheese_price = apply_discount usual.cheese_price;
    eggs_price = apply_discount usual.eggs_price;
    milk_price = apply_discount usual.milk_price;
    water_price = apply_discount usual.water_price;
    butter_price = apply_discount usual.butter_price;
    sugar_price = apply_discount usual.sugar_price;
    chocolate_price = apply_discount usual.chocolate_price;
    plant_food_price = apply_discount usual.plant_food_price;
    ladybug_price = apply_discount usual.ladybug_price;
    beef_price = apply_discount usual.beef_price;
    chicken_price = apply_discount usual.chicken_price;
    bell_pepper_price = create_plant_and_get_price "Bell Pepper";
  }

let discount_plant price plant_name =
  if price < Plant.get_price (Plant.create_plant plant_name "") then
    "[DISCOUNT!!]"
  else ""

let discount_general price name =
  (* let usual = { cheese_price = 5.0; eggs_price = 3.0; milk_price = 5.0;
     water_price = 1.0; butter_price = 2.0; sugar_price = 1.5; chocolate_price =
     3.20; plant_food_price = 2.0; ladybug_price = 8.0; beef_price = 6.0;
     chicken_price = 5.50; } in *)
  match name with
  | "Cheese" -> if price < usual.cheese_price then "[DISCOUNT] 💸" else ""
  | "Eggs" -> if price < usual.eggs_price then "[DISCOUNT] 💸" else ""
  | "Milk" -> if price < usual.milk_price then "[DISCOUNT] 💸" else ""
  | "Water" -> if price < usual.water_price then "[DISCOUNT] 💸" else ""
  | "Butter" -> if price < usual.butter_price then "[[DISCOUNT] 💸" else ""
  | "Sugar" -> if price < usual.sugar_price then "[DISCOUNT] 💸" else ""
  | "Chocolate" -> if price < usual.chocolate_price then "[DISCOUNT] 💸]" else ""
  | "Plant Food" ->
      if price < usual.plant_food_price then "[DISCOUNT] 💸" else ""
  | "Lady Bug" -> if price < usual.ladybug_price then "[DISCOUNT] 💸" else ""
  | "Beef" -> if price < usual.beef_price then "[DISCOUNT] 💸" else ""
  | "Chicken" -> if price < usual.chicken_price then "[DISCOUNT] 💸" else ""
  | _ -> ""

let general_discount name price =
  "\n" ^ name ^ ": ($" ^ string_of_float price ^ ") "
  ^ discount_general price name

let plant_and_price name price =
  "\n" ^ name ^ ": ($" ^ string_of_float price ^ ") "
  ^ discount_plant price name

let print_store store =
  plant_and_price "Daisy" store.daisy_price
  ^ plant_and_price "Strawberry" store.strawberry_price
  ^ plant_and_price "Sunflower" store.sunflower_price
  ^ plant_and_price "Rose" store.rose_price
  ^ plant_and_price "Tulip" store.tulip_price
  ^ plant_and_price "Tomato" store.tomato_price
  ^ plant_and_price "Lemon" store.lemon_price
  ^ plant_and_price "Potato" store.potato_price
  ^ plant_and_price "Onion" store.onion_price
  ^ plant_and_price "Wheat" store.wheat_price
  ^ plant_and_price "Apple" store.apple_price
  ^ plant_and_price "Corn" store.corn_price
  ^ plant_and_price "Peach" store.peach_price
  ^ plant_and_price "Pineapple" store.pineapple_price
  ^ plant_and_price "Cactus" store.cactus_price
  ^ general_discount "Cheese" store.cheese_price
  ^ general_discount "Eggs" store.eggs_price
  ^ general_discount "Milk" store.milk_price
  ^ general_discount "Water" store.water_price
  ^ general_discount "Clover" store.clover_price
  ^ general_discount "Rice" store.rice_price
  ^ general_discount "Lettuce" store.lettuce_price
  ^ general_discount "Mango" store.mango_price
  ^ general_discount "Butter" store.butter_price
  ^ general_discount "Sugar" store.sugar_price
  ^ general_discount "Chocolate" store.chocolate_price
  ^ general_discount "Plant Food" store.plant_food_price
  ^ general_discount "Ladybug" store.ladybug_price
  ^ general_discount "Beef" store.beef_price
  ^ general_discount "Chicken" store.chicken_price
  ^ general_discount "Bell Pepper" store.bell_pepper_price

(* add seedling -> goes into garden *)
(* add other item -> goes into inventory *)

(*discounts / item of the day *)
(* let buy_item name = Inventory. *)

let buy_item item_name store inv garden =
  let price =
    match item_name with
    | "Daisy" -> store.daisy_price
    | "Strawberry" -> store.strawberry_price
    | "Sunflower" -> store.sunflower_price
    | "Rose" -> store.rose_price
    | "Tulip" -> store.tulip_price
    | "Tomato" -> store.tomato_price
    | "Lemon" -> store.lemon_price
    | "Pineapple" -> store.pineapple_price
    | "Onion" -> store.onion_price
    | "Potato" -> store.potato_price
    | "Wheat" -> store.wheat_price
    | "Apple" -> store.apple_price
    | "Corn" -> store.corn_price
    | "Peach" -> store.peach_price
    | "Cactus" -> store.cactus_price
    | "Clover" -> store.clover_price
    | "Rice" -> store.rice_price
    | "Lettuce" -> store.lettuce_price
    | "Mango" -> store.mango_price
    | "Cheese" -> store.cheese_price
    | "Eggs" -> store.eggs_price
    | "Milk" -> store.milk_price
    | "Water" -> store.water_price
    | "Butter" -> store.butter_price
    | "Chicken" -> store.chicken_price
    | "Sugar" -> store.sugar_price
    | "Chocolate" -> store.chocolate_price
    | "Plant Food" -> store.plant_food_price
    | "Lady Bug" -> store.ladybug_price
    | "Beef" -> store.beef_price
    | "Bell Pepper" -> store.bell_pepper_price
    | _ -> 0.0 (* Default price for unknown items *)
  in
  let new_inv, new_money =
    match item_name with
    | "Milk"
    | "Eggs"
    | "Water"
    | "Cheese"
    | "Butter"
    | "Sugar"
    | "Chocolate"
    | "Plant Food"
    | "Lady Bug"
    | "Beef" -> (Inventory.add item_name inv, garden)
    | _ ->
        let () = print_endline "Name of Plant? (i.e. Benjamin)" in
        let name = read_line () in
        let new_inv = Inventory.add item_name inv in
        let new_money = Garden.inc_money_amt (-1.0 *. price) garden in
        (new_inv, Garden.add_plant item_name name new_money)
  in
  (new_inv, new_money)

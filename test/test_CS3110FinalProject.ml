open OUnit2
open CS3110FinalProject
(* open Plant open Garden open Store open Inventory open Recipe *)

let daisy = Plant.create_plant "Daisy" "Sam"
let sunflower = Plant.create_plant "Sunflower" "Emily"
let rose = Plant.create_plant "Rose" "Julie"
let tulip = Plant.create_plant "Tulip" "Michael"
let tomato = Plant.create_plant "Tomato" "Sophia"
let potato = Plant.create_plant "Potato" "Daniel"
let onion = Plant.create_plant "Onion" "Olivia"
let wheat = Plant.create_plant "Wheat" "William"
let apple = Plant.create_plant "Apple" "Isabella"
let peach = Plant.create_plant "Peach" "Alexander"
let strawberry = Plant.create_plant "Strawberry" "Mia"
let mango = Plant.create_plant "Mango" "James"
let cactus = Plant.create_plant "Cactus" "Charlotte"
let lemon = Plant.create_plant "Lemon" "Benjamin"
let pineapple = Plant.create_plant "Pineapple" "Amelia"
let clover = Plant.create_plant "Clover" "Ethan"
let rice = Plant.create_plant "Rice" "Abigail"
let lettuce = Plant.create_plant "Lettuce" "Henry"
let bell_pepper = Plant.create_plant "Bell Pepper" "Ella"

let corn =
  Plant.create_plant "Corn" "Matthew" (* let garden = Garden.create_garden () *)

let test_get_name _ =
  assert_equal "Sam" (Plant.get_name daisy);
  assert_equal "Emily" (Plant.get_name sunflower);
  assert_equal "Julie" (Plant.get_name rose);
  assert_equal "Michael" (Plant.get_name tulip);
  assert_equal "Sophia" (Plant.get_name tomato);
  assert_equal "Daniel" (Plant.get_name potato);
  assert_equal "Olivia" (Plant.get_name onion);
  assert_equal "William" (Plant.get_name wheat);
  assert_equal "Isabella" (Plant.get_name apple);
  assert_equal "Alexander" (Plant.get_name peach);
  assert_equal "Mia" (Plant.get_name strawberry);
  assert_equal "James" (Plant.get_name mango);
  assert_equal "Charlotte" (Plant.get_name cactus);
  assert_equal "Benjamin" (Plant.get_name lemon);
  assert_equal "Amelia" (Plant.get_name pineapple);
  assert_equal "Ethan" (Plant.get_name clover);
  assert_equal "Abigail" (Plant.get_name rice);
  assert_equal "Henry" (Plant.get_name lettuce);
  assert_equal "Ella" (Plant.get_name bell_pepper);
  assert_equal "Matthew" (Plant.get_name corn)

let test_get_height _ =
  assert_equal 0 (Plant.get_height daisy);
  assert_equal 0 (Plant.get_height rose)

let test_get_height_and_feed _ =
  assert_equal 0 (Plant.get_height daisy);
  assert_equal 0 (Plant.get_height rose);

  (* Feed the plants *)
  let daisy_after_feed = Plant.feed daisy "Sam" in
  let rose_after_feed = Plant.feed rose "Julie" in

  (* Verify the height after feeding *)
  assert_equal 1 (Plant.get_height daisy_after_feed);
  assert_equal 1 (Plant.get_height rose_after_feed)

let test_get_life _ =
  assert_equal true (Plant.get_life daisy);
  assert_equal true (Plant.get_life rose)

let test_get_hydration _ =
  assert_equal 25 (Plant.get_hydration daisy);
  assert_equal 25 (Plant.get_hydration rose)

let test_get_price _ =
  assert_equal 3.0 (Plant.get_price daisy);
  assert_equal 8.0 (Plant.get_price rose)

let test_get_category_flowers _ =
  assert_equal "Flowers" (Plant.get_category daisy);
  assert_equal "Flowers" (Plant.get_category sunflower);
  assert_equal "Flowers" (Plant.get_category rose);
  assert_equal "Flowers" (Plant.get_category tulip)

let test_get_category_vegetables _ =
  assert_equal "Vegetables" (Plant.get_category tomato);
  assert_equal "Vegetables" (Plant.get_category potato);
  assert_equal "Vegetables" (Plant.get_category onion);
  assert_equal "Vegetables" (Plant.get_category lettuce);
  assert_equal "Vegetables" (Plant.get_category bell_pepper)

let test_get_category_fruits _ =
  assert_equal "Fruits" (Plant.get_category apple);
  assert_equal "Fruits" (Plant.get_category peach);
  assert_equal "Fruits" (Plant.get_category strawberry);
  assert_equal "Fruits" (Plant.get_category mango);
  assert_equal "Fruits" (Plant.get_category lemon);
  assert_equal "Fruits" (Plant.get_category pineapple)

let test_get_category_defensive_items _ =
  assert_equal "Defensive Items" (Plant.get_category cactus);
  assert_equal "Defensive Items" (Plant.get_category clover)

let test_get_category_grains _ =
  assert_equal "Grains" (Plant.get_category rice);
  assert_equal "Grains" (Plant.get_category corn);
  assert_equal "Grains" (Plant.get_category wheat)

let test_get_type _ =
  assert_equal "Daisy" (Plant.get_type daisy);
  assert_equal "Sunflower" (Plant.get_type sunflower);
  assert_equal "Rose" (Plant.get_type rose);
  assert_equal "Tulip" (Plant.get_type tulip);
  assert_equal "Tomato" (Plant.get_type tomato);
  assert_equal "Potato" (Plant.get_type potato);
  assert_equal "Onion" (Plant.get_type onion);
  assert_equal "Wheat" (Plant.get_type wheat);
  assert_equal "Apple" (Plant.get_type apple);
  assert_equal "Peach" (Plant.get_type peach);
  assert_equal "Strawberry" (Plant.get_type strawberry);
  assert_equal "Mango" (Plant.get_type mango);
  assert_equal "Cactus" (Plant.get_type cactus);
  assert_equal "Lemon" (Plant.get_type lemon);
  assert_equal "Pineapple" (Plant.get_type pineapple);
  assert_equal "Clover" (Plant.get_type clover);
  assert_equal "Rice" (Plant.get_type rice);
  assert_equal "Lettuce" (Plant.get_type lettuce);
  assert_equal "Bell Pepper" (Plant.get_type bell_pepper);
  assert_equal "Corn" (Plant.get_type corn)

let test_water _ =
  let daisy_watered = Plant.water daisy "Sam" in
  assert_equal 26 (Plant.get_hydration daisy_watered);
  let rose_watered = Plant.water rose "Julie" in
  assert_equal 26 (Plant.get_hydration rose_watered);
  let corn_watered = Plant.water corn "Matthew" in
  assert_equal 26 (Plant.get_hydration corn_watered)

let test_apply_discount _ =
  let discounted_daisy = Plant.apply_discount daisy in
  let discounted_rose = Plant.apply_discount rose in
  (* Check if the price has been discounted *)
  assert_bool
    "Discounted price should be less than or\n   equal to original price"
    (Plant.get_price discounted_daisy <= Plant.get_price daisy);
  assert_bool
    "Discounted price should be less than or equal to\n   original price"
    (Plant.get_price discounted_rose <= Plant.get_price rose)

(* let test_neglect _ = let neglected_daisy = Plant.neglect daisy "Daisy" in let
   neglected_rose = Plant.neglect rose "Rose" in (* Check if hydration has been
   reduced by 1 *) assert_equal (Plant.get_hydration daisy - 1)
   (Plant.get_hydration neglected_daisy); assert_equal (Plant.get_hydration rose
   - 1) (Plant.get_hydration neglected_rose) *)

let suite =
  "Test Suite for Plant Module"
  >::: [
         "test_get_name" >:: test_get_name;
         "test_get_height" >:: test_get_height;
         "test_get_height_and_feed" >:: test_get_height_and_feed;
         "test_get_life" >:: test_get_life;
         "test_get_hydration" >:: test_get_hydration;
         "test_get_price" >:: test_get_price;
         "test_get_category_flowers" >:: test_get_category_flowers;
         "test_get_category_vegetables" >:: test_get_category_vegetables;
         "test_get_category_fruits" >:: test_get_category_fruits;
         "test_get_category_defensive_items"
         >:: test_get_category_defensive_items;
         "test_get_category_grains" >:: test_get_category_grains;
         "test_get_type" >:: test_get_type;
         "test_water" >:: test_water;
         "test_apply_discount" >:: test_apply_discount;
         (* "test_neglect" >:: test_neglect; *)
       ]

let () = run_test_tt_main suite

open OUnit2
open CS3110FinalProject
(* open Plant open Garden open Store open Inventory open Recipe *)

let new_inv = Inventory.create_inventory
let garden = Garden.create_garden ()
let add_eggs = Inventory.add "Eggs" new_inv
let add_milk = Inventory.add "Milk" new_inv
let add_cheese = Inventory.add "Cheese" new_inv
let add_daisy = Inventory.harvest "Daisy" new_inv garden
let add_apple = Inventory.harvest "Apple" new_inv garden
let add_corn = Inventory.harvest "Corn" new_inv garden
let add_cactus = Inventory.harvest "Cactus" new_inv garden

(**)

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
  assert_equal true (Plant.get_life sunflower);
  assert_equal true (Plant.get_life rose);
  assert_equal true (Plant.get_life tulip);
  assert_equal true (Plant.get_life tomato);
  assert_equal true (Plant.get_life potato);
  assert_equal true (Plant.get_life onion);
  assert_equal true (Plant.get_life wheat);
  assert_equal true (Plant.get_life apple);
  assert_equal true (Plant.get_life peach);
  assert_equal true (Plant.get_life strawberry);
  assert_equal true (Plant.get_life mango);
  assert_equal true (Plant.get_life cactus);
  assert_equal true (Plant.get_life lemon);
  assert_equal true (Plant.get_life pineapple);
  assert_equal true (Plant.get_life clover);
  assert_equal true (Plant.get_life rice);
  assert_equal true (Plant.get_life lettuce);
  assert_equal true (Plant.get_life bell_pepper);
  assert_equal true (Plant.get_life corn)

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

(* Test feed function *)
let test_feed_daisy _ =
  let feed_plant = Plant.feed daisy "Sam" in
  assert_equal 1 (Plant.get_height feed_plant);
  assert_equal 3.0 (Plant.get_price feed_plant)

let test_feed_sunflower _ =
  let feed1 = Plant.feed sunflower "Emily" in
  let feed2 = Plant.feed feed1 "Emily" in
  assert_equal 2 (Plant.get_height feed2);
  assert_equal 2.0 (Plant.get_price feed2)

let test_feed_rose _ =
  let feed1 = Plant.feed rose "Julie" in
  let feed2 = Plant.feed feed1 "Julie" in
  let feed3 = Plant.feed feed2 "Julie" in
  assert_equal 3 (Plant.get_height feed3);
  assert_equal 8.0 (Plant.get_price feed3)

let test_feed_tulip _ =
  let feed1 = Plant.feed tulip "Michael" in
  let feed2 = Plant.feed feed1 "Michael" in
  assert_equal 2 (Plant.get_height feed2);
  assert_equal 5.0 (Plant.get_price feed2)

let test_feed_tomato _ =
  let feed1 = Plant.feed tomato "Sophia" in
  let feed2 = Plant.feed feed1 "Sophia" in
  let feed3 = Plant.feed feed2 "Sophia" in
  let feed4 = Plant.feed feed3 "Sophia" in
  assert_equal 4 (Plant.get_height feed4);
  assert_equal 5.0 (Plant.get_price feed4)

let test_feed_apple _ =
  let feed_plant = Plant.feed apple "Isabella" in
  assert_equal 1 (Plant.get_height feed_plant);
  assert_equal 7.0 (Plant.get_price feed_plant)

let test_feed_onion _ =
  let feed1 = Plant.feed onion "Olivia" in
  let feed2 = Plant.feed feed1 "Olivia" in
  assert_equal 2 (Plant.get_height feed2);
  assert_equal 5.0 (Plant.get_price feed2)

let test_feed_potato _ =
  let feed1 = Plant.feed potato "Daniel" in
  let feed2 = Plant.feed feed1 "Daniel" in
  let feed3 = Plant.feed feed2 "Daniel" in
  let feed4 = Plant.feed feed3 "Daniel" in
  assert_equal 4 (Plant.get_height feed4);
  assert_equal 7.0 (Plant.get_price feed4)

let test_feed_wheat _ =
  let feed1 = Plant.feed wheat "William" in
  assert_equal 1 (Plant.get_height feed1);
  assert_equal 4.0 (Plant.get_price feed1)

let test_feed_rice _ =
  let feed1 = Plant.feed rice "Abigail" in
  assert_equal 1 (Plant.get_height feed1);
  assert_equal 5.0 (Plant.get_price feed1)

let test_feed_peach _ =
  let feed1 = Plant.feed peach "Alexander" in
  let feed2 = Plant.feed feed1 "Alexander" in
  let feed3 = Plant.feed feed2 "Alexander" in
  let feed4 = Plant.feed feed3 "Alexander" in
  assert_equal 4 (Plant.get_height feed4);
  assert_equal 3.5 (Plant.get_price feed4)

let test_feed_strawberry _ =
  let feed_plant = Plant.feed strawberry "Mia" in
  assert_equal 1 (Plant.get_height feed_plant);
  assert_equal 4.0 (Plant.get_price feed_plant)

let test_feed_corn _ =
  let feed1 = Plant.feed corn "Matthew" in
  let feed2 = Plant.feed feed1 "Matthew" in
  assert_equal 2 (Plant.get_height feed2);
  assert_equal 4.0 (Plant.get_price feed2)

let test_feed_cactus _ =
  let feed_plant = Plant.feed cactus "Charlotte" in
  assert_equal 1 (Plant.get_height feed_plant);
  assert_equal 30.0 (Plant.get_price feed_plant)

let test_feed_lettuce _ =
  let feed1 = Plant.feed lettuce "Henry" in
  let feed2 = Plant.feed feed1 "Henry" in
  let feed3 = Plant.feed feed2 "Henry" in
  assert_equal 3 (Plant.get_height feed3);
  assert_equal 6.0 (Plant.get_price feed3)

let test_feed_lemon _ =
  let feed1 = Plant.feed lemon "Benjamin" in
  let feed2 = Plant.feed feed1 "Benjamin" in
  let feed3 = Plant.feed feed2 "Benjamin" in
  assert_equal 3 (Plant.get_height feed3);
  assert_equal 3.2 (Plant.get_price feed3)

let test_feed_mango _ =
  let feed1 = Plant.feed mango "James" in
  let feed2 = Plant.feed feed1 "James" in
  assert_equal 2 (Plant.get_height feed2);
  assert_equal 6.0 (Plant.get_price feed2)

let test_feed_pineapple _ =
  let feed1 = Plant.feed pineapple "Amelia" in
  let feed2 = Plant.feed feed1 "Amelia" in
  let feed3 = Plant.feed feed2 "Amelia" in
  let feed4 = Plant.feed feed3 "Amelia" in
  assert_equal 4 (Plant.get_height feed4);
  assert_equal 5.8 (Plant.get_price feed4)

let test_feed_clover _ =
  let feed1 = Plant.feed clover "Ethan" in
  assert_equal 1 (Plant.get_height feed1);
  assert_equal 1.0 (Plant.get_price feed1)

let test_feed_bellpepper _ =
  let feed1 = Plant.feed bell_pepper "Ella" in
  let feed2 = Plant.feed feed1 "Ella" in
  assert_equal 2 (Plant.get_height feed2);
  assert_equal 2.0 (Plant.get_price feed2)

(* Test water function *)
let test_water_daisy _ =
  let water1 = Plant.water daisy "Sam" in
  assert_equal 26 (Plant.get_hydration water1)

let test_water_sunflower _ =
  let water1 = Plant.water sunflower "Emily" in
  let water2 = Plant.water water1 "Emily" in
  assert_equal 27 (Plant.get_hydration water2)

let test_water_rose _ =
  let water1 = Plant.water rose "Julie" in
  let water2 = Plant.water water1 "Julie" in
  assert_equal 27 (Plant.get_hydration water2)

let test_water_tulip _ =
  let water1 = Plant.water tulip "Michael" in
  let water2 = Plant.water water1 "Michael" in
  assert_equal 27 (Plant.get_hydration water2)

let test_water_tomato _ =
  let water1 = Plant.water tomato "Sophia" in
  let water2 = Plant.water water1 "Sophia" in
  let water3 = Plant.water water2 "Sophia" in
  let water4 = Plant.water water3 "Sophia" in
  assert_equal 29 (Plant.get_hydration water4)

let test_water_apple _ =
  let water1 = Plant.water apple "Isabella" in
  assert_equal 26 (Plant.get_hydration water1)

let test_water_onion _ =
  let water1 = Plant.water onion "Olivia" in
  let water2 = Plant.water water1 "Olivia" in
  assert_equal 27 (Plant.get_hydration water2)

let test_water_potato _ =
  let water1 = Plant.water potato "Daniel" in
  let water2 = Plant.water water1 "Daniel" in
  let water3 = Plant.water water2 "Daniel" in
  let water4 = Plant.water water3 "Daniel" in
  assert_equal 29 (Plant.get_hydration water4)

let test_water_wheat _ =
  let water1 = Plant.water wheat "William" in
  let hydration = Plant.get_hydration water1 in
  assert_equal 26 hydration

let test_water_rice _ =
  let water1 = Plant.water rice "Abigail" in
  assert_equal 26 (Plant.get_hydration water1)

let test_water_peach _ =
  let water1 = Plant.water peach "Alexander" in
  let water2 = Plant.water water1 "Alexander" in
  let water3 = Plant.water water2 "Alexander" in
  let water4 = Plant.water water3 "Alexander" in
  assert_equal 29 (Plant.get_hydration water4)

let test_water_strawberry _ =
  let water1 = Plant.water strawberry "Mia" in
  assert_equal 26 (Plant.get_hydration water1)

let test_water_corn _ =
  let water1 = Plant.water corn "Matthew" in
  let water2 = Plant.water water1 "Matthew" in
  assert_equal 27 (Plant.get_hydration water2)

let test_water_cactus _ =
  let water1 = Plant.water cactus "Charlotte" in
  assert_equal 11 (Plant.get_hydration water1)

let test_water_lettuce _ =
  let water1 = Plant.water lettuce "Henry" in
  let water2 = Plant.water water1 "Henry" in
  let water3 = Plant.water water2 "Henry" in
  assert_equal 28 (Plant.get_hydration water3)

let test_water_mango _ =
  let water1 = Plant.water mango "James" in
  let water2 = Plant.water water1 "James" in
  assert_equal 27 (Plant.get_hydration water2)

let test_water_pineapple _ =
  let water1 = Plant.water pineapple "Amelia" in
  let water2 = Plant.water water1 "Amelia" in
  let water3 = Plant.water water2 "Amelia" in
  let water4 = Plant.water water3 "Amelia" in
  assert_equal 29 (Plant.get_hydration water4)

let test_water_clover _ =
  let water1 = Plant.water clover "Ethan" in
  let hydration = Plant.get_hydration water1 in
  assert_equal 26 hydration

let test_water_bellpepper _ =
  let water1 = Plant.water bell_pepper "Ella" in
  let water2 = Plant.water water1 "Ella" in
  let water3 = Plant.water water2 "Ella" in
  assert_equal 28 (Plant.get_hydration water3)

(* Test neglect function*)
let test_neglect_daisy _ =
  let water1 = Plant.neglect daisy "Sam" in
  assert_equal 24 (Plant.get_hydration water1)

let test_neglect_sunflower _ =
  let water1 = Plant.neglect sunflower "Emily" in
  let water2 = Plant.neglect water1 "Emily" in
  assert_equal 23 (Plant.get_hydration water2)

let test_neglect_rose _ =
  let water1 = Plant.neglect rose "Julie" in
  let water2 = Plant.neglect water1 "Julie" in
  assert_equal 23 (Plant.get_hydration water2)

let test_neglect_tulip _ =
  let water1 = Plant.neglect tulip "Michael" in
  let water2 = Plant.neglect water1 "Michael" in
  assert_equal 23 (Plant.get_hydration water2)

let test_neglect_tomato _ =
  let water1 = Plant.neglect tomato "Sophia" in
  let water2 = Plant.neglect water1 "Sophia" in
  let water3 = Plant.neglect water2 "Sophia" in
  let water4 = Plant.neglect water3 "Sophia" in
  assert_equal 21 (Plant.get_hydration water4)

let test_neglect_apple _ =
  let water1 = Plant.neglect apple "Isabella" in
  assert_equal 24 (Plant.get_hydration water1)

let test_neglect_onion _ =
  let water1 = Plant.neglect onion "Olivia" in
  let water2 = Plant.neglect water1 "Olivia" in
  assert_equal 23 (Plant.get_hydration water2)

let test_neglect_potato _ =
  let water1 = Plant.neglect potato "Daniel" in
  let water2 = Plant.neglect water1 "Daniel" in
  let water3 = Plant.neglect water2 "Daniel" in
  let water4 = Plant.neglect water3 "Daniel" in
  assert_equal 21 (Plant.get_hydration water4)

let test_neglect_wheat _ =
  let water1 = Plant.neglect wheat "William" in
  assert_equal 24 (Plant.get_hydration water1)

let test_neglect_rice _ =
  let water1 = Plant.neglect rice "Abigail" in
  assert_equal 24 (Plant.get_hydration water1)

let test_neglect_peach _ =
  let water1 = Plant.neglect peach "Alexander" in
  let water2 = Plant.neglect water1 "Alexander" in
  let water3 = Plant.neglect water2 "Alexander" in
  let water4 = Plant.neglect water3 "Alexander" in
  assert_equal 21 (Plant.get_hydration water4)

let test_neglect_strawberry _ =
  let water1 = Plant.neglect strawberry "Mia" in
  assert_equal 24 (Plant.get_hydration water1)

let test_neglect_corn _ =
  let water1 = Plant.neglect corn "Matthew" in
  let water2 = Plant.neglect water1 "Matthew" in
  assert_equal 23 (Plant.get_hydration water2)

let test_neglect_cactus _ =
  let water1 = Plant.neglect cactus "Charlotte" in
  assert_equal 9 (Plant.get_hydration water1)

let test_neglect_lettuce _ =
  let water1 = Plant.neglect lettuce "Henry" in
  let water2 = Plant.neglect water1 "Henry" in
  let water3 = Plant.neglect water2 "Henry" in
  assert_equal 22 (Plant.get_hydration water3)

let test_neglect_mango _ =
  let water1 = Plant.neglect mango "James" in
  let water2 = Plant.neglect water1 "James" in
  assert_equal 23 (Plant.get_hydration water2)

let test_neglect_pineapple _ =
  let water1 = Plant.neglect pineapple "Amelia" in
  let water2 = Plant.neglect water1 "Amelia" in
  let water3 = Plant.neglect water2 "Amelia" in
  let water4 = Plant.neglect water3 "Amelia" in
  assert_equal 21 (Plant.get_hydration water4)

let test_neglect_clover _ =
  let water1 = Plant.neglect clover "Ethan" in
  assert_equal 24 (Plant.get_hydration water1)

let test_neglect_bellpepper _ =
  let water1 = Plant.neglect bell_pepper "Ella" in
  let water2 = Plant.neglect water1 "Ella" in
  let water3 = Plant.neglect water2 "Ella" in
  assert_equal 22 (Plant.get_hydration water3)

(* Test stampede function *)
let test_stampede_life_true _ =
  let stampeded_sunflower = Plant.stampede 0.3 sunflower in
  assert_equal true (Plant.get_life stampeded_sunflower)

let feed_tests =
  "feed test suite"
  >::: [
         "test feed daisy" >:: test_feed_daisy;
         "test feed sunflower" >:: test_feed_sunflower;
         "test feed rose" >:: test_feed_rose;
         "test feed tulip" >:: test_feed_tulip;
         "test feed tomato" >:: test_feed_tomato;
         "test feed apple" >:: test_feed_apple;
         "test feed onion" >:: test_feed_onion;
         "test feed potato" >:: test_feed_potato;
         "test feed wheat" >:: test_feed_wheat;
         "test feed rice" >:: test_feed_rice;
         "test feed peach" >:: test_feed_peach;
         "test feed strawberry" >:: test_feed_strawberry;
         "test feed corn" >:: test_feed_corn;
         "test feed cactus" >:: test_feed_cactus;
         "test feed lettuce" >:: test_feed_lettuce;
         "test feed lemon" >:: test_feed_lemon;
         "test feed mango" >:: test_feed_mango;
         "test feed pineapple" >:: test_feed_pineapple;
         "test feed clover" >:: test_feed_clover;
         "test feed bellpepper" >:: test_feed_bellpepper;
       ]

let water_tests =
  "water test suite"
  >::: [
         "test water daisy" >:: test_water_daisy;
         "test water sunflower" >:: test_water_sunflower;
         "test water rose" >:: test_water_rose;
         "test water tulip" >:: test_water_tulip;
         "test water tomato" >:: test_water_tomato;
         "test water apple" >:: test_water_apple;
         "test water onion" >:: test_water_onion;
         "test water potato" >:: test_water_potato;
         "test water wheat" >:: test_water_wheat;
         "test water rice" >:: test_water_rice;
         "test water peach" >:: test_water_peach;
         "test water strawberry" >:: test_water_strawberry;
         "test water corn" >:: test_water_corn;
         "test water cactus" >:: test_water_cactus;
         "test water lettuce" >:: test_water_lettuce;
         "test water mango" >:: test_water_mango;
         "test water pineapple" >:: test_water_pineapple;
         "test water clover" >:: test_water_clover;
         "test water bellpepper" >:: test_water_bellpepper;
       ]

let neglect_tests =
  "neglect test suite"
  >::: [
         "test neglect daisy" >:: test_neglect_daisy;
         "test neglect sunflower" >:: test_neglect_sunflower;
         "test neglect rose" >:: test_neglect_rose;
         "test neglect tulip" >:: test_neglect_tulip;
         "test neglect tomato" >:: test_neglect_tomato;
         "test neglect apple" >:: test_neglect_apple;
         "test neglect onion" >:: test_neglect_onion;
         "test neglect potato" >:: test_neglect_potato;
         "test neglect wheat" >:: test_neglect_wheat;
         "test neglect rice" >:: test_neglect_rice;
         "test neglect peach" >:: test_neglect_peach;
         "test neglect strawberry" >:: test_neglect_strawberry;
         "test neglect corn" >:: test_neglect_corn;
         "test neglect cactus" >:: test_neglect_cactus;
         "test neglect lettuce" >:: test_neglect_lettuce;
         "test neglect mango" >:: test_neglect_mango;
         "test neglect pineapple" >:: test_neglect_pineapple;
         "test neglect clover" >:: test_neglect_clover;
         "test neglect bellpepper" >:: test_neglect_bellpepper;
       ]

let inventory_tests =
  "inventory test suite"
  >::: [
         ( "a new inventory has nothing in it" >:: fun _ ->
           assert_equal 0 (Inventory.get_length new_inv) );
         ( "test adding a eggs" >:: fun _ ->
           assert_equal 1 (Inventory.get_length add_eggs) );
         ( "test adding cheese" >:: fun _ ->
           assert_equal 1 (Inventory.get_length add_cheese) );
         ( "test adding milk" >:: fun _ ->
           assert_equal 1 (Inventory.get_length add_milk) );
         ( "Harvesting short plant shouldn't add it to garden: flower"
         >:: fun _ -> assert_equal 0 (Garden.get_plant_count (snd add_daisy)) );
         ( "Harvesting short plant shouldn't add it to garden: fruit"
         >:: fun _ -> assert_equal 0 (Garden.get_plant_count (snd add_apple)) );
         ( "Harvesting short plant shouldn't add it to garden: defensive item"
         >:: fun _ -> assert_equal 0 (Garden.get_plant_count (snd add_cactus))
         );
         ( "Harvesting short plant shouldn't add it to garden: grains"
         >:: fun _ -> assert_equal 0 (Garden.get_plant_count (snd add_corn)) );
         ( "test adding a plant increases inventory size by 1" >:: fun _ ->
           assert_equal 1
             (Inventory.get_length add_eggs - Inventory.get_length new_inv) );
       ]

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
         water_tests;
         "test_apply_discount" >:: test_apply_discount;
         feed_tests;
         neglect_tests;
         inventory_tests;
         "test stampede" >:: test_stampede_life_true;
       ]

let () = run_test_tt_main suite

type plant_type = {
  height : int;
  life : bool;
  hydration : int;
  name : string;
  price : float;
}

type species =
  | Daisy
  | Sunflower
  | Rose
  | Tulip
  | Tomato
  | Potato
  | Onion
  | Wheat
  | Apple
  | Peach
  | Strawberry
  | Mango
  | Cactus
  | Lemon
  | Pineapple
  | Clover
  | Rice
  | Lettuce
  | Bell_Pepper
  | Corn

type t = plant_type * species

let get_name (plant : t) =
  match plant with
  | plant_type, _ -> plant_type.name

let get_height (plant : t) =
  match plant with
  | plant_type, _ -> plant_type.height

let get_life (plant : t) =
  match plant with
  | plant_type, _ -> plant_type.life

let get_hydration (plant : t) =
  match plant with
  | plant_type, _ -> plant_type.hydration

let get_price (plant : t) =
  match plant with
  | plant_type, _ -> plant_type.price

let get_sale_price (plant : t) (plant_name : string) : float =
  match plant with
  | { name = n; price = p; _ }, _ -> if plant_name = n then p else 0.0

let apply_discount (plant : t) : t =
  let rand_val = Random.float 1.0 in
  match plant with
  | plant_type, species ->
      let new_price =
        if rand_val < 0.5 then plant_type.price *. 0.5 else plant_type.price
      in
      ({ plant_type with price = new_price }, species)

(**OVER LINE COUNT BY 3 *)
let get_type (plant : t) =
  match plant with
  | _, species -> (
      match species with
      | Daisy -> "Daisy"
      | Sunflower -> "Sunflower"
      | Rose -> "Rose"
      | Tulip -> "Tulip"
      | Tomato -> "Tomato"
      | Potato -> "Potato"
      | Onion -> "Onion"
      | Wheat -> "Wheat"
      | Apple -> "Apple"
      | Peach -> "Peach"
      | Strawberry -> "Strawberry"
      | Mango -> "Mango"
      | Cactus -> "Cactus"
      | Lemon -> "Lemon"
      | Pineapple -> "Pineapple"
      | Clover -> "Clover"
      | Rice -> "Rice"
      | Lettuce -> "Lettuce"
      | Bell_Pepper -> "Bell Pepper"
      | Corn -> "Corn")

let feed (type_of_plant : t) plant_name : t =
  match type_of_plant with
  | plant_type, species ->
      let new_price =
        if plant_name = plant_type.name then plant_type.price +. 1.0
        else plant_type.price
      in
      let new_height =
        if plant_name = plant_type.name then plant_type.height + 1
        else plant_type.height
      in
      ({ plant_type with price = new_price; height = new_height }, species)

let water (type_of_plant : t) plant_name : t =
  match type_of_plant with
  | plant_type, species ->
      let new_hydration =
        if plant_name = plant_type.name then plant_type.hydration + 1
        else plant_type.hydration
      in
      ({ plant_type with hydration = new_hydration }, species)

let neglect (type_of_plant : t) plant_name : t =
  match type_of_plant with
  | plant_type, species ->
      let new_hydration =
        if plant_name = plant_type.name then plant_type.hydration - 1
        else plant_type.hydration
      in
      ({ plant_type with hydration = new_hydration }, species)

let get_plant_emoji height threshold emoji_if_true emoji_if_false =
  if height < threshold then emoji_if_true else emoji_if_false

(**OVER LINE COUNT BY 3 *)
let print_plant (plant : t) =
  match plant with
  | { height = h; life = _; hydration = _; name = _; price = _ }, species -> (
      match species with
      | Daisy -> get_plant_emoji h 3 "🌱" "��"
      | Sunflower -> get_plant_emoji h 6 "🌱" "��"
      | Rose -> get_plant_emoji h 4 "🌱" "��"
      | Tulip -> get_plant_emoji h 5 "🌱" "��"
      | Tomato -> get_plant_emoji h 3 "🌱" (if h < 7 then "��" else "🍅")
      | Lettuce -> get_plant_emoji h 4 "🌱" "��"
      | Onion -> get_plant_emoji h 4 "🌱" "��"
      | Potato -> get_plant_emoji h 6 "🌱" "��"
      | Wheat -> get_plant_emoji h 5 "🌱" "��"
      | Apple -> get_plant_emoji h 3 "🌱" (if h < 7 then "��" else "🍎")
      | Mango -> get_plant_emoji h 6 "🌱" "��"
      | Peach -> get_plant_emoji h 3 "🌱" (if h < 7 then "��" else "🍑")
      | Strawberry -> get_plant_emoji h 2 "🌱" (if h < 7 then "��" else "🍓")
      | Cactus -> get_plant_emoji h 7 "🌿" "��"
      | Lemon -> get_plant_emoji h 4 "🌱" "��"
      | Pineapple -> get_plant_emoji h 4 "🌱" "��"
      | Rice -> get_plant_emoji h 4 "🎍" "��"
      | Clover -> get_plant_emoji h 1 "🌱" "��"
      | Bell_Pepper -> get_plant_emoji h 4 "🌱" "��"
      | Corn -> get_plant_emoji h 4 "🌱" (if h < 7 then "��" else "🌽"))

(**OVER LINE COUNT BY 5 *)
let create_plant plant_name n : t =
  let plant_type1 =
    { height = 0; life = true; hydration = 25; name = n; price = 0.0 }
  in
  match plant_name with
  | "Daisy" -> ({ plant_type1 with price = 3.0 }, Daisy)
  | "Sunflower" -> ({ plant_type1 with price = 2.0 }, Sunflower)
  | "Rose" -> ({ plant_type1 with price = 8.0 }, Rose)
  | "Tulip" -> ({ plant_type1 with price = 5.0 }, Tulip)
  | "Tomato" -> ({ plant_type1 with price = 5.0 }, Tomato)
  | "Apple" -> ({ plant_type1 with price = 7.0 }, Apple)
  | "Onion" -> ({ plant_type1 with price = 5.0 }, Onion)
  | "Potato" -> ({ plant_type1 with price = 7.0 }, Potato)
  | "Wheat" -> ({ plant_type1 with price = 4.0 }, Wheat)
  | "Rice" -> ({ plant_type1 with price = 5.0 }, Rice)
  | "Peach" -> ({ plant_type1 with price = 3.5 }, Peach)
  | "Strawberry" -> ({ plant_type1 with price = 4.0 }, Strawberry)
  | "Corn" -> ({ plant_type1 with price = 4.0 }, Corn)
  | "Cactus" -> ({ plant_type1 with price = 30.0; hydration = 10 }, Cactus)
  | "Lettuce" -> ({ plant_type1 with price = 6.0 }, Lettuce)
  | "Lemon" -> ({ plant_type1 with price = 3.2 }, Lemon)
  | "Mango" -> ({ plant_type1 with price = 6.0 }, Mango)
  | "Pineapple" -> ({ plant_type1 with price = 5.8 }, Pineapple)
  | "Clover" -> ({ plant_type1 with price = 1.0 }, Clover)
  | "Bell Pepper" -> ({ plant_type1 with price = 2.0 }, Bell_Pepper)
  | _ -> failwith "Our store doesn't have this plant yet. Please try again."

let check_life (type_of_plant : t) plant_name : t =
  match type_of_plant with
  | plant_type, species ->
      let new_life =
        if
          plant_name = plant_type.name
          && (plant_type.hydration > 20 || plant_type.hydration < 0)
        then plant_type.life = false
        else plant_type.life
      in
      ({ plant_type with life = new_life }, species)

let get_category (type_of_plant : t) =
  match type_of_plant with
  | _, Daisy | _, Sunflower | _, Rose | _, Tulip -> "Flowers"
  | _, Tomato | _, Lettuce | _, Bell_Pepper | _, Onion | _, Potato ->
      "Vegetables"
  | _, Lemon | _, Pineapple | _, Apple | _, Peach | _, Strawberry | _, Mango ->
      "Fruits"
  | _, Clover | _, Cactus -> "Defensive Items"
  | _, Rice | _, Wheat | _, Corn -> "Grains"

let check_status (plant : t) =
  match plant with
  | { height = _; life = _; hydration = y; name = n; price = p }, species ->
      if y > 20 || y < 0 then
        ( { height = 0; life = false; hydration = y; name = n; price = p },
          species )
      else plant

let stampede rand_val (plant : t) : t =
  (* let rand_val = Random.float 1.0 in *)
  match plant with
  | { height = h; life = _; hydration = w; name = n; price = p }, species ->
      if rand_val < 0.2 then
        (*0.5 - defense_val*)
        ( { height = h; life = false; hydration = w; name = n; price = p },
          species )
      else
        ( { height = h; life = true; hydration = w - 2; name = n; price = p },
          species )

let pollinate (plant : t) : t =
  match plant with
  | { height = h; life = l; hydration = w; name = n; price = p }, species ->
      ( { height = h + 2; life = l; hydration = w + 1; name = n; price = p },
        species )

let drought (plant : t) : t =
  match plant with
  | { height = h; life = l; hydration = w; name = n; price = p }, species ->
      check_status
        ( { height = h; life = l; hydration = w - 3; name = n; price = p },
          species )

let dragon (plant : t) : t =
  let rand_val = Random.float 1.0 in
  match plant with
  | { height = h; life = _; hydration = w; name = n; price = p }, species ->
      if rand_val < 0.3 then
        (*0.5 - defense_val*)
        ( { height = h; life = false; hydration = w; name = n; price = p },
          species )
      else
        ( { height = h; life = true; hydration = w - 4; name = n; price = p },
          species )

let ice (plant : t) : t =
  match plant with
  | { height = h; life = l; hydration = w; name = n; price = p }, species ->
      check_status
        ( { height = h - 2; life = l; hydration = w - 1; name = n; price = p },
          species )

let unicorn (plant : t) : t =
  match plant with
  | { height = h; life = l; hydration = w; name = n; price = p }, species ->
      check_status
        ( { height = h + 2; life = l; hydration = w + 2; name = n; price = p },
          species )

let fairies (plant : t) : t =
  match plant with
  | { height = h; life = l; hydration = w; name = n; price = p }, species ->
      check_status
        ( { height = h + 3; life = l; hydration = w + 1; name = n; price = p },
          species )

let is_alive (plant : t) =
  match plant with
  | { life = l; _ }, _ -> l = true

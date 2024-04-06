type plant_type = {
  height : int;
  life : bool;
  hydration : int;
  name : string;
  price : float;
}

type t =
  | Daisy of plant_type
  | Sunflower of plant_type
  | Rose of plant_type
  | Tulip of plant_type
  | Tomato of plant_type
  | Corn of plant_type
  | Carrot of plant_type
  | Onion of plant_type
  | Potato of plant_type
  | Wheat of plant_type
  | Apple of plant_type
  | Peach of plant_type
  | Strawberry of plant_type
  | Orange of plant_type
  | Cactus of plant_type

let get_type plant =
  match plant with
  | Daisy { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Daisy"
  | Sunflower { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Sunflower"
  | Rose { height = _; life = _; hydration = _; name = _; price = _ } -> "Rose"
  | Tulip { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Tulip"
  | Tomato { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Tomato"
  | Corn { height = _; life = _; hydration = _; name = _; price = _ } -> "Corn"
  | Carrot { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Carrot"
  | Onion { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Onion"
  | Potato { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Potato"
  | Wheat { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Wheat"
  | Apple { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Apple"
  | Peach { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Peach"
  | Strawberry { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Strawberry"
  | Orange { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Orange"
  | Cactus { height = _; life = _; hydration = _; name = _; price = _ } ->
      "Cactus"

let get_height plant =
  match plant with
  | Daisy { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Sunflower { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Rose { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Tulip { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Tomato { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Corn { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Carrot { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Onion { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Potato { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Wheat { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Apple { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Peach { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Strawberry { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Orange { height = h; life = _; hydration = _; name = _; price = _ } -> h
  | Cactus { height = h; life = _; hydration = _; name = _; price = _ } -> h

let feed plant =
  match plant with
  | Daisy { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Daisy
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Daisy { height = _; life = false; hydration = _; name = n; price = _ } ->
      Daisy { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Sunflower
      { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Sunflower
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Sunflower { height = _; life = false; hydration = _; name = n; price = _ }
    ->
      Sunflower
        { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Rose { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Rose
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Rose { height = _; life = false; hydration = _; name = n; price = _ } ->
      Rose { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Tulip { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Tulip
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Tulip { height = _; life = false; hydration = _; name = n; price = _ } ->
      Tulip { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Tomato { height = h; life = true; hydration = hydro; name = n; price = p }
    ->
      Tomato
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Tomato { height = _; life = false; hydration = _; name = n; price = _ } ->
      Tomato { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Corn { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Corn
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Corn { height = _; life = false; hydration = _; name = n; price = _ } ->
      Corn { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Carrot { height = h; life = true; hydration = hydro; name = n; price = p }
    ->
      Carrot
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Carrot { height = _; life = false; hydration = _; name = n; price = _ } ->
      Carrot { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Onion { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Onion
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Onion { height = _; life = false; hydration = _; name = n; price = _ } ->
      Onion { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Potato { height = h; life = true; hydration = hydro; name = n; price = p }
    ->
      Potato
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Potato { height = _; life = false; hydration = _; name = n; price = _ } ->
      Potato { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Wheat { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Wheat
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Wheat { height = _; life = false; hydration = _; name = n; price = _ } ->
      Wheat { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Apple { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Apple
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Apple { height = _; life = false; hydration = _; name = n; price = _ } ->
      Apple { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Peach { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Peach
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Peach { height = _; life = false; hydration = _; name = n; price = _ } ->
      Peach { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Strawberry
      { height = h; life = true; hydration = hydro; name = n; price = p } ->
      Strawberry
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Strawberry { height = _; life = false; hydration = _; name = n; price = _ }
    ->
      Strawberry
        { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Orange { height = h; life = true; hydration = hydro; name = n; price = p }
    ->
      Orange
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Orange { height = _; life = false; hydration = _; name = n; price = _ } ->
      Orange { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Cactus { height = h; life = true; hydration = hydro; name = n; price = p }
    ->
      Cactus
        { height = h + 1; life = true; hydration = hydro; name = n; price = p }
  | Cactus { height = _; life = false; hydration = _; name = n; price = _ } ->
      Cactus { height = 0; life = false; hydration = 0; name = n; price = 0.0 }

(** could have false case in water, neglect, and print for more lines ASK
    BRENDEN if func can have more than 20*)
let water plant =
  match plant with
  | Daisy { height = x; life = true; hydration = h; name = n; price = p } ->
      Daisy { height = x; life = true; hydration = h + 1; name = n; price = p }
  | Daisy { height = _; life = false; hydration = _; name = n; price = p } ->
      Daisy { height = 0; life = false; hydration = 0; name = n; price = p }
  | Sunflower { height = h; life = b; hydration = hydro; name = n; price = p }
    ->
      Sunflower
        { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Rose { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Rose { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Tulip { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Tulip { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Tomato { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Tomato
        { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Corn { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Corn { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Carrot { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Carrot
        { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Onion { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Onion { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Potato { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Potato
        { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Wheat { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Wheat { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Apple { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Apple { height = h; life = b; hydration = hydro + 1; name = n; price = p }
  | Peach { height = x; life = true; hydration = h; name = n; price = p } ->
      Peach { height = x; life = true; hydration = h + 1; name = n; price = p }
  | Peach { height = x; life = false; hydration = _; name = n; price = p } ->
      Peach { height = x; life = false; hydration = 0; name = n; price = p }
  | Strawberry { height = x; life = true; hydration = h; name = n; price = p }
    ->
      Strawberry
        { height = x; life = true; hydration = h + 1; name = n; price = p }
  | Strawberry { height = x; life = false; hydration = _; name = n; price = p }
    ->
      Strawberry
        { height = x; life = false; hydration = 0; name = n; price = p }
  | Orange { height = h; life = true; hydration = hydro; name = n; price = p }
    ->
      Orange
        { height = h; life = true; hydration = hydro + 1; name = n; price = p }
  | Orange { height = _h; life = false; hydration = _; name = n; price = _ } ->
      Orange { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Cactus { height = x; life = true; hydration = h; name = n; price = p } ->
      Cactus { height = x; life = true; hydration = h + 1; name = n; price = p }
  | Cactus { height = x; life = false; hydration = _; name = n; price = p } ->
      Cactus { height = x; life = false; hydration = 0; name = n; price = p }

let neglect plant =
  match plant with
  | Daisy { height = x; life = true; hydration = hydro; name = n; price = p } ->
      Daisy
        { height = x; life = true; hydration = hydro - 1; name = n; price = p }
  | Daisy { height = _; life = false; hydration = _; name = n; price = p } ->
      Daisy { height = 0; life = false; hydration = 0; name = n; price = p }
  | Sunflower { height = h; life = b; hydration = hydro; name = n; price = p }
    ->
      Sunflower
        { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Rose { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Rose { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Tulip { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Tulip { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Tomato { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Tomato
        { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Corn { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Corn { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Carrot { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Carrot
        { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Onion { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Onion { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Potato { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Potato
        { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Wheat { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Wheat { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Apple { height = h; life = b; hydration = hydro; name = n; price = p } ->
      Apple { height = h; life = b; hydration = hydro - 1; name = n; price = p }
  | Peach { height = x; life = true; hydration = hydro; name = n; price = p } ->
      Peach
        { height = x; life = true; hydration = hydro - 1; name = n; price = p }
  | Peach { height = _; life = false; hydration = _; name = n; price = p } ->
      Peach { height = 0; life = false; hydration = 0; name = n; price = p }
  | Strawberry
      { height = x; life = true; hydration = hydro; name = n; price = p } ->
      Strawberry
        { height = x; life = true; hydration = hydro - 1; name = n; price = p }
  | Strawberry { height = _; life = false; hydration = _; name = n; price = p }
    ->
      Strawberry
        { height = 0; life = false; hydration = 0; name = n; price = p }
  | Orange { height = h; life = true; hydration = hydro; name = n; price = p }
    ->
      Orange
        { height = h; life = true; hydration = hydro + 1; name = n; price = p }
  | Orange { height = _h; life = false; hydration = _; name = n; price = _ } ->
      Orange { height = 0; life = false; hydration = 0; name = n; price = 0.0 }
  | Cactus { height = x; life = true; hydration = hydro; name = n; price = p }
    ->
      Cactus
        { height = x; life = true; hydration = hydro - 1; name = n; price = p }
  | Cactus { height = _; life = false; hydration = _; name = n; price = p } ->
      Cactus { height = 0; life = false; hydration = 0; name = n; price = p }

let print_plant plant =
  match plant with
  | Daisy { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 3 then "🌱" else "🌹"
  | Daisy { height = _; life = false; hydration = _; name = _; price = _ } -> ""
  | Sunflower { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 6 then "🌱" else "🌻"
  | Sunflower { height = _; life = false; hydration = _; name = _; price = _ }
    -> ""
  | Rose { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 4 then "🌱" else "🌹"
  | Rose { height = _; life = false; hydration = _; name = _; price = _ } -> ""
  | Tulip { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 5 then "🌱" else "🌷"
  | Tulip { height = _; life = false; hydration = _; name = _; price = _ } -> ""
  | Tomato { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 3 then "🌱" else if h >= 3 && h < 7 then "🪴" else "🍅"
  | Tomato { height = _; life = false; hydration = _; name = _; price = _ } ->
      ""
  | Corn { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 4 then "🌱" else "🌽"
  | Corn { height = _; life = false; hydration = _; name = _; price = _ } -> ""
  | Carrot { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 5 then "🌱" else "🥕"
  | Carrot { height = _; life = false; hydration = _; name = _; price = _ } ->
      ""
  | Onion { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 4 then "🌱" else "🧅"
  | Onion { height = _; life = false; hydration = _; name = _; price = _ } -> ""
  | Potato { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 6 then "🌱" else "🥔"
  | Potato { height = _; life = false; hydration = _; name = _; price = _ } ->
      ""
  | Wheat { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 5 then "🌱" else "🌾"
  | Wheat { height = _; life = false; hydration = _; name = _; price = _ } -> ""
  | Apple { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 3 then "🌱" else if h >= 3 && h < 7 then "🌳" else "🍏"
  | Apple { height = _; life = false; hydration = _; name = _; price = _ } -> ""
  | Orange { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 6 then "🌱" else "🍊"
  | Orange { height = _; life = false; hydration = _; name = _; price = _ } ->
      ""
  | Peach { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 5 then "🌱" else "🍑"
  | Peach { height = _; life = false; hydration = _; name = _; price = _ } -> ""
  | Strawberry { height = h; life = true; hydration = _; name = _; price = _ }
    -> if h < 2 then "🍂" else "🍓"
  | Strawberry { height = _; life = false; hydration = _; name = _; price = _ }
    -> ""
  | Cactus { height = h; life = true; hydration = _; name = _; price = _ } ->
      if h < 7 then "🌿" else "🌵"
  | Cactus { height = _; life = false; hydration = _; name = _; price = _ } ->
      ""

let create_plant plant_name n =
  match plant_name with
  | "Daisy" ->
      Daisy { height = 0; life = true; hydration = 25; name = n; price = 3.0 }
  | "Sunflower" ->
      Sunflower
        { height = 0; life = true; hydration = 25; name = n; price = 2.0 }
  | "Rose" ->
      Rose { height = 0; life = true; hydration = 25; name = n; price = 8.0 }
  | "Tulip" ->
      Tulip { height = 0; life = true; hydration = 25; name = n; price = 5.0 }
  | "Tomato" ->
      Tomato { height = 0; life = true; hydration = 25; name = n; price = 5.0 }
  | "Apple" ->
      Apple { height = 0; life = true; hydration = 25; name = n; price = 7.0 }
  | "Corn" ->
      Corn { height = 0; life = true; hydration = 25; name = n; price = 2.0 }
  | "Carrot" ->
      Carrot { height = 0; life = true; hydration = 25; name = n; price = 3.0 }
  | "Onion" ->
      Onion { height = 0; life = true; hydration = 25; name = n; price = 5.0 }
  | "Potato" ->
      Potato { height = 0; life = true; hydration = 25; name = n; price = 7.0 }
  | "Wheat" ->
      Wheat { height = 0; life = true; hydration = 25; name = n; price = 4.0 }
  | "Orange" ->
      Orange { height = 0; life = true; hydration = 25; name = n; price = 2.0 }
  | "Peach" ->
      Peach { height = 0; life = true; hydration = 25; name = n; price = 3.5 }
  | "Strawberry" ->
      Strawberry
        { height = 0; life = true; hydration = 25; name = n; price = 4.0 }
  | "Cactus" ->
      Cactus { height = 0; life = true; hydration = 10; name = n; price = 8.0 }
  | _ ->
      failwith
        "Our store doesn't have this plant yet. Please try to buy a plant we \
         have."

let check_life plant =
  match plant with
  | Daisy { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 20 || y < 0 then
        Daisy { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Sunflower
      { height = h; life = true; hydration = hydro; name = n; price = p } ->
      if hydro > 20 || hydro < 0 then
        Sunflower
          { height = h; life = false; hydration = hydro; name = n; price = p }
      else plant
  | Peach { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 10 || y < 0 then
        Peach { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Strawberry { height = _; life = true; hydration = y; name = n; price = p }
    ->
      if y > 10 || y < 0 then
        Strawberry
          { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Cactus { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 5 || y < 0 then
        Cactus { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Rose { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Rose { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Tulip { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Tulip { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Carrot { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Carrot { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Onion { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Onion { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Potato { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Potato { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Wheat { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Wheat { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Apple { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Apple { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Orange { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Orange { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | Corn { height = _; life = true; hydration = y; name = n; price = p } ->
      if y > 15 || y < 0 then
        Corn { height = 0; life = false; hydration = y; name = n; price = p }
      else plant
  | _ -> plant

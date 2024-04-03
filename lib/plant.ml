type plant_type = {
  height : int;
  life : bool;
  hydration : int;
}

type t =
  | Flower of plant_type
  | Peach of plant_type
  | Strawberry of plant_type
  | Cactus of plant_type

let feed plant =
  match plant with
  | Flower { height = x; life = true; hydration = hydro } ->
      Flower { height = x + 1; life = true; hydration = hydro }
  | Flower { height = _; life = false; hydration = _ } ->
      Flower { height = 0; life = false; hydration = 0 }
  | Peach { height = x; life = true; hydration = hydro } ->
      Peach { height = x + 1; life = true; hydration = hydro }
  | Peach { height = _; life = false; hydration = _ } ->
      Peach { height = 0; life = false; hydration = 0 }
  | Strawberry { height = x; life = true; hydration = hydro } ->
      Strawberry { height = x + 1; life = true; hydration = hydro }
  | Strawberry { height = _; life = false; hydration = _ } ->
      Strawberry { height = 0; life = false; hydration = 0 }
  | Cactus { height = x; life = true; hydration = hydro } ->
      Cactus { height = x + 1; life = true; hydration = hydro }
  | Cactus { height = _; life = false; hydration = _ } ->
      Cactus { height = 0; life = false; hydration = 0 }

let water plant =
  match plant with
  | Flower { height = x; life = true; hydration = h } ->
      Flower { height = x; life = true; hydration = h + 1 }
  | Flower { height = x; life = false; hydration = _ } ->
      Flower { height = x; life = false; hydration = 0 }
  | Peach { height = x; life = true; hydration = h } ->
      Peach { height = x; life = true; hydration = h + 1 }
  | Peach { height = x; life = false; hydration = _ } ->
      Peach { height = x; life = false; hydration = 0 }
  | Strawberry { height = x; life = true; hydration = h } ->
      Strawberry { height = x; life = true; hydration = h + 1 }
  | Strawberry { height = x; life = false; hydration = _ } ->
      Strawberry { height = x; life = false; hydration = 0 }
  | Cactus { height = x; life = true; hydration = h } ->
      Cactus { height = x; life = true; hydration = h + 1 }
  | Cactus { height = x; life = false; hydration = _ } ->
      Cactus { height = x; life = false; hydration = 0 }

let neglect plant =
  match plant with
  | Flower { height = x; life = true; hydration = hydro } ->
      Flower { height = x; life = true; hydration = hydro - 1 }
  | Flower { height = _; life = false; hydration = _ } ->
      Flower { height = 0; life = false; hydration = 0 }
  | Peach { height = x; life = true; hydration = hydro } ->
      Peach { height = x; life = true; hydration = hydro - 1 }
  | Peach { height = _; life = false; hydration = _ } ->
      Peach { height = 0; life = false; hydration = 0 }
  | Strawberry { height = x; life = true; hydration = hydro } ->
      Strawberry { height = x; life = true; hydration = hydro - 1 }
  | Strawberry { height = _; life = false; hydration = _ } ->
      Strawberry { height = 0; life = false; hydration = 0 }
  | Cactus { height = x; life = true; hydration = hydro } ->
      Cactus { height = x; life = true; hydration = hydro - 1 }
  | Cactus { height = _; life = false; hydration = _ } ->
      Cactus { height = 0; life = false; hydration = 0 }

let print_plant plant =
  match plant with
  | Flower { height = h; life = true; hydration = _ } ->
      if h < 3 then "🍃" else "🌹"
  | Flower { height = h; life = false; hydration = _ } ->
      if h < 3 then "🍃" else "🌹"
  | Peach { height = h; life = true; hydration = _ } ->
      if h < 5 then "🌱" else "🍊"
  | Peach { height = h; life = false; hydration = _ } ->
      if h < 5 then "🌱" else "🍊"
  | Strawberry { height = h; life = true; hydration = _ } ->
      if h < 2 then "🍂" else "🍓"
  | Strawberry { height = h; life = false; hydration = _ } ->
      if h < 2 then "🍂" else "🍓"
  | Cactus { height = h; life = true; hydration = _ } ->
      if h < 7 then "🌿" else "🌵"
  | Cactus { height = h; life = false; hydration = _ } ->
      if h < 7 then "🌿" else "🌵"

let create_plant plant_name =
  match plant_name with
  | "Flower" -> Flower { height = 3; life = true; hydration = 25 }
  | "Peach" -> Peach { height = 3; life = true; hydration = 25 }
  | "Strawberry" -> Strawberry { height = 4; life = true; hydration = 25 }
  | "Cactus" -> Cactus { height = 5; life = true; hydration = 10 }
  | _ ->
      failwith
        "Our store doesn't have this plant yet. Please try to buy a plant we \
         have."

let check_life plant =
  match plant with
  | Flower { height = _; life = true; hydration = y } ->
      if y > 20 || y < 0 then Flower { height = 0; life = false; hydration = y }
      else plant
  | Peach { height = _; life = true; hydration = y } ->
      if y > 10 || y < 0 then Peach { height = 0; life = false; hydration = y }
      else plant
  | Strawberry { height = _; life = true; hydration = y } ->
      if y > 10 || y < 0 then
        Strawberry { height = 0; life = false; hydration = y }
      else plant
  | Cactus { height = _; life = true; hydration = y } ->
      if y > 5 || y < 0 then Cactus { height = 0; life = false; hydration = y }
      else plant
  | _ -> plant

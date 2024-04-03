type plant_type = {
  height : int;
  hydration : int;
}

type t =
  | Flower of plant_type
  | Peach of plant_type
  | Strawberry of plant_type
  | Cactus of plant_type

let feed plant =
  match plant with
  | Flower { height = x; hydration = h } ->
      Flower { height = x + 1; hydration = h }
  | Peach { height = x; hydration = h } ->
      Peach { height = x + 1; hydration = h }
  | Strawberry { height = x; hydration = h } ->
      Strawberry { height = x + 1; hydration = h }
  | Cactus { height = x; hydration = h } ->
      Cactus { height = x + 1; hydration = h }

let water plant =
  match plant with
  | Flower { height = x; hydration = h } ->
      Flower { height = x; hydration = h + 1 }
  | Peach { height = x; hydration = h } ->
      Peach { height = x; hydration = h + 1 }
  | Strawberry { height = x; hydration = h } ->
      Strawberry { height = x; hydration = h + 1 }
  | Cactus { height = x; hydration = h } ->
      Cactus { height = x; hydration = h + 1 }

let print_plant plant =
  match plant with
  | Flower { height = h; hydration = _ } -> if h < 3 then "🍃" else "🌹"
  | Peach { height = h; hydration = _ } -> if h < 5 then "🌱" else "🍊"
  | Strawberry { height = h; hydration = _ } -> if h < 2 then "🍂" else "🍓"
  | Cactus { height = h; hydration = _ } -> if h < 7 then "🌿" else "🌵"

let create_plant plant_name =
  match plant_name with
  | "Flower" -> Flower { height = 3; hydration = 25 }
  | "Peach" -> Peach { height = 3; hydration = 25 }
  | "Strawberry" -> Strawberry { height = 4; hydration = 25 }
  | "Cactus" -> Cactus { height = 5; hydration = 10 }
  | _ -> failwith "we don't have this plant yet..."

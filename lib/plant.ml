type plant_type = { height : int }

type t =
  | Flower of plant_type
  | Peach of plant_type
  | Strawberry of plant_type
  | Cactus of plant_type

let feed plant =
  match plant with
  | Flower { height = x } -> Flower { height = x + 1 }
  | Peach { height = x } -> Peach { height = x + 1 }
  | Strawberry { height = x } -> Strawberry { height = x + 1 }
  | Cactus { height = x } -> Cactus { height = x + 1 }

let print_plant plant =
  match plant with
  | Flower { height = h } -> if h < 3 then "🍃" else "🌹"
  | Peach { height = h } -> if h < 5 then "🌱" else "🍊"
  | Strawberry { height = h } -> if h < 2 then "🍂" else "🍓"
  | Cactus { height = h } -> if h < 7 then "🌿" else "🌵"

let create_plant plant_name =
  match plant_name with
  | "Flower" -> Flower { height = 3 }
  | "Peach" -> Peach { height = 3 }
  | "Strawberry" -> Strawberry { height = 4 }
  | "Cactus" -> Cactus { height = 5 }
  | _ -> failwith "we don't have this plant yet..."

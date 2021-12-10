type card = 
  {
    id : int;
    description : string;
    effect : string * string;
  }

type t = {
  cards : card array;
}

open Yojson.Basic.Util

let convert_to_card card = {
  id = card |> member "id" |> to_int;
  description = card |> member "description" |> to_string;
  effect = 
    let effect_list = card |> member "effect" |> to_list |> List.map to_string in
    (List.hd effect_list , List.nth effect_list 1)
}

let from_json json = 
  let extracted_cards = 
    json 
    |> member "cards" 
    |> to_list 
    |> List.map convert_to_card 
    |> Array.of_list in
  {
    cards = extracted_cards;
  }

let pick_random_card cards = 
  Random.self_init ();
  cards.cards.((Random.int 27) + 3)

let get_id card = card.id

let get_description card = card.description

let get_effect card = card.effect
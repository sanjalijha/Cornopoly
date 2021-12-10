type tile_id = int

exception UnknownTile of tile_id
type tile = {
  id : tile_id;
  name : string;
  price : int;
  color : string;
  lounge_cost : int;
  rents : int array;
  mutable rent_position : int;
  mortgage : int;
  mutable owner : int;
}
type t = {
  start_tile : tile_id;
  tiles : tile list;
}

open Yojson.Basic.Util

(** [convert_to_tile tile] converts a json representation of tile to 
    a record. It extracts the important fields and sets other fields to 
    their default value. This is a helper function. *)
let convert_to_tile tile = {
  id = tile |> member "id" |> to_int;
  name = tile |> member "name" |> to_string;
  price = tile |> member "price" |> to_int;
  color = tile |> member "color" |> to_string;
  lounge_cost = tile |> member "cost for lounge" |> to_int;
  rents = tile |> member "rents" |> to_list |> List.map to_int |> Array.of_list;
  mortgage = tile |> member "mortgage" |> to_int;
  rent_position = 0;
  owner = -1;
}

let from_json json = 
  let extracted_tiles = json |> member "tiles" |> to_list 
                        |> List.map convert_to_tile in
  let start = json |> member "start tile" |> to_int in
  {
    start_tile = start;
    tiles = extracted_tiles
  }

let start_tile board =
  board.start_tile

let tile_ids board = 
  List.sort_uniq compare (List.map (fun tile -> tile.id) board.tiles)

let rec get_tile_by_id id = function
  | [] -> raise (UnknownTile id)
  | h :: t -> if id = h.id then h else get_tile_by_id id t

let update_owner id lst new_owner = 
  let tile = get_tile_by_id id lst in
  tile.owner <- new_owner

let update_rent id lst = 
  let tile = get_tile_by_id id lst in
  tile.rent_position <- tile.rent_position + 1

let get_tiles board = board.tiles

let get_owner tile = tile.owner

let get_name tile = tile.name

let get_color tile = tile.color

let get_price tile = tile.price

let get_lounge_price tile = tile.lounge_cost

let get_rents tile = tile.rents

let get_mortgage tile = tile.mortgage

let get_rent tile = 
  if tile.id = 0 then 0 else tile.rents.(tile.rent_position)

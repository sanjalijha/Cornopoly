(** 
   Representation of static board data.

   This module represents the data stored in json files pertaining to the board, 
   including the tiles. It handles loading of that data from JSON as well
   as querying the data.
*)

(** The abstract type of values representing boards. *)
type t

(** The type of tile identifiers. *)
type tile_id = int

(** The type of tiles *)
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

(** Raised when an unknown tile is encountered. *)
exception UnknownTile of tile_id

(** [from_json j] is the board that [j] represents.
    Requires: [j] is a valid JSON board representation. *)
val from_json : Yojson.Basic.t -> t

(** [start_tile b] is the identifier of the starting tile in board
    [b]. *)
val start_tile : t -> tile_id

(** [tile_ids b] is a set-like list of all of the tile identifiers in 
    board [b]. *)
val tile_ids : t -> tile_id list

(** [get_tile_by_id t_id lst] is the tile in [lst] with id = [t_id] *)
val get_tile_by_id : tile_id -> tile list -> tile

(** [update_owner t_id lst owner] updates the owner field in tile 
    from id = [t_id] to [owner] *)
val update_owner : int -> tile list -> int -> unit

(** [update_rent t_id lst] updates the rent_position field in tile 
    from rent_position = p to p + 1*)
val update_rent : int -> tile list -> unit

(** [get_tiles b] is the list of tiles in board [b] *)
val get_tiles : t -> tile list

(** [get_owner t] is owner of tile [t] *)
val get_owner : tile -> int

(** [get_name t] is name of tile [t] *)
val get_name : tile -> string

(** [get_color t] is color of tile [t] *)
val get_color : tile -> string

(** [get_price t] is price of tile [t] *)
val get_price : tile -> int

(** [get_lounge_price t] is cost of building a lounge on tile [t] *)
val get_lounge_price : tile -> int

(** [get_rents t] is list of rents on tile [t] *)
val get_rents : tile -> int array

(** [get_mortgage t] is mortagage value of tile [t] *)
val get_mortgage : tile -> int

(** [get_rent t] is the current rent on tile [t] *)
val get_rent : tile -> int

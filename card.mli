(** 
   Representation of static cards data.

   This module represents the data stored in json files pertaning to the 
   monopoly cards, including their description and effect. It handles loading 
   of that data from JSON as well as querying the data.
*)

(** The abstract type of values representing a single card. *)
type card = 
  {
    id : int;
    description : string;
    effect : string * string;
  }

(** The abstract type of values representing all cards. *)
type t = {
  cards : card array;
}

(** [from_json j] is the cards that [j] represents.
    Requires: [j] is a valid JSON card representation. *)
val from_json : Yojson.Basic.t -> t

(** [pick_random_card cards] is a randomly chosen card from [cards] *)
val pick_random_card : t -> card

(** [get_id card] is the indentifier of [card] *)
val get_id : card -> int

(** [get_description card] is the description of [card] *)
val get_description : card -> string

(** [get_effect card] is the effect of [card] *)
val get_effect : card -> string * string
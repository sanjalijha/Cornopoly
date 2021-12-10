(** 
   Representation of dynamic board and player state.
   This module represents the state of a board, card, and player
   and functions that cause the state to change.
*)

(** The abstract type of values representing a player's piece *)
type piece = int

(** The abstract type of values representing a player's current state *)
type player = {
  name : string;
  player_piece : piece;
  mutable money : int;
  mutable position : int;
  mutable properties : (string, (Board.tile_id * int) list) Hashtbl.t;
  mutable num_utilities : int;
  mutable num_jail_free : int;
  mutable num_turns_jail : int;
}

(** The abstract type of values representing a turn in the game state. *)
type turn

(** The abstract type of values representing the game state. *)
type t =
  {
    mutable players : player list;
    mutable game_pieces : piece list; (* can create a set*)
    mutable current_player : piece; (* TODO: player -> piece but piece -/> player *)
    mutable players_in_jail : player list; 
    board : Board.t;
    tiles : Board.tile list;
    cards : Card.t;
  }

(** Raised when an invalid piece is encountered. *)
exception InvalidPiece of piece

(** Raised when an already used piece is encountered. *)
exception UsedPiece of piece

(** Raised when an unkown input for choosing the piece is encountered. *)
exception NotANumPiece of string

(** Raised when a player is not found in the list of players*)
exception PlayerNotFound of piece 

(** [lounge_criteria] is a hashtable representing the mapping from color to 
    the number of cards required to build lounges/ cafes *)
val lounge_criteria : (string, int) Hashtbl.t

(** [new player state name piece] add a new player with [name] and
    [piece] to the list of players in [state] *)
val new_player : t -> string -> int -> t

(** [check_valid state piece] is [piece] if it is valid (not taken by any other
    player or if it is in the allowed pieces). 
    Raises: InavalidPiece of [piece] if [piece] is invalid
*)
val check_valid : t -> string -> piece

(** [init_state board card] initializes and empty state with the Board 
    respresented by [board] and the Cards represented by [card] *)
val init_state : Board.t -> Card.t -> t

(** [get_current_player_name state] is the current player in [state] *)
val get_current_player : t -> player

(** [init_turn state] initializes turn using [state] *)
val init_turn : t -> turn 

(** [play_turn state turn] rolls the dice in [turn] and updates the current 
    player's position in [state] *)
val play_turn : t -> turn -> unit

(** [string_of_turn state turn] prints the information about the dice in [turn]
    and tile, player position in [state] *)
val string_of_turn : t -> turn -> bool -> string 

(** [next_player state] is next player's name. The next player is whose turn 
    it is after current player from the list of players in [state] *)
val next_player : t -> string

(** [buy_tile state] updates [state] such that if a tile is unowned and 
    current player has enough money, the tile's owner becomes the current player
    by reducing player money by the price of tile *)
val buy_tile : t -> unit

(** [update_state state turn] is [turn] with current player in [state] updated
    to a new player *)
val update_state : t -> turn -> turn

(** [pay_rent state] modifies [state] by reducing the rent payer's money and 
    increasing the owner's money by the rent of owner's tile and performing
    all other corressponding state changes *)
val pay_rent : t -> turn -> unit

(** [check_tile_status state] is the string representation of the status of the
    tile. The status for buyable tiles can be "RENT", "UNOWNED", "PLAYER OWNED",
    otherwise it represents what tile the current_player in [state] has landed 
    on eg. "CARD", "SUPER TAX", "INCOME TAX"  *)
val check_tile_status : t -> string

(** [pick_card state] is a randomly chosed card from the list of cards in 
    [state]  *)
val pick_card : t -> Card.card

(** [execute_card state card] performs the effect of [card]. It updates the 
    state of the game based on the effect of the card *)
val execute_card : t -> Card.card -> unit

(** [get_tile_from_turn_tax turn] is the tile when [turn] lands on a 
    tax tile *)
val get_tile_from_turn_tax : turn -> Board.tile

(** [change_player_bank state unit op mult] modifies [state] by 
    and increasing the owner's money by the rent of owner's tile and performing
    all other corressponding state changes 
    Requires: [op] is "+" or "-" *)
val change_player_bank : t -> int -> string -> int -> unit

(** [go_bonus state] add 200 to the current player's money in [state] *)
val go_bonus : t -> unit

(** [check_for_gojf state] is true if current player in [state] has a 
    get out of jail card and false otherwise *)
val check_for_gojf : t -> bool

(** [put_player_jail state] updates [state] by adding current_player 
    to jail *)
val put_player_jail : t -> unit

(** [use_gojf state] updates [state] by removing current_player 
    from jail and removing the player's get out jail free card *)
val use_gojf : t -> unit

(** [use_gojf state] updates [state] by removing current_player 
    from jail and removing the player's get out jail free card *)
val build_lounge : t -> unit

(** [check_player_in_jail state] is true if current player in [state] is 
    in jail, false otherwise *)
val check_player_in_jail : t -> bool

(** [player_in_jail state turn] is the die roll of current player in
    [state] if the player is in jail *)
val play_jail_turn : t -> turn -> string

(** [check_move_jail state turn] is 2 if current_player in [state] 
    is in jail and 1 if current player is out of jail after current [turn] *)
val check_move_jail : turn -> int

(** [get_player_name player] is the name of the [player] *)
val get_player_name : player -> string

(** [print_player_properties properties state] is a string list list
    representation of the color of [properties] and property names a player 
    owns in [state]*)
val print_player_properties : (string, (Board.tile_id * int) list) Hashtbl.t 
  -> t -> string list list

(** [get_player_properties player] is the properties owned by 
    [player]*)
val get_player_properties : player -> (string, (Board.tile_id * int) list)
    Hashtbl.t

(** [get_player_name player] is the money of the [player] *)
val get_player_money : player -> int

(** [check_player_extras player] is true if the [player] has a get out of jail
    card, false otherwise *)
val check_player_extras : player -> bool

(** [get_num_gojf player] is the number of get out of jail free cards [player]
    has *)
val get_num_gojf : player -> int

(** [get_postion_name state player] is the name of the tile [player] is on in 
    [state] *)
val get_position_name : t -> player -> string

(** [get_all_players state] is the list of all players in [state] *)
val get_all_players : t -> player list

(** [remove_player state] modifies [state] by removing current_player
    from players *)
val remove_player : t -> unit

(** [calculate_winner state] decides the winner of the game in [state] 
    once the game ends *)
val calculate_winner : t -> string
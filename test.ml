open OUnit2
open Board
open State
open Card

(* TEST PLAN:

   We plan to test all functions in board.mli, state.mli, and card.mli. 
   The other functions, not in the aforementioned .mli files, but in the 
   corresponding .ml files are helper functions and testing the parent function,
   will automatically test those helper functions.

   We ommitted tests for main.ml since main.ml is just an interface between the 
   state changes and what the user sees when the corresspinding state is 
   modified. Testing board, state, and card ensures the state changes and quering to
   board and cards is correct and main.ml therefore can be tested out by 
   simulating the game, and reading the outputs on the terminal.

   We want to use a modular appraoch for testing, whereby we have a test 
   function for all the testing functions. Test cases were developed using 
   the black-box technique, where we ensure that given the specification, for 
   a particular input, we get the correct output. We then test our functions for 
   an empty or 0 input, a single or 1 input, and then for multiple or 2 inputs, 
   also ensuring that we test all branches of pattern matches and if else
   statements.

   Our test suite therefore, correctly demonstrates the correctness of 
   our program

   Summary of TESTING PLAN:
   Functions automatically tested by OUnit2: functions in board.mli, state.mli,
   and card.mli
   Manually tested: main.ml
   Methodology used: Black-Box testing
   Why is it correct? Every state change used in the game is tested, and the
   game is simulated to check the messages the use sees
*)


(********************************************************************
   Helper functions for testing of set-like lists. 
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(********************************************************************
   End helper functions.
 ********************************************************************)

let cornell = Board.from_json (Yojson.Basic.from_file "cornell.json")
let one_tile = Board.from_json (Yojson.Basic.from_file "oneTile.json")

let start_tile_test
    (name : string)
    (input : Board.t)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (start_tile input))

let tile_ids_test
    (name : string)
    (input : Board.t)
    (expected_output : int list) : test = 
  name >:: (fun _ -> 
      assert_equal true (cmp_set_like_lists expected_output (tile_ids input)))

let get_tile_by_id_test 
    (name : string)
    (t_id : Board.tile_id)
    (t_lst : Board.tile list)
    (expected_output : tile) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_tile_by_id t_id t_lst))

let get_owner_test 
    (name : string)
    (tl : Board.tile)
    (expected_output : int) : test = 
  name >:: (fun _ ->
      let owner = get_owner tl in
      assert_equal expected_output (owner))

let update_owner_test 
    (name : string)
    (t_id : Board.tile_id)
    (t_lst : Board.tile list)
    (owner : int)
  : test = 
  name >:: (fun _ -> 
      update_owner t_id t_lst owner;
      assert_equal true (let tile = get_tile_by_id t_id t_lst in 
                         get_owner tile = owner))

let get_tiles_test 
    (name : string)
    (board : Board.t)
    (expected_output : Board.tile list)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_tiles board))

let get_name_test
    (name : string)
    (tile : tile)
    (expected_output : string)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_name tile))

let get_color_test 
    (name : string)
    (tile : tile)
    (expected_output : string)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_color tile))

let get_price_test 
    (name : string)
    (tile : tile)
    (expected_output : int)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_price tile))

let get_lounge_price_test
    (name : string)
    (tile : tile)
    (expected_output : int)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_lounge_price tile))

let get_rents_test 
    (name : string)
    (tile : tile)
    (expected_output : int array)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_rents tile))

let get_mortgage_test 
    (name : string)
    (tile : tile)
    (expected_output : int)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_mortgage tile))

let get_rent_test 
    (name : string)
    (tile : tile)
    (expected_output : int)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_rent tile))

let tile_ids_cornell = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 
                        16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 
                        30; 31; 32; 33; 34; 35; 36; 37; 38; 39]

let (first_tile: Board.tile) = {
  id = 0;
  name = "GO";
  price = 0;
  color = "SPC";
  lounge_cost = 0;
  rents = [||];
  rent_position = 0;
  mortgage = 0;
  owner = -1;
}

let (card_tile: Board.tile) = {
  id = 7;
  name = "CORNELL CARD";
  price = 0;
  color = "Card";
  lounge_cost = 0;
  rents = [||];
  rent_position = 0;
  mortgage = 0;
  owner = -1;
}

let (train_tile: Board.tile) = {
  id = 15;
  name = "Carpenter Hall Bus Stop";
  price = 200;
  color = "train";
  lounge_cost = 0;
  rents = [|25;50;100;200|];
  rent_position = 0;
  mortgage = 100;
  owner = -1;
}

let (util_tile: Board.tile) = {
  id = 28;
  name = "Cornell IT Services";
  price = 150;
  color = "util";
  lounge_cost = 0;
  rents = [||];
  rent_position = 0;
  mortgage = 75;
  owner = -1;
}

let new_owner_ft = {
  first_tile with 
  owner = 2;
}

let new_owner_ft2 = {
  first_tile with 
  owner = 5;
}

let new_owner_ft3 = {
  first_tile with 
  owner = 9;
}

let new_owner_ft4 = {
  first_tile with 
  owner = 11;
}

let updating_tile = {
  first_tile with 
  owner = 1;
}

let (last_tile: Board.tile) = {
  id = 39;
  name = "Gates Hall";
  price = 400;
  color = "blue";
  lounge_cost = 200;
  rents = [|50;200;600;1400;1700;2000|];
  rent_position = 0;
  mortgage = 200;
  owner = -1;
}

let (only_tile: Board.tile) = {
  id = 0;
  name = "Single tile";
  price = 0;
  color = "colorful";
  lounge_cost = 0;
  rents = [||];
  rent_position = 0;
  mortgage = 0;
  owner = -1;
}

let cornell_tiles = get_tiles cornell
let single_tile_list = get_tiles one_tile

let board_tests =
  [
    (* start_tile test *)
    start_tile_test "start tile_id cornell.json: 0" cornell 0;
    start_tile_test "start tile_id new.json: 0" one_tile 0;

    (* tile_ids test *)
    tile_ids_test "tile ids of cornell.json" cornell tile_ids_cornell;
    tile_ids_test "start tile_id new.json: 0" one_tile [0];

    (* get_tile_by_id test *)
    get_tile_by_id_test "first tile test" 0 cornell_tiles first_tile;
    get_tile_by_id_test "card tile test" 7 cornell_tiles card_tile;
    get_tile_by_id_test "train tile test" 15 cornell_tiles train_tile;
    get_tile_by_id_test "util tile test" 28 cornell_tiles util_tile;
    get_tile_by_id_test "last tile test" 39 cornell_tiles last_tile;
    get_tile_by_id_test "only tile" 0 single_tile_list only_tile;

    (* get_owner test *)
    get_owner_test "owner of an edited cornell tile" new_owner_ft 2;
    get_owner_test "owner of the second edited cornell tile" new_owner_ft2 5;
    get_owner_test "owner of the third edited cornell tile" new_owner_ft3 9;
    get_owner_test "owner of the fourth edited cornell tile" new_owner_ft4 11;
    get_owner_test "owner of only tile" only_tile (-1);

    (* update_owner test *)
    update_owner_test "first tile update owner" 0 cornell_tiles 21;
    update_owner_test "card tile update owner" 7 cornell_tiles 12;
    update_owner_test "train tile update owner" 15 cornell_tiles 8;
    update_owner_test "twenty fourth tile update owner" 23 cornell_tiles 11;
    update_owner_test "thirty second update owner" 31 cornell_tiles 6;
    update_owner_test "last tile update owner" 39 cornell_tiles 2;

    (* get_tiles test *)
    get_tiles_test "one tile list" one_tile [only_tile];

    (* get_name test *)
    get_name_test "get name of first tile" first_tile "GO";
    get_name_test "get name of card tile" card_tile "CORNELL CARD";
    get_name_test "get name of train tile" train_tile "Carpenter Hall Bus Stop";
    get_name_test "get name of util tile" util_tile "Cornell IT Services";
    get_name_test "get name of last tile" last_tile "Gates Hall";
    get_name_test "get name of only tile" only_tile "Single tile";

    (* get_color test *)
    get_color_test "get color of first tile" first_tile "SPC";
    get_color_test "get color of last tile" last_tile "blue";
    get_color_test "get color of only tile" only_tile "colorful";

    (* get_price test *)
    get_price_test "get price of first tile" first_tile 0;
    get_price_test "get price of last tile" last_tile 400;
    get_price_test "get price of only tile" only_tile 0;

    (* get_lounge_price test *)
    get_lounge_price_test "get lounge price of first tile" first_tile 0;
    get_lounge_price_test "get lounge price of last tile" last_tile 200;
    get_lounge_price_test "get lounge price of only tile" only_tile 0;

    (* get_rents test *)
    get_rents_test "rents of first tile" first_tile [||];
    get_rents_test "rents of last tile" last_tile [|50;200;600;1400;1700;2000|];
    get_rents_test "rents of only tile" only_tile [||];

    (* get_mortgage test *)
    get_mortgage_test "get mortgage of first tile" first_tile 0;
    get_mortgage_test "get mortgage of last tile" last_tile 200;
    get_mortgage_test "get mortgage of only tile" only_tile 0;

    (* get_rent test *)
    get_rent_test "get rent of first tile" first_tile 0;
    get_rent_test "get rent of last tile" last_tile 50;
    get_rent_test "get rent of only tile" only_tile 0;
  ]


(* We initalize a few states and keep udating it through the functions to 
   compare it with manual udates on the state, we test all pattern matches
   in the various functions*)

let cornell_cards = Card.from_json (Yojson.Basic.from_file "cornell_cards.json")

let pick_random_card_test
    (name : string)
    (cards : Card.t) : test = 
  name >:: (fun _ -> 
      let card = pick_random_card cards in
      assert_equal true (Array.exists (fun x -> x = card) cards.cards))

let get_id_test
    (name : string)
    (card : Card.card)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_id card))

let get_description_test
    (name : string)
    (card : Card.card)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_description card))

let get_effect_test
    (name : string)
    (card : Card.card)
    (expected_output : string * string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_effect card))

let random_card = {
  id = 52;
  description = "Hellooo";
  effect = ("yes","no");
}

let first_card = cornell_cards.cards.(0)
let fc_description = "You sat in the wrong TCAT! Advance to 'GO', collect 200"

let last_card = cornell_cards.cards.(31)
let lc_description = "Collect 100$"

let card_tests = [
  pick_random_card_test "random test 1" cornell_cards;
  pick_random_card_test "random test 2" cornell_cards;
  pick_random_card_test "random test 3" cornell_cards;
  pick_random_card_test "random test 4" cornell_cards;
  pick_random_card_test "random test 5" cornell_cards;

  get_id_test "first card id" first_card 0; 
  get_id_test "last card id" last_card 31; 
  get_id_test "random card id" random_card 52;

  get_description_test "first card description" first_card fc_description; 
  get_description_test "last card description" last_card lc_description; 
  get_description_test "random card description" random_card "Hellooo";

  get_effect_test "first card effect" first_card ("Pass Go","0"); 
  get_effect_test "last card effect" last_card ("Add","100"); 
  get_effect_test "random card effect" random_card ("yes","no");
]

let init_state_test
    (name : string)
    (board : Board.t)
    (cards : Card.t)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (init_state board cards))

let execute_card_test 
    (name : string)
    (turn : State.turn)
    (expected_output : tile)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_tile_from_turn_tax turn))

let get_tile_from_turn_tax_test 
    (name : string)
    (turn : State.turn)
    (expected_output : tile)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_tile_from_turn_tax turn))

let change_player_bank_test 
    (name : string)
    (state : State.t)
    (unit : int)
    (op : string)
    (mult : int)
    (expected_output : State.t)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (change_player_bank state unit op mult; 
                                    state))

let go_bonus_test 
    (name : string)
    (state : State.t)
    (expected_output : State.t)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (go_bonus state; state))

let check_for_gojf_test 
    (name : string)
    (state : State.t)
    (expected_output : bool)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_for_gojf state))

let put_player_test 
    (name : string)
    (state : State.t)
    (expected_output : State.t)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (put_player_jail state; state))

let use_gojf_test 
    (name : string)
    (state : State.t)
    (expected_output : State.t)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (use_gojf state; state))

let build_lounge_test 
    (name : string)
    (state : State.t)
    (expected_output : State.t)
  : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (build_lounge state; state))

let cornell_cards = Card.from_json (Yojson.Basic.from_file "cornell_cards.json")

let (new_state : State.t) = {
  players = [];
  game_pieces = []; 
  current_player = 0;
  players_in_jail = [];
  board = cornell;
  tiles = Board.get_tiles cornell;
  cards = cornell_cards
}

let patrick = {
  name = "Patrick";
  player_piece = 2;
  money = 10;
  position = 0;
  properties = Hashtbl.create 10;
  num_utilities = 2;
  num_jail_free = 3;
  num_turns_jail = 4;
}

let patrick_updated = {
  name = "Patrick";
  player_piece = 2;
  money = 210;
  position = 0;
  properties = Hashtbl.create 10;
  num_utilities = 2;
  num_jail_free = 3;
  num_turns_jail = 4;
}

let (patrick_after_go: State.t) = {
  players = [patrick_updated];
  game_pieces = []; 
  current_player = 2;
  players_in_jail = [];
  board = cornell;
  tiles = Board.get_tiles cornell;
  cards = cornell_cards
}

let (patrick_state : State.t) = {
  players = [patrick];
  game_pieces = []; 
  current_player = 2;
  players_in_jail = [];
  board = cornell;
  tiles = Board.get_tiles cornell;
  cards = cornell_cards
}

let john = {
  name = "John";
  player_piece = 3;
  money = 100;
  position = 0;
  properties = Hashtbl.create 10;
  num_utilities = 0;
  num_jail_free = 0;
  num_turns_jail = 0;
}

let john_after_reduction = {
  name = "John";
  player_piece = 3;
  money = 0;
  position = 0;
  properties = Hashtbl.create 10;
  num_utilities = 0;
  num_jail_free = 0;
  num_turns_jail = 0;
}

let (john_state : State.t) = {
  players = [john];
  game_pieces = []; 
  current_player = 3;
  players_in_jail = [];
  board = cornell;
  tiles = Board.get_tiles cornell;
  cards = cornell_cards
}

let (john_state_reduced : State.t) = {
  players = [john_after_reduction];
  game_pieces = []; 
  current_player = 3;
  players_in_jail = [];
  board = cornell;
  tiles = Board.get_tiles cornell;
  cards = cornell_cards
}

let henry = {
  name = "Henry";
  player_piece = 3;
  money = 100;
  position = 0;
  properties = Hashtbl.create 10;
  num_utilities = 0;
  num_jail_free = 0;
  num_turns_jail = 0;
}


let (henry_state : State.t) = {
  players = [henry];
  game_pieces = []; 
  current_player = 3;
  players_in_jail = [];
  board = cornell;
  tiles = Board.get_tiles cornell;
  cards = cornell_cards
}

let henry_changed_bank = {
  name = "Henry";
  player_piece = 3;
  money = 190;
  position = 0;
  properties = Hashtbl.create 10;
  num_utilities = 0;
  num_jail_free = 0;
  num_turns_jail = 0;
}

let (henry_changed_bank_state : State.t) = {
  players = [henry_changed_bank];
  game_pieces = []; 
  current_player = 3;
  players_in_jail = [];
  board = cornell;
  tiles = Board.get_tiles cornell;
  cards = cornell_cards
}

let (henry_changed_bank_state : State.t) = {
  players = [henry_changed_bank];
  game_pieces = []; 
  current_player = 3;
  players_in_jail = [];
  board = cornell;
  tiles = Board.get_tiles cornell;
  cards = cornell_cards
}

let state_tests = [
  (* init_state test *)
  init_state_test "initialize a new state" cornell cornell_cards new_state;

  (*  go_bonus test *)
  go_bonus_test "patrick gets bonus" patrick_state patrick_after_go;

  (* check_for_gojf test *)
  check_for_gojf_test "patrick has a gojf" patrick_state true;
  check_for_gojf_test "patrick has a gojf" henry_state false;

  (* change player bank test *)
  change_player_bank_test "add 90 to henry" 
    henry_state 90 "+" 1 henry_changed_bank_state;
  change_player_bank_test "subtract 100 from john"
    john_state 100 "-" 1 john_state_reduced;
]

let suite =
  "test suite for MS3"  >::: List.flatten [
    board_tests;
    state_tests;
    card_tests;
  ]

let _ = run_test_tt_main suite
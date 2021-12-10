type piece = int

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
type info = { 
  color : string;
  player : player;
  dice1 : string;
  dice2 : string;
  plr : string;
  landed : string;
  cost : string;
  m : string;
  rents : int array;
  cl : int;
  jail : string;
}

exception InvalidPiece of piece
exception UsedPiece of piece
exception NotANumPiece of string
exception PlayerNotFound of piece 
exception CannotBuyTile of piece
exception NotEnoughMoney of piece
exception CannotSellTile of piece
exception CannotBuild of piece
exception NotEnoughProperties of piece

let lounge_criteria = Hashtbl.create 10

let () = Hashtbl.add lounge_criteria "brown" 2
let () = Hashtbl.add lounge_criteria "light blue" 3
let () = Hashtbl.add lounge_criteria "pink" 3
let () = Hashtbl.add lounge_criteria "orange" 3
let () = Hashtbl.add lounge_criteria "red" 3
let () = Hashtbl.add lounge_criteria "yellow" 3
let () = Hashtbl.add lounge_criteria "green" 3
let () = Hashtbl.add lounge_criteria "blue" 2
let () = Hashtbl.add lounge_criteria "util" 2
let () = Hashtbl.add lounge_criteria "train" 4
let () = Hashtbl.add lounge_criteria "SPC" 4

let add_player n p : player = 
  let properties = Hashtbl.create 12 in
  Hashtbl.add properties "brown" [];
  Hashtbl.add properties "light blue" [];
  Hashtbl.add properties "pink" [];
  Hashtbl.add properties "orange" [];
  Hashtbl.add properties "red" [];
  Hashtbl.add properties "yellow" [];
  Hashtbl.add properties "green" [];
  Hashtbl.add properties "blue" [];
  Hashtbl.add properties "util" [];
  Hashtbl.add properties "train" [];
  Hashtbl.add properties "SPC" [];
  Hashtbl.add properties "card" [];
  {
    name = n;
    player_piece = p;
    money = 1;
    position = 0;
    properties = properties;
    num_utilities = 0;
    num_jail_free = 0;
    num_turns_jail = 0;
  }

type t = {
  mutable players : player list;
  mutable game_pieces : piece list; 
  mutable current_player : piece; 
  mutable players_in_jail : player list; 
  board : Board.t;
  tiles : Board.tile list;
  cards : Card.t;
}

let init_state board cards : t = 
  {
    players = [];
    game_pieces = []; 
    current_player = 0;
    players_in_jail = [];
    board = board;
    tiles = Board.get_tiles board;
    cards = cards
  }

type turn = {
  mutable die_1 : int;
  mutable die_2 : int;
  mutable position : int;
  mutable tile : Board.tile;
  mutable piece : piece;
}

let init_turn state : turn = {
  die_1 = 0;
  die_2 = 0;
  position = 0;
  tile = Board.get_tile_by_id 0 state.tiles;
  piece = 0;
}

let check_used_piece pieces p = 
  if List.mem p pieces then raise (UsedPiece p) else p

let check_if_num p  = 
  try 
    int_of_string p 
  with Failure _ -> raise (NotANumPiece p)

let check_valid_piece p = 
  if p > 0 && p < 7 then p else raise (InvalidPiece p)

let check_valid state piece = 
  piece 
  |> check_if_num 
  |> check_valid_piece 
  |> check_used_piece state.game_pieces

let new_player state name piece = 
  let player = add_player name piece in
  state.players <- state.players @ [player];
  state.game_pieces <- state.game_pieces @ [piece];
  state.current_player <- piece; 
  state

let check_move_jail turn = 
  if Board.get_color turn.tile = "Jail" then 2 else 1

let get_tile_from_turn_tax turn = turn.tile

let get_player_name player = player.name

let find_player_by_piece piece players = 
  try 
    List.find (fun p -> p.player_piece = piece) players
  with Failure _ -> raise (PlayerNotFound piece)

let find_player_name_by_piece piece players =
  (find_player_by_piece piece players).name

let get_current_player state = 
  find_player_by_piece state.current_player state.players

let get_all_players state = state.players

let die_roll () = 
  Random.self_init ();
  let dice1 = 2 in
  let dice2 = 2 in 
  (dice1, dice2)

let rec find_next_piece piece hd pieces = 
  match pieces with 
  | [] -> piece
  | h :: [] -> hd
  | h :: (a :: t) -> if h = piece then 
      a else find_next_piece piece hd (a :: t)

let next_player state = 
  let piece = find_next_piece state.current_player
      (List.hd state.game_pieces) state.game_pieces in
  find_player_name_by_piece piece state.players

let go_bonus state =
  let cur_player = find_player_by_piece state.current_player state.players in 
  cur_player.money <- cur_player.money + 200;
  print_string "You passed GO! 200 was added to your account, you have ";
  print_int cur_player.money;
  print_endline " now."

let check_player_in_jail state  = 
  let cur_player = find_player_by_piece state.current_player state.players in 
  List.exists (fun x -> x = cur_player) state.players_in_jail

let get_player_properties player = player.properties

let get_player_money player = player.money

let check_player_extras player = 
  if player.num_jail_free > 0 then true else false

let get_num_gojf player = player.num_jail_free
let play_turn state turn = 
  let cur_player = find_player_by_piece state.current_player state.players in
  let position = cur_player.position in
  let roll = die_roll () in
  turn.die_1 <- fst roll;
  turn.die_2 <- snd roll;
  turn.position <- (position + turn.die_1 + turn.die_2) mod 40;
  turn.piece <- state.current_player;
  turn.tile <- Board.get_tile_by_id turn.position state.tiles;
  if turn.position < position then go_bonus state;
  cur_player.position <- turn.position

let update_state state turn = 
  state.current_player <- find_next_piece state.current_player 
      (List.hd state.game_pieces) state.game_pieces;
  turn

let get_out_of_jail_reason turn = 
  if turn.die_1 = turn.die_2 then 
    "You rolled the same on both die! You're out of Jail" else
    "Your 3 turns are up - You're out of jail!" 

let get_tile_info state turn : info = 
  let player = find_player_by_piece turn.piece state.players in
  let jail_reason = get_out_of_jail_reason turn in
  {
    color =  Board.get_color turn.tile;
    player = player;
    dice1 = "Die Roll 1 : " ^ string_of_int turn.die_1;
    dice2 =  "Die Roll 2 : " ^ string_of_int turn.die_2;
    plr = player.name ^ " Moves " ^ string_of_int (turn.die_1 + turn.die_2) 
          ^ " steps forward";
    landed = "Landed on " ^ Board.get_name turn.tile;
    cost = "Cost : $" ^ string_of_int (Board.get_price turn.tile);
    m = "Cost of Mortgage : $" ^ string_of_int 
          (Board.get_mortgage turn.tile);
    rents = Board.get_rents turn.tile;
    cl = Board.get_lounge_price turn.tile;
    jail = jail_reason;
  }

let string_of_turn_util info jail =
  let die_string = info.dice1 ^ "\n" ^ info.dice2 ^ "\n\n" in
  if jail then 
    info.jail ^ "\n" ^ info.plr ^ "\n" ^ info.landed ^ "\n\n" ^ info.cost 
  else 
    let next_string = info.plr ^ "\n" ^ info.landed ^ "\n\n" ^ info.cost 
    in die_string ^ next_string

let string_of_turn_train info jail = 
  let r = "\nRent with 1 Bus Stop: $" ^ string_of_int info.rents.(0) in
  let r1 = "\nRent with 2 Bus Stops: $" ^ string_of_int info.rents.(1) in
  let r2 = "\nRent with 3 Bus Stops: $" ^ string_of_int info.rents.(2) in
  let r3 = "\nRent with 4 Bus Stops: $" ^ string_of_int info.rents.(3) in
  let rents = r ^ r1 ^ r2 ^ r3 in 
  let m_str = "\n\n" ^ info.plr ^ "\n" ^ info.landed ^ "\n\n" ^ info.cost ^ 
              rents in
  if jail then 
    info.dice1 ^ "\n" ^ info.dice2 ^ "\n\n" ^ info.jail ^ m_str
  else info.dice1 ^ "\n" ^ info.dice2 ^ m_str

let string_of_turn_color info jail = 
  let r = "Rent : $" ^ string_of_int info.rents.(0) in
  let r1 = "\nRent with 1 Lounge : $" ^ string_of_int info.rents.(1) in
  let r2 = "\nRent with 2 Lounges : $" ^ string_of_int info.rents.(2) in
  let r3 = "\nRent with 3 Lounges : $" ^ string_of_int info.rents.(3) in
  let cl = "\n\nCost of Lounge : $" ^ string_of_int info.cl in
  let rents = r ^ r1  ^ r2 ^ r3 ^ cl ^ "\n"  in 
  let m_str = "\n\n" ^ info.plr ^ "\n" ^ info.landed ^ "\n\n" ^ info.cost ^ "\n" in 
  if jail then 
    info.dice1 ^ "\n" ^ info.dice2 ^ "\n\n" ^ info.jail ^ m_str ^ rents 
  else info.dice1 ^ "\n" ^ info.dice2 ^ m_str ^ rents

let string_of_turn_simple info jail = 
  let die_string = info.dice1 ^ "\n" ^ info.dice2 ^ "\n\n" in
  if jail then 
    info.jail ^ "\n" ^ info.plr ^ "\n" ^ info.landed
  else let next_string = info.plr ^ "\n" ^ info.landed in die_string ^ next_string

let string_of_turn state turn jail = 
  let info = get_tile_info state turn in
  if info.color = "SPC" then (string_of_turn_simple info jail) else
  if info.color = "util" then (string_of_turn_util info jail) else
  if info.color = "train" then  (string_of_turn_train info jail) else
  if info.color = "Card" then (string_of_turn_simple info jail) else
  if info.color = "Free Parking" then (string_of_turn_simple info jail) else
  if info.color = "Non-Jail" then (string_of_turn_simple info jail) else
  if info.color = "Jail" then (string_of_turn_simple info jail) else
  if info.color = "Super Tax" || info.color = "Income Tax" then 
    (string_of_turn_simple info jail)
  else string_of_turn_color info jail

let check_tile_status (state : t) =
  let piece = state.current_player in
  let player = find_player_by_piece piece state.players in
  let position = player.position in
  let tile = Board.get_tile_by_id position state.tiles in 
  let color = Board.get_color tile in
  if (color = "Card") then "CARD"
  else if (color = "Jail") then "JAIL"
  else if (color = "Non-Jail") then "NON-JAIL"
  else if (color = "SPC") then "GO"
  else if (color = "Free Parking") then "FREE"
  else if (color = "Income Tax") then "INCOME TAX"
  else if (color = "Super Tax") then "SUPER TAX"
  else
    let owner = Board.get_owner tile in
    (* let player_bank = player.money in *)
    if owner = -1 then "UNOWNED" else 
    if owner <> piece then "RENT" else 
    if color = "util" || color = "train" then 
      "PLAYER OWNED T/U" else "PLAYER OWNED"

let reset_owner prop_lst state = 
  List.iter (fun x -> (Board.update_owner (fst x) state.tiles (-1))) prop_lst

let rec remove_from_list player players updated = 
  match players with 
  | [] -> updated
  | h :: t -> if h.player_piece = player.player_piece then updated @ t else
      (remove_from_list player t (h :: updated))

let rec remove_piece_from_list piece pieces updated = 
  match pieces with 
  | [] -> updated
  | h :: t -> if h = piece then updated @ t else
      (remove_piece_from_list piece t (h :: updated))

let rec max_money max pl = function
  | [] -> pl
  | h :: t -> if h.money > max then max_money h.money h t else max_money max pl t

let calculate_winner state = 
  let cur_player = find_player_by_piece state.current_player state.players in 
  let player = max_money 0 cur_player state.players in player.name

let remove_player state = 
  let player = get_current_player state in
  Hashtbl.iter (fun k v -> reset_owner v state) player.properties;
  let updated_players = remove_from_list player state.players [] in
  let updated_game_pieces =  remove_piece_from_list player.player_piece 
      state.game_pieces [] in
  state.players <- updated_players;
  state.game_pieces <- updated_game_pieces

let pay_util_rent player tile turn state =
  let owner = Board.get_owner tile in
  let player_bank = player.money in
  let owner_player = find_player_by_piece owner state.players in
  print_endline ("You need to pay rent to " ^ owner_player.name ^ "\n");
  print_string ("The rent is 10 times your die roll - ");
  let rent = (turn.die_1 + turn.die_2) * 10 in
  print_int rent;
  if player.money < rent then
    begin
      print_endline "You do not have enough money! Sorry GAME OVER";
      remove_player state;
      if List.length (get_all_players state) = 0 then 
        begin
          print_endline  "Game Over :)";
          match read_line () with
          | _ -> exit 0 end end
  else begin
    player.money <- player_bank - rent;
    owner_player.money <- owner_player.money + rent;
    print_endline ("\nYou have " ^ string_of_int player.money ^  " left") end

let pay_train_rent player tile state =
  let owner = Board.get_owner tile in
  let player_bank = player.money in
  let rent = Board.get_rent tile in
  let owner_player = find_player_by_piece owner state.players in
  let property_list = Hashtbl.find owner_player.properties "train" in
  let num_trains = List.length (property_list) in
  let total_rent = rent * num_trains in
  print_endline ("You need to pay rent to " ^ owner_player.name ^ "\n");
  print_string ("The rent is ");
  print_int total_rent;
  if player.money < rent then
    begin
      print_endline "You do not have enough money! Sorry GAME OVER";
      remove_player state;
      if List.length (get_all_players state) = 0 then 
        begin
          print_endline  "Game Over :)";
          match read_line () with
          | _ -> exit 0 end end
  else
    begin
      player.money <- player_bank - rent;
      owner_player.money <- owner_player.money + rent;
      print_endline ("\nYou have " ^ string_of_int player.money ^  " left")
    end

let buy_tile (state : t) = 
  let piece = state.current_player in
  let player = find_player_by_piece piece state.players in
  let position = player.position in
  let tile = Board.get_tile_by_id position state.tiles in
  let price = Board.get_price tile in 
  let owner = Board.get_owner tile in
  let color = Board.get_color tile in
  let player_bank = player.money in
  if owner <> -1 then raise (CannotBuyTile piece) else 
  if player_bank < price then 
    print_endline "Oops not enough money!" 
  else if color = "SPC" then raise (CannotBuyTile piece) else
    let updated_list = (position, 0) :: 
                       (Hashtbl.find player.properties color) in
    if color = "util" then player.num_utilities <- player.num_utilities + 1; 
    Hashtbl.replace player.properties color updated_list;
    player.money <- player_bank - price;
    Board.update_owner position state.tiles piece;
    print_endline ("You have bought " ^ Board.get_name tile ^ " \n");
    print_string "You have $";
    print_int player.money;
    print_endline " left."

let sell_tile (state : t) (id : Board.tile_id) =
  let piece = state.current_player in
  let player = find_player_by_piece piece state.players in
  let position = player.position in
  let tile = Board.get_tile_by_id id state.tiles in
  let mortgage = Board.get_mortgage tile in 
  let owner = Board.get_owner tile in
  let color = Board.get_color tile in
  let player_bank = player.money in
  if owner <> piece then raise (CannotSellTile piece) else 
    begin
      let updated_list = List.remove_assoc position 
          (Hashtbl.find player.properties color) in
      if color = "util" then player.num_utilities <- player.num_utilities - 1; 
      Hashtbl.replace player.properties color updated_list;
      player.money <- player_bank + mortgage;
      Board.update_owner (- 1) state.tiles piece;
      print_endline ("You have sold " ^ Board.get_name tile ^ " \n");
      print_string "You have ";
      print_int player.money end

let build_lounge_suceed state position player property_list color piece tile =
  let num_lounges = List.assoc position (property_list) in
  let old_list = List.remove_assoc position (property_list) in
  let updated_list = (position, num_lounges + 1) :: (old_list) in
  let price = Board.get_lounge_price tile in 
  Hashtbl.replace player.properties color updated_list;
  player.money <- player.money - price;
  Board.update_owner position state.tiles piece;
  Board.update_rent position state.tiles;
  print_endline "You successfully built a lounge!"

let build_lounge_fail state color player tile position piece =
  let price = Board.get_lounge_price tile in 
  let player_bank = player.money in
  if player_bank < price then print_endline "Oops not enough money" else
    let property_list = Hashtbl.find player.properties color in
    let num_lounges = List.assoc position (property_list) in
    if num_lounges > 3 then
      print_endline "You have built the maximum number of lounges on this 
      property. You cannot build more lounges"
    else
    if player.money < price then 
      begin
        print_endline "You do not have enough money! Sorry GAME OVER";
        remove_player state;
        if List.length (get_all_players state) = 0 then 
          begin
            print_endline  "Game Over :)";
            match read_line () with
            | _ -> exit 0 end end
    else 
      build_lounge_suceed state position player property_list color piece tile

let build_lounge state = 
  let piece = state.current_player in
  let player = find_player_by_piece piece state.players in
  let position = player.position in
  let tile = Board.get_tile_by_id position state.tiles in
  let color = Board.get_color tile in
  let owner = Board.get_owner tile in
  if owner <> piece then raise (CannotBuild piece) else 
  if List.length (Hashtbl.find player.properties color) <> 
     Hashtbl.find lounge_criteria color then 
    print_endline "You do not have all the cards in this color set! 
  You cannot build a lounge at this time" else 
    build_lounge_fail state color player tile position piece

let check_pay_rent_pass state player owner_player rent = 
  let player_bank = player.money in
  if player.money < rent then 
    begin
      print_endline "You do not have enough money! Sorry GAME OVER";
      remove_player state;
      if List.length (get_all_players state) = 0 then 
        begin
          print_endline  "Game Over :)";
          match read_line () with
          | _ -> exit 0 end end
  else 
    begin
      player.money <- player_bank - rent;
      owner_player.money <- owner_player.money + rent;
      print_endline ("\nYou have " ^ string_of_int player.money ^  " left")
    end

let pay_rent (state : t) turn =
  print_endline "pay rent";
  let piece = state.current_player in
  let player = find_player_by_piece piece state.players in
  let position = player.position in
  let tile = Board.get_tile_by_id position state.tiles in 
  if (Board.get_color tile) = "util" then 
    pay_util_rent player tile turn state 
  else if (Board.get_color tile) = "train" then
    pay_train_rent player tile state  
  else
    let owner = Board.get_owner tile in
    let rent = Board.get_rent tile in
    let owner_player = find_player_by_piece owner state.players in
    print_endline ("You need to pay rent to " ^ owner_player.name ^ "\n");
    print_string ("The rent is ");
    print_int rent;
    check_pay_rent_pass state player owner_player rent

let execute_change_p cur_player state m mult action = 
  if (cur_player.money < (m * mult) && action = "-" )then 
    begin
      print_endline "You do not have enough money! Sorry GAME OVER";
      remove_player state;
      if List.length (get_all_players state) = 0 then 
        begin
          print_endline  "Game Over :)";
          match read_line () with
          | _ -> exit 0 end end
  else 
    begin
      cur_player.money <- cur_player.money - m * mult;
      print_string "You have $";
      print_int cur_player.money;
      print_endline " left."; 
    end

let change_player_bank state m action mult = 
  let cur_player = find_player_by_piece state.current_player state.players in 
  if action = "+" then 
    begin
      cur_player.money <- cur_player.money + m * mult;
      print_string "You have $";
      print_int cur_player.money;
      print_endline " now."; 
    end
  else execute_change_p cur_player state m mult action 

let rec change_all_bank state m action = 
  let cur_player = find_player_by_piece state.current_player state.players in 
  let players = state.players in 
  let others = List.filter (fun x -> x <> cur_player) players in 
  if action = "+" then
    List.iter (fun p -> p.money <- p.money + m;) others else
    List.iter (fun p -> p.money <- p.money - m;) others

let check_for_gojf state =
  let cur_player = find_player_by_piece state.current_player state.players in 
  if cur_player.num_jail_free > 0 then true else false

let put_player_jail state =   
  let cur_player = find_player_by_piece state.current_player state.players in 
  state.players_in_jail <- cur_player :: state.players_in_jail;
  cur_player.position <- 10

let use_gojf state =  
  let cur_player = find_player_by_piece state.current_player state.players in 
  cur_player.num_jail_free <- cur_player.num_jail_free - 1

let pick_card state = Card.pick_random_card state.cards

let add_gojf state =
  let cur_player = find_player_by_piece state.current_player state.players in 
  cur_player.num_jail_free <- cur_player.num_jail_free + 1

let execute_card state card = 
  let effect = Card.get_effect card in
  match effect with
  | ("Add", m) -> change_player_bank state (int_of_string m) "+" 1
  | ("Add Each", m) -> 
    let mult = (List.length state.players) - 1 in 
    change_player_bank state (int_of_string m) "+" mult;
    change_all_bank state (int_of_string m) "-";
  | ("Sub", m) -> change_player_bank state (int_of_string m) "-" 1
  | ("Sub Each", m) ->  
    let mult = (List.length state.players) - 1 in 
    change_player_bank state (int_of_string m) "-" mult;
    change_all_bank state (int_of_string m) "+";
  | ("Pass Go", dest) -> 
    let cur_player = find_player_by_piece state.current_player state.players in 
    cur_player.position <- int_of_string dest;
    go_bonus state;
  | ("Card", m) -> add_gojf state;
  | _ -> failwith "Please Restart Game"

let remove_player_jail state player = 
  List.filter (fun x -> x <> player) state.players_in_jail

let play_jail_turn state turn = 
  print_endline "in jail turn";
  let cur_player = find_player_by_piece state.current_player state.players in
  cur_player.num_turns_jail <- cur_player.num_turns_jail + 1;
  let position = cur_player.position in
  let roll = die_roll () in
  turn.die_1 <- fst roll;
  turn.die_2 <- snd roll;
  if fst roll = snd roll || cur_player.num_turns_jail = 3 then 
    begin
      state.players_in_jail <- remove_player_jail state cur_player;
      turn.position <- (position + turn.die_1 + turn.die_2) mod 40;
      turn.piece <- state.current_player;
      turn.tile <- Board.get_tile_by_id turn.position state.tiles;
      cur_player.position <- turn.position;
      string_of_turn state turn true end
  else 
    let  dice1 = "\nDie Roll 1 : " ^ string_of_int turn.die_1 in
    let dice2 =  "\nDie Roll 2 : " ^ string_of_int turn.die_2 in
    dice1 ^ dice2 ^ "\n\n" ^
    "You did not get the same number on both die.\nRemain in Jail"

let rec add_properties 
    (elem : (Board.tile_id * int) list)
    (ret_list : string list)
    (state : t) = 
  match elem with
  | [] -> ret_list
  | h :: t -> 
    let name = Board.get_tile_by_id (fst h) state.tiles |> Board.get_name in 
    add_properties t (ret_list @ [name]) state

let rec parse 
    (lst : (string * (Board.tile_id * int) list) list)
    (new_lst : string list list)
    (state : t) = 
  match lst with
  | [] -> new_lst
  | h :: t -> begin 
      if List.length (snd h) > 0 then
        let updated_lst = (add_properties (snd h) [fst h] state) in 
        parse t (new_lst @ [updated_lst]) state
      else
        parse t new_lst state
    end

let print_player_properties 
    (properties : (string, (Board.tile_id * int) list) Hashtbl.t)
    (state : t) = 
  let lst = properties |> Hashtbl.to_seq |> List.of_seq in
  parse lst [] state

let get_position_name state (player : player) = 
  let tile = Board.get_tile_by_id player.position state.tiles in  
  (Board.get_name tile)


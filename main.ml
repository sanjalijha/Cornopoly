let rec string_of_list i = function
  | [] -> ""
  | [h] -> h 
  | h :: t -> 
    if i = 0 then
      " " ^ h ^ " - " ^ string_of_list (i + 1) t
    else
      h ^ ", " ^ string_of_list (i + 1) t

let rec string_of_list_list = function
  | [] -> ""
  | h :: t -> (string_of_list 0 h) ^ "\n" ^ string_of_list_list t

let rec play_turn state turn = 
  print_endline "\nEnter 'Roll' to roll the die, 'Check' to check your state\ 
  or 'Game' to view game state, or 'Leave' to leave game, enter 'Quit' to end game";
  print_string "> ";
  match (read_line ()) with
  | "Roll" | "roll" -> check_player_in_jail state turn
  | "Check" | "check" -> check_player_status state turn "ROLL"
  | "Game" | "game" -> check_game_status state turn "ROLL"
  | "Leave" | "leave" -> State.remove_player state; 
    print_endline "You have been removed! Thanks for playing";
    if List.length (State.get_all_players state) = 0 then 
      begin
        print_endline  "Game Over :)";
        match read_line () with
        | _ -> exit 0 end
    else game_play state turn 2
  | "Quit" | "quit" -> print_endline "Thanks for playing";
    print_string (State.calculate_winner state);
    print_endline " WON!";
    ignore (exit 0) ; () 
  | _ -> 
    print_endline "Oops! You did not enter a valid command. Please try again";
    game_play state turn 0

and jump_to_turn state turn = function
  | "ROLL" -> play_turn state turn
  | "UNOWNED" -> land_unowned state turn
  | _ -> failwith "add mpre callback functions"

and print_player_status player state = 
  ANSITerminal.(print_string [red] (("\n" ^ State.get_player_name player) ^ "'s State :" ));
  let money = string_of_int (State.get_player_money player) in 
  print_string "\nWallet : $";
  print_endline money;
  print_string "Location : ";
  print_endline (State.get_position_name state player);
  print_endline "Properties:";
  let properties = State.get_player_properties player in
  let properties_print = State.print_player_properties properties state in
  print_endline (string_of_list_list properties_print);
  print_string (string_of_int (State.get_num_gojf player));
  print_endline " Get Out Of Jail Free Cards";

and check_player_status state turn called_by = 
  let cur_player = State.get_current_player state in 
  print_player_status cur_player state;
  jump_to_turn state turn called_by

and print_all_players_status state turn called_by = function
  | [] -> jump_to_turn state turn called_by;
  | h :: t -> print_player_status h state;
    print_all_players_status state turn called_by t

and check_game_status state turn called_by = 
  let players = State.get_all_players state in 
  print_all_players_status state turn called_by players

and check_player_in_jail state turn = 
  let jail = State.check_player_in_jail state in 
  if jail then 
    begin
      print_endline "You're in Jail";
      print_endline ("\n" ^ State.play_jail_turn state turn;);
      game_play state turn (State.check_move_jail turn)
    end else begin
    State.play_turn state turn;
    print_endline ("\n" ^ State.string_of_turn state turn jail);
    game_play state turn 1
  end

and player_action state turn = 
  match (State.check_tile_status state) with
  | "UNOWNED" -> land_unowned state turn
  | "PLAYER OWNED" -> land_player_owned state turn
  | "PLAYER OWNED T/U" -> land_player_owned_tu state turn
  | "RENT" -> land_rent state turn
  | "CARD" -> land_card state turn
  | "JAIL" -> land_jail state turn
  | "NON-JAIL" -> land_visiting_jail state turn
  | "GO" -> land_go state turn
  | "FREE" -> land_free state turn
  | "INCOME TAX" | "SUPER TAX" -> land_tax state turn
  | _ -> failwith "There is some error! Please restart the game"

and land_player_owned state turn = 
  print_endline "\nYou own this tile, you can build a lounge if you have a set of this color!";
  print_endline "Type Build to build a lounge on this tile or Pass to continue";
  print_endline ">";
  match (read_line ()) with
  | "Build" | "build" -> 
    State.build_lounge state;
    game_play state turn 2
  | "Pass" | "pass" -> game_play state turn 2
  | _ -> print_endline "Sorry please type your command again"; 
    land_player_owned state turn

and land_rent state turn = 
  print_string "\nUh Oh! ";
  State.pay_rent state turn;
  game_play state turn 2

and land_card state turn = 
  print_endline "\nYour Cornell Card says -";
  let card = (State.pick_card state) in
  ANSITerminal.(print_string [red]
                  ("\n" ^ Card.get_description card^ "\n")) ;
  State.execute_card state card;
  game_play state turn 2

and land_player_owned_tu state turn = 
  print_endline "\nYou own this property - Enjoy!";
  game_play state turn 2

and land_visiting_jail state turn = 
  print_endline "\nYou're just visiting Jail!";
  game_play state turn 2

and land_jail state turn = 
  print_string "\nOh no! You're in";
  ANSITerminal.(print_string [yellow] (" Jail"^ "\n")) ;
  if (State.check_for_gojf state) then 
    begin
      print_endline "But you have a Get Out Of Jail Free Card - Would you like to use it?";
      gojf_present state turn
    end
  else 
    begin
      print_endline "You can either pay $50 and get out now or stay for 3 turns.";
      gojf_not_present state turn
    end

and gojf_not_present state turn =
  print_endline "\nEnter 'Pay' to pay or 'Stay' to remain in Jail";
  match (read_line ()) with
  | "Stay" | "stay" -> State.put_player_jail state;
    game_play state turn 2
  | "Pay" | "pay" ->  State.change_player_bank state 50 "-" 1;
    print_endline "You start your next turn from the Jail Tile";
    game_play state turn 2
  | _ -> print_endline "Oops! Please enter a valid command.";
    gojf_present state turn

and gojf_present state turn =
  print_endline "Enter 'Use' to use your card or 'Stay' to remain in Jail";
  match (read_line ()) with
  | "Stay" | "stay" -> State.put_player_jail state;
    (game_play state turn 2)
  | "Use" | "use" -> State.use_gojf state;
    print_endline "\nYou used your Card. You're out of Jail!";
    (game_play state turn 2)
  | _ -> print_endline "Oops! Please enter a valid command.";
    gojf_not_present state turn

and land_go state turn = 
  print_endline "\nYou received a GO Bonus!";
  State.go_bonus state;
  game_play state turn 2

and land_free state turn = 
  print_endline "\nYou are in Free Parking";
  print_endline "\nEnjoy doing nothing!";
  game_play state turn 2

and land_tax state turn = 
  print_string "\nYou have to pay $";
  let tile = State.get_tile_from_turn_tax turn in 
  let tax_amnt = Board.get_price tile in
  print_string ((string_of_int tax_amnt) ^ "\n");
  State.change_player_bank state tax_amnt "-" 1;
  game_play state turn 2

and land_unowned state turn = 
  print_endline "\nType 'Buy' to Buy the tile 'Pass' to pass 'Check' to check your status or 'Game' to view game status";
  print_string "> ";
  match (read_line ()) with
  | "Buy" | "buy" -> State.buy_tile state;
    game_play state turn 2
  | "pass" | "Pass" ->
    print_endline ("\n" ^ State.string_of_turn state turn false);
    game_play state turn 2
  | "Check" | "check" -> check_player_status state turn "UNOWNED"
  | "Game" | "game" -> check_game_status state turn "UNOWNED"
  | _ -> 
    print_endline "Oops! You did not enter a valid command. Please try again";
    land_unowned state turn

and game_play state (turn : State.turn) = function
  | 0 -> play_turn state turn;
  | 1 -> player_action state turn;
  | 2 -> ANSITerminal.(print_string [blue]
                         ("\n" ^ (State.next_player state) ^ "'s Turn!")); 
    let turn = (State.update_state state turn) in game_play state turn 0 
  | _ -> failwith "cannot reach here"

let game_start state = 
  let cur_player = State.get_current_player state in
  let cur_player_name = State.get_player_name cur_player in
  ANSITerminal.(print_string [blue]
                  ("\n" ^ cur_player_name ^ " goes first"));
  print_endline " - Roll the die to start the game!";
  let turn = State.init_turn state in
  game_play state turn 0

let match_all_string name = 
  print_string  "Hi ";
  print_string name;
  print_endline "! Enter the number of your selected piece ";
  print_string "> "

let add_more_players_string name = 
  print_string name;
  print_endline " has been added to the game!";
  print_endline  "\nPlease add the next players and select your pieces! Type 'Done' when all players have been added.";
  print_endline  "Enter Player Name ";
  print_string "> "

let process_done state = 
  print_endline "\nYou're done adding the players, let's start the game!";
  game_start state

let rec add_player state name = 
  match String.trim name with 
  | "done" | "Done" -> process_done state
  | _ -> match_all_string name;
    let piece = (read_line ()) in
    match State.check_valid state piece with 
    | exception State.NotANumPiece piece -> 
      print_endline "Oops! You have to enter the number corresponding to the piece. Try again";
      add_player state name;
    | exception State.UsedPiece piece -> 
      print_endline "Oops! Two players can't have the same piece. Try again";
      add_player state name;
    | exception State.InvalidPiece piece -> 
      print_endline "Oops! You entered a piece that does not exist. Select a piece from the List";
      add_player state name;
    | _ ->
      begin
        let new_state = State.new_player state name (int_of_string piece) in
        add_more_players_string name;
        add_player new_state (read_line ())
      end

let chose_piece_instruction () = 
  ANSITerminal.(print_string [cyan] "\n
   1. CTB Bagel \n
   2. TCAT \n
   3. Big Red Bear \n
   4. Hockey Stick \n
   5. Clock Tower \n
   6. Louie's Food Truck \n");
  print_endline  "\n Please add the players and select your pieces!
  Type 'Done' when all players have been added";
  print_endline  "\n Enter Player Name ";
  print_string "> "

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red] 
                  "\n\nWelcome to CORNOPOLY! - The Cornell Version of Monopoly!
                  \n");
  print_endline  "Basic Rules: 1 - unlimited Players, Ages 12 & up";
  print_endline  "Each player starts with 1500 and no properties";
  print_endline  "Let's get started: ";
  let board = (Board.from_json (Yojson.Basic.from_file "cornell.json")) in
  let cards = (Card.from_json (Yojson.Basic.from_file "cornell_cards.json")) in
  let game_state = State.init_state board cards in
  print_endline  "\n Here are the playing pieces -";
  chose_piece_instruction ();
  ignore (add_player game_state (read_line ()));
  print_endline  "Game Over :)";
  match read_line () with
  | _ -> exit 0

(* Execute the game engine. *)
let () = main ()

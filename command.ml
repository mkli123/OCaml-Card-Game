type menu_command =
|Draft
|Start
|Exit
|Help

type game_command =
|Attack of (int * int)
|End
|HPow of int option
|PCard of int
|LookH
|LookC of int
|Concede
|Help

(*parse the input for the menu options*)
let rec parse_menu () =
  let cmd = read_line () in
  let str = String.lowercase (String.trim cmd) in
  match str with
  |"draft" -> Draft
  |"start" -> Start
  |"exit" -> Exit
  |"help" -> Help
  |x -> Printf.printf "Invalid command\n"; parse_menu ()

(*Returns the second word in the string
 * - str = the string being split
 * - first = the first word in the string
 *)
let next_word str first : string =
  if(String.contains str ' ') then
    let sp = String.index str ' ' in
    String.sub str (sp+1) ((String.length str)-(String.length first) - 1)
  else ""

(*see if the command inputted is for an attack*)
let valid_attack str : bool =
  let len  = String.length str in
  if(len > 6) then
    let cmd = String.sub str 0 6 in
    let num = String.trim (next_word str cmd) in
    if((cmd = "attack") && (String.contains num ' ')) then
      let space = String.index num ' ' in
      let si1 = String.sub num 0 space in
      let si2 = String.trim (next_word num si1) in
      let bi1 =
        try
          let _ = int_of_string si1 in true
        with
        |x -> false in
      let bi2 =
        try
          let _ = int_of_string si2 in true
        with
        |x -> false in
      (bi1 && bi2)
    else false
  else false

(*see if the command inputted is for playing a card*)
let valid_pcard_or_lookc str : bool =
  let len  = String.length str in
  if(len > 5) then
    let cmd = String.sub str 0 5 in
    if(cmd = "pcard" || cmd = "lookc") then
      let num = String.trim (next_word str cmd) in
      try
        let _ = int_of_string num in true
      with
      |x -> false
    else false
  else false

(*see if the command inputted is for hero power*)
let valid_hpow str : bool =
  let len  = String.length str in
  if(len > 4) then
    let cmd = String.sub str 0 4 in
    if(cmd = "hpow") then
      let num = String.trim (next_word str cmd) in
      try
        let _ = int_of_string num in true
      with
      |x -> false
    else false
  else false

(*pares the input for the game commands*)
let rec parse_game () =
  let cmd = read_line () in
  let str = String.lowercase (String.trim cmd) in
  match str with
  |"end" -> End
  |"lookh" -> LookH
  |"concede" -> Concede
  |"help" -> Help
  |"hpow" -> HPow None
  |s when (valid_attack str) -> do_attack str
  |s when (valid_pcard_or_lookc str) -> do_pcard_or_lookc str
  |s when (valid_hpow str) -> do_hpow str
  |_ -> Printf.printf "Invalid command\n"; parse_game ()

(*output the attack command*)
and do_attack str =
  let len  = String.length str in
  if(len > 6) then
    let cmd = String.sub str 0 6 in
    let num = String.trim (next_word str cmd) in
    if((cmd = "attack") && (String.contains num ' ')) then
      let space = String.index num ' ' in
      let si1 = String.sub num 0 space in
      let si2 = String.trim (next_word num si1) in
      let bi1 =
        try
          let _ = int_of_string si1 in true
        with
        |x -> false in
      let bi2 =
        try
          let _ = int_of_string si2 in true
        with
        |x -> false in
      if (bi1 && bi2) then
        let i1 = int_of_string si1 in
        let i2 = int_of_string si2 in
        Attack (i1,i2)
      else parse_game ()
    else parse_game ()
  else parse_game ()

(*output the pcard command*)
and do_pcard_or_lookc str =
  let len  = String.length str in
  if(len > 5) then
    let cmd = String.sub str 0 5 in
    let num = String.trim (next_word str cmd) in
    if(cmd = "pcard" || cmd = "lookc") then
      let bi =
        try
          let _ = int_of_string num in true
        with
        |x -> false in
      if(bi) then
        if(cmd = "pcard") then PCard (int_of_string num)
        else if (cmd = "lookc") then LookC (int_of_string num)
        else parse_game ()
      else parse_game ()
    else parse_game ()
  else parse_game ()

(*output the hpow command*)
and do_hpow str =
  let len  = String.length str in
  if(len > 4) then
    let cmd = String.sub str 0 4 in
    let num = String.trim (next_word str cmd) in
    if(cmd = "hpow") then
      let bi =
        try
          let _ = int_of_string num in true
        with
        |x -> false in
      if(bi) then HPow (Some (int_of_string num))
      else parse_game ()
    else parse_game ()
  else parse_game ()
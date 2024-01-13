open Printf

let total_red = 12
let total_green = 13
let total_blue = 14

let usage () =
  let progname = Sys.argv.(0) in
  printf "usage: %s <file>\n" progname;
  exit 1

let process contents =
  let result = ref 0 in
  String.split_on_char '\n' contents
  |> List.iter (fun line ->
      match String.split_on_char ':' line with
      | [game_str; draws_str] ->
        (match String.split_on_char ' ' game_str with
         | [_; game_num_str] ->
           let game_num = int_of_string game_num_str in
           let valid = ref true in
           String.split_on_char ';' draws_str
           |> List.iter (fun draw_str ->
              let trimmed_draw = String.sub draw_str 1 ((String.length draw_str) - 1) in
              String.split_on_char ',' trimmed_draw
              |> List.iter (fun color_amount ->
                let trimmed_color_amount =
                  if (String.get color_amount 0) == ' '
                    then (String.sub color_amount 1 ((String.length color_amount) - 1))
                    else color_amount
                in
                match String.split_on_char ' ' trimmed_color_amount with
                | [amount_str; color] ->
                  let amount = int_of_string amount_str in
                  (match color with
                  | "red" -> if amount > total_red then valid := false
                  | "green" -> if amount > total_green then valid := false
                  | "blue" -> if amount > total_blue then valid := false
                  | _ -> failwith "unknown color"
                  )
                | _ -> ()
              )
           );
           if !valid then result := !result + game_num
         | _ -> ()
        )
      | _ -> ()
    );
    !result

let () =
  if Array.length Sys.argv < 2 then
    usage ()
  else
    let filename = Sys.argv.(1) in
    let input_channel = open_in filename in
    try
      let contents = In_channel.input_all input_channel in
      let result = process contents in
        printf "result = %d\n" result
    with e ->
      raise e

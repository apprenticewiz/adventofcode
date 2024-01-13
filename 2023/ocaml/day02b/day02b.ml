open Printf

let usage () =
  let progname = Sys.argv.(0) in
  printf "usage: %s <file>\n" progname;
  exit 1

let process contents =
  let result = ref 0 in
  String.split_on_char '\n' contents
  |> List.iter (fun line ->
      match String.split_on_char ':' line with
      | [_; draws_str] ->
        (let red_needed = ref 0 in
         let green_needed = ref 0 in
         let blue_needed = ref 0 in
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
                 | "red" -> if amount > !red_needed then red_needed := amount
                 | "green" -> if amount > !green_needed then green_needed := amount
                 | "blue" -> if amount > !blue_needed then blue_needed := amount
                 | _ -> failwith "unknown color"
                 )
               | _ -> ()
              )
           );
           result := !result + !red_needed * !green_needed * !blue_needed
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

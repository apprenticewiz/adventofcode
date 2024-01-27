open Printf

type position = { row : int; col : int; }

module PositionMap = Map.Make(struct
  type t = position
  let compare p1 p2 =
    match compare p1.row p2.row with
    | 0 -> compare p1.col p2.col
    | result -> result
end)

let usage () =
  let progname = Sys.argv.(0) in
  printf "usage: %s <file>\n" progname;
  exit 1

let is_digit ch =
  match ch with
    '0'..'9' -> true
  | _ -> false

let build_numbers contents =
  let (_, number_locs) = List.fold_left (fun (row, number_locs) line ->
    let (_, _, _, _, num_locs) = List.fold_left (fun (col, scanning_number, number, current_pos, num_locs) ch ->
      if scanning_number
        then
          if is_digit ch
            then
              if (col + 1) = (String.length line)
                then (col + 1, false, "", { row = -1; col = -1 }, (PositionMap.add current_pos (number ^ (String.make 1 ch)) num_locs))
                else (col + 1, true, (number ^ (String.make 1 ch)), current_pos, num_locs)
            else (col + 1, false, "", { row = -1; col = -1 }, (PositionMap.add current_pos number num_locs))
        else
          if is_digit ch
            then (col + 1, true, (String.make 1 ch), { row = row; col = col }, num_locs)
            else (col + 1, false, number, current_pos, num_locs)
      ) (0, false, "", { row = -1; col = -1 }, number_locs) (List.init (String.length line) (String.get line))
    in (row + 1, num_locs)
    ) (0, PositionMap.empty) (String.split_on_char '\n' contents)
  in number_locs

let build_parts contents =
  let (_, part_locs) = List.fold_left (fun (row, part_locs) line ->
    let (_, prt_locs) = List.fold_left (fun (col, prt_locs) ch ->
      if ch = '*'
        then (col + 1, (PositionMap.add { row = row; col = col } ch prt_locs))
        else (col + 1, prt_locs)
      ) (0, part_locs) (List.init (String.length line) (String.get line))
    in (row + 1, prt_locs)
    ) (0, PositionMap.empty) (String.split_on_char '\n' contents)
  in part_locs

let check_parts number_locs part_locs =
  List.fold_left (fun result part_loc ->
    let adjacents = List.fold_left (fun adjacents_list (number_loc, number) ->
        let number_row = number_loc.row in
        let number_col_first = number_loc.col in
        let number_col_last = number_loc.col + (String.length number) - 1 in
        let neighbors = [
          { row = -1; col = -1 }; { row = -1; col = 0 }; { row = -1; col = 1 };
          { row = 0; col = -1 }; { row = 0; col = 1 };
          { row = 1; col = -1 }; { row = 1; col = 0 }; { row = 1; col = 1 }    
        ] in
        let found_adjacent = List.fold_left (fun found neighbor ->
          let adjacent_pos = { row = part_loc.row + neighbor.row; col = part_loc.col + neighbor.col }
          in found || ((adjacent_pos.row == number_row) &&
                       (adjacent_pos.col >= number_col_first) &&
                       (adjacent_pos.col <= number_col_last))
        ) false neighbors
        in
          if found_adjacent
            then adjacents_list @ [int_of_string number]
            else adjacents_list
      ) [] (PositionMap.bindings number_locs)
    in
      if (List.length adjacents) = 2
        then result + (List.fold_left (fun acc num -> acc * num) 1 adjacents)
        else result
  ) 0 (List.map (fun (k, v) -> k) (PositionMap.bindings part_locs))

let process contents =
  let number_locs = build_numbers contents in
  let part_locs = build_parts contents in
  check_parts number_locs part_locs

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

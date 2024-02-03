open Printf

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

let usage () =
  let progname = Sys.argv.(0) in
  printf "usage: %s <file>\n" progname;
  exit 1

let process contents =
  List.fold_left (fun result line ->
      if line <> ""
        then
          (let rest = (List.nth (String.split_on_char ':' line) 1) in
           let winning_str = (List.nth (String.split_on_char '|' rest) 0) in
           let winning_set = List.fold_left (fun set num_str ->
             if num_str <> ""
               then (IntSet.add (int_of_string num_str) set)
               else set
             ) IntSet.empty (String.split_on_char ' ' winning_str) in
           let hand_str = (List.nth (String.split_on_char '|' rest) 1) in
           let hand_set = List.fold_left (fun set num_str ->
               if num_str <> ""
                 then (IntSet.add (int_of_string num_str) set)
                 else set
             ) IntSet.empty (String.split_on_char ' ' hand_str) in
           let intersection = IntSet.inter winning_set hand_set in
           let count = IntSet.cardinal intersection in
           result + if (count > 0) then (Int.shift_left 1 (count - 1)) else 0)
        else result
    ) 0 (String.split_on_char '\n' contents)

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

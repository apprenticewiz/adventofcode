open Printf

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

let usage () =
  let progname = Sys.argv.(0) in
  printf "usage: %s <file>\n" progname;
  exit 1

let process contents =
  let instances = List.fold_left (fun instances line ->
      if line <> ""
        then
          (let card_part = (List.nth (String.split_on_char ':' line) 0) in
           let card_num_split = (String.split_on_char ' ' card_part) in
           let card_num_str = (List.nth card_num_split ((List.length card_num_split) - 1)) in
           let card_num = (int_of_string card_num_str) in
           let rest = (List.nth (String.split_on_char ':' line) 1) in
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
           List.fold_left (fun prev_instances i ->
               let copies =
                 (if ((IntMap.find_opt i prev_instances) <> None)
                    then (IntMap.find i prev_instances)
                    else 0) + 1 +
                 (if ((IntMap.find_opt card_num prev_instances) <> None)
                    then (IntMap.find card_num prev_instances)
                    else 0) in
               (IntMap.add i copies prev_instances)
             ) instances (List.init count (fun n -> n + 1 + card_num))
          )
        else instances
    ) IntMap.empty (String.split_on_char '\n' contents) in
  (List.fold_left (fun acc n -> acc + n) 0 (List.map (fun (k, v) -> v) (IntMap.bindings instances))) +
  ((List.length (String.split_on_char '\n' contents)) - 1)

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

open Printf

let usage () =
  let progname = Sys.argv.(0) in
  printf "usage: %s <file>\n" progname;
  exit 1

let rec find_str needle haystack at =
  if String.length haystack = 0 then
    -1
  else if String.starts_with ~prefix:needle haystack then
    at
  else
    find_str needle (String.sub haystack 1 ((String.length haystack) - 1)) (at + 1)

let rec rfind_str needle haystack at =
  if String.length haystack = 0 then
    -1
  else if String.ends_with ~suffix:needle haystack then
    at
  else
    rfind_str needle (String.sub haystack 0 ((String.length haystack) - 1)) (at - 1)

let process contents =
  let sum = ref 0 in
  let digits_map = Hashtbl.create 20 in
    Hashtbl.add digits_map "0" 0;
    Hashtbl.add digits_map "1" 1;
    Hashtbl.add digits_map "2" 2;
    Hashtbl.add digits_map "3" 3;
    Hashtbl.add digits_map "4" 4;
    Hashtbl.add digits_map "5" 5;
    Hashtbl.add digits_map "6" 6;
    Hashtbl.add digits_map "7" 7;
    Hashtbl.add digits_map "8" 8;
    Hashtbl.add digits_map "9" 9;
    Hashtbl.add digits_map "zero" 0;
    Hashtbl.add digits_map "one" 1;
    Hashtbl.add digits_map "two" 2;
    Hashtbl.add digits_map "three" 3;
    Hashtbl.add digits_map "four" 4;
    Hashtbl.add digits_map "five" 5;
    Hashtbl.add digits_map "six" 6;
    Hashtbl.add digits_map "seven" 7;
    Hashtbl.add digits_map "eight" 8;
    Hashtbl.add digits_map "nine" 9;
    contents |> String.split_on_char '\n' |> List.iter (fun line ->
      let min_index = ref None in
      let max_index = ref None in
      let left_digit = ref 0 in
      let right_digit = ref 0 in
      Hashtbl.iter (fun digit_str digit ->
        if (find_str digit_str line 0) <> -1 then
          let left_index = find_str digit_str line 0 in
          (match !min_index with
          | None -> min_index := Some left_index; left_digit := digit
          | Some mi when left_index < mi -> min_index := Some left_index; left_digit := digit
          | _ -> ());
          let right_index = rfind_str digit_str line ((String.length line) - 1) in
          (match !max_index with
          | None -> max_index := Some right_index; right_digit := digit
          | Some mi when right_index > mi -> max_index := Some right_index; right_digit := digit
          | _ -> ());
      ) digits_map;
      sum := !sum + (!left_digit * 10) + !right_digit
    );
    !sum

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

open Printf

let usage () =
  let progname = Sys.argv.(0) in
  printf "usage: %s <file>\n" progname;
  exit 1

let process contents =
  let sum = ref 0 in
  let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] in
  contents |> String.split_on_char '\n' |> List.iter (fun line ->
    let min_index = ref None in
    let max_index = ref None in
    let left_digit = ref '0' in
    let right_digit = ref '0' in
    digits |> List.iter (fun digit ->
      if String.contains line digit then
        let left_index = String.index line digit in
        (match !min_index with
        | None -> min_index := Some left_index; left_digit := digit
        | Some mi when left_index < mi -> min_index := Some left_index; left_digit := digit
        | _ -> ());
        let right_index = String.rindex line digit in
        (match !max_index with
        | None -> max_index := Some right_index; right_digit := digit
        | Some mi when right_index > mi -> max_index := Some right_index; right_digit := digit
        | _ -> ());
    );
    sum := !sum + (((int_of_char !left_digit) - (int_of_char '0')) * 10) + ((int_of_char !right_digit) - (int_of_char '0'))
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

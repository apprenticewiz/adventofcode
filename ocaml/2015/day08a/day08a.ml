open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let scan_line line =
  let code_len = String.length line in
  let rec count_escaped i chars =
    match chars with
    | [] -> i
    | [ _ ] -> i + 1
    | ch1 :: ch2 :: rest ->
        if ch1 = '\\' then
          match ch2, rest with
          | ('\\' | '"'), rest2 -> count_escaped (i + 1) rest2
          | 'x', _ :: _ :: rest4 -> count_escaped (i + 1) rest4
          | _ -> count_escaped (i + 1) (ch2 :: rest)
        else
          count_escaped (i + 1) (ch2 :: rest)
  in
  let chars = List.of_seq (String.to_seq (String.sub line 1 (code_len - 2))) in
  let mem_len = count_escaped 0 chars in
  code_len - mem_len

let process_file filename =
  try
    let lines = In_channel.input_lines (In_channel.open_text filename) in
    let process_line acc line = (
      acc + scan_line line
    ) in
    List.fold_left process_line 0 lines
  with
  | Sys_error msg ->
      eprintf "error reading file: %s\n" msg;
      exit 1

let () =
  let args = Sys.argv
  in if Array.length args < 2 then
       usage args.(0) ;
     let filename = args.(1) in
     let result = process_file filename in
     printf "result = %d\n" result

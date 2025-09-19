open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let scan_line line =
  let code_len = String.length line in
  let rec do_scan i chars =
    match chars with
    | [] -> i
    | '\\' :: rest -> do_scan (i + 2) rest
    | '"' :: rest -> do_scan (i + 2) rest
    | _ :: rest -> do_scan (i + 1) rest
  in
  let chars = List.of_seq (String.to_seq line) in
  let enc_len = do_scan 0 chars in
  2 + (enc_len - code_len)

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

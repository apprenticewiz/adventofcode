open In_channel
open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let process_file filename =
  let f floor ch =
    match ch with
    | '(' -> floor + 1
    | ')' -> floor - 1
    | _ -> floor
  in
  try
    let content = input_all (open_text filename) in
    String.fold_left f 0 content
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

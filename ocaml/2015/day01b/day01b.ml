open In_channel
open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let process_file filename =
  let rec check_pos str pos floor =
    if floor < 0 then
      pos
    else if String.length str = 0 then
      0
    else let ch = String.get str pos
         in match ch with
            | '(' -> check_pos str (pos + 1) (floor + 1)
            | ')' -> check_pos str (pos + 1) (floor - 1)
            | _ -> check_pos str (pos + 1) floor
  in
  try
    let content = input_all (open_text filename) in
    check_pos content 0 0
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

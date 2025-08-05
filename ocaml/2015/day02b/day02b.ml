open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let process_file filename =
  let lines = In_channel.input_lines (In_channel.open_text filename) in
  let process_line total_len line = (
    let dims = String.split_on_char 'x' line in
    let l = int_of_string (List.nth dims 0) in
    let w = int_of_string (List.nth dims 1) in
    let h = int_of_string (List.nth dims 2) in
    let perim1 = 2 * (l + w) in
    let perim2 = 2 * (l + h) in
    let perim3 = 2 * (w + h) in
    let present_len = Int.min perim1 (Int.min perim2 perim3) in
    let bow_len = l * w * h in
    total_len + present_len + bow_len
  ) in
  List.fold_left process_line 0 lines

let () =
  let args = Sys.argv
  in if Array.length args < 2 then
       usage args.(0) ;
     let filename = args.(1) in
     let result = process_file filename in
     printf "result = %d\n" result

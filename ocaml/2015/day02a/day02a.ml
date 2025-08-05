open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let process_file filename =
  let lines = In_channel.input_lines (In_channel.open_text filename) in
  let process_line total_area line = (
    let dims = String.split_on_char 'x' line in
    let l = int_of_string (List.nth dims 0) in
    let w = int_of_string (List.nth dims 1) in
    let h = int_of_string (List.nth dims 2) in
    let area1 = l * w in
    let area2 = l * h in
    let area3 = w * h in
    let surface_area = (2 * area1) + (2 * area2) + (2 * area3) in
    let min_area = Int.min area1 (Int.min area2 area3) in
    total_area + surface_area + min_area
  ) in
  List.fold_left process_line 0 lines

let () =
  let args = Sys.argv
  in if Array.length args < 2 then
       usage args.(0) ;
     let filename = args.(1) in
     let result = process_file filename in
     printf "result = %d\n" result

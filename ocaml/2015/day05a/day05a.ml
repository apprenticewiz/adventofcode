open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let prop1 str =
  let is_vowel ch = (ch = 'a' || ch = 'e' || ch = 'i' || ch = 'o' || ch = 'u') in
  let len = String.length str in
  let rec loop i vowel_count =
    if vowel_count == 3 then true
    else if i >= len then false
    else if is_vowel str.[i] then loop (i + 1) (vowel_count + 1)
    else loop (i + 1) vowel_count
  in loop 0 0

let prop2 str =
  let len = String.length str in
  let rec loop i =
    if i >= len - 1 then false
    else if str.[i] = str.[i + 1] then true
    else loop (i + 1)
  in loop 0

let prop3 str =
  let len = String.length str in
  let rec loop i =
    if i >= len - 1 then true
    else
      let a = str.[i] and b = str.[i + 1] in
      match (a, b) with
      | ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') -> false
      | _ -> loop (i + 1)
  in loop 0

let process_file filename =
  try
    let lines = In_channel.input_lines (In_channel.open_text filename) in
    let process_line count line = (
      if prop1 line && prop2 line && prop3 line then
        count + 1
      else
        count
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

open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let prop1 str =
  let len = String.length str in
  let rec loop1 i =
    if i >= len - 3 then false
    else
      (let a = str.[i] and b = str.[i + 1] in
       let rec loop2 j =
         if j >= len - 1 then false
         else
           (let c = str.[j] and d = str.[j + 1] in
            if (a = c) && (b = d) then true
            else (loop2 (j + 1))) in
       if (loop2 (i + 2)) then true
       else (loop1 (i + 1)))
  in loop1 0

let prop2 str =
  let len = String.length str in
  let rec loop i =
    if i >= len - 2 then false
    else if str.[i] = str.[i + 2] then true
    else loop (i + 1)
  in loop 0

let process_file filename =
  try
    let lines = In_channel.input_lines (In_channel.open_text filename) in
    let process_line count line = (
      if prop1 line && prop2 line then
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

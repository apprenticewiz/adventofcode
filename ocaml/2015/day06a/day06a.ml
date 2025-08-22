open Printf
open Sys

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let perform grid action (x1, y1, x2, y2) =
  for j = y1 to y2 do
    for i = x1 to x2 do
      match action with
      | "turn on" -> grid.(j).(i) <- true
      | "turn off" -> grid.(j).(i) <- false
      | "toggle" -> grid.(j).(i) <- (not grid.(j).(i))
      | _ -> ()
    done
  done

let count grid =
  let count = ref 0 in
  for j = 0 to 999 do
    for i = 0 to 999 do
      if grid.(j).(i) then
        count := !count + 1
      else
        ()
     done
  done ;
  !count

let process_file filename =
  try
    let grid = Array.init 1000 (fun _ -> Array.make 1000 false) in
    let lines = In_channel.input_lines (In_channel.open_text filename) in
    let re = Str.regexp {|\(turn on\|turn off\|toggle\) \([0-9]+\),\([0-9]+\) through \([0-9]+\),\([0-9]+\)|} in
    Seq.iter (fun line ->
      if Str.string_match re line 0 then
        let action = Str.matched_group 1 line in
        let x1 = int_of_string (Str.matched_group 2 line) in
        let y1 = int_of_string (Str.matched_group 3 line) in
        let x2 = int_of_string (Str.matched_group 4 line) in
        let y2 = int_of_string (Str.matched_group 5 line) in
        perform grid action (x1, y1, x2, y2)
    ) (List.to_seq lines) ;
    count grid
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

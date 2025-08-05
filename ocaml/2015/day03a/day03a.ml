open Printf
open Sys

module Position2D =
  struct
    type t = int * int
    let compare (x0, y0) (x1, y1) =
      match Stdlib.compare x0 x1 with
      | 0 -> Stdlib.compare y0 y1
      | res -> res
  end

module PositionSet = Set.Make(Position2D)

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let process_file filename =
  let process_ch (santa, positions) ch = (
    let (santa_x, santa_y) = santa in
    let new_santa = ( match ch with
                      | '^' -> (santa_x, santa_y + 1)
                      | 'v' -> (santa_x, santa_y - 1)
                      | '<' -> (santa_x - 1, santa_y)
                      | '>' -> (santa_x + 1, santa_y)
                      | _ -> santa ) in
    let new_positions = PositionSet.add new_santa positions in
    (new_santa, new_positions)
  )
  in
  try
    let content = In_channel.input_all (In_channel.open_text filename) in
    let (_, final_positions) = String.fold_left process_ch ((0, 0), PositionSet.empty) content in
    PositionSet.cardinal final_positions
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

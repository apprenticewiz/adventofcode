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
  let process_ch (santa, robo_santa, positions, santa_move) ch = (
    let (santa_x, santa_y) = santa in
    let (robo_santa_x, robo_santa_y) = robo_santa in
    let new_santa = if santa_move then
                      ( match ch with
                        | '^' -> (santa_x, santa_y + 1)
                        | 'v' -> (santa_x, santa_y - 1)
                        | '<' -> (santa_x - 1, santa_y)
                        | '>' -> (santa_x + 1, santa_y)
                        | _ -> santa )
                    else santa in
    let new_robo_santa = if santa_move then
                           robo_santa
                         else
                           ( match ch with
                             | '^' -> (robo_santa_x, robo_santa_y + 1)
                             | 'v' -> (robo_santa_x, robo_santa_y - 1)
                             | '<' -> (robo_santa_x - 1, robo_santa_y)
                             | '>' -> (robo_santa_x + 1, robo_santa_y)
                             | _ -> robo_santa ) in
    let new_positions = if santa_move then
                          (PositionSet.add new_santa positions)
                        else
                          (PositionSet.add new_robo_santa positions) in
    (new_santa, new_robo_santa, new_positions, not santa_move)
  )
  in
  try
    let content = In_channel.input_all (In_channel.open_text filename) in
    let (_, _, final_positions, _) = String.fold_left process_ch ((0, 0), (0, 0), PositionSet.empty, true) content in
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

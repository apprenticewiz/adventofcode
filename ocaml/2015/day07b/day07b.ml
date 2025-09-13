open Printf
open Sys

type operator =
    AssignOp of string
  | NotOp of string
  | AndOp of (string * string)
  | OrOp of (string * string)
  | LeftShiftOp of (string * int)
  | RightShiftOp of (string * int)

module StringMap = Map.Make(String)

let usage progname =
  eprintf "usage: %s <input file>\n" progname;
  exit 1

let parse_line line =
  let tokens = String.split_on_char ' ' line in
  match tokens with
    | src :: "->" :: dest :: _ -> (dest, AssignOp src)
    | "NOT" :: src :: "->" :: dest :: _ -> (dest, NotOp src)
    | src1 :: "AND" :: src2 :: "->" :: dest :: _ -> (dest, AndOp (src1, src2))
    | src1 :: "OR" :: src2 :: "->" :: dest :: _ -> (dest, OrOp (src1, src2))
    | src :: "LSHIFT" :: amt_str :: "->" :: dest :: _ -> (dest, LeftShiftOp (src, int_of_string amt_str))
    | src :: "RSHIFT" :: amt_str :: "->" :: dest :: _ -> (dest, RightShiftOp (src, int_of_string amt_str))
    | _ -> failwith ("malformed input line: " ^ line)

let rec eval ops cache expr =
  match (int_of_string_opt expr) with
    | Some n -> (n, cache)
    | None ->
        (match StringMap.find_opt expr cache with
          | Some v -> (v, cache)
          | None ->
              (let oper = StringMap.find expr ops in
               let (r, c) = (match oper with
                               | AssignOp src -> eval ops cache src
                               | NotOp src -> (let (a, c') = eval ops cache src in
                                               (Int.lognot a, c'))
                               | AndOp (src1, src2) -> (let (a, c') = eval ops cache src1 in
                                                        let (b, c'') = eval ops c' src2 in
                                                        (Int.logand a b, c''))
                               | OrOp (src1, src2) -> (let (a, c') = eval ops cache src1 in
                                                       let (b, c'') = eval ops c' src2 in
                                                       (Int.logor a b, c''))
                               | LeftShiftOp (src, amt) -> (let (a, c') = eval ops cache src in
                                                            (Int.shift_left a amt, c'))
                               | RightShiftOp (src, amt) -> (let (a, c') = eval ops cache src in
                                                             (Int.shift_right_logical a amt, c'))) in
               let masked = Int.logand r 65535 in
               (masked, StringMap.add expr masked c)))

let process_file filename =
  try
    let lines = In_channel.input_lines (In_channel.open_text filename) in
    let operations = Seq.fold_left
      (fun acc line ->
        let (dest, oper) = parse_line line in
        StringMap.add dest oper acc
      )
      StringMap.empty
      (List.to_seq lines) in
    let (a, _) = eval operations StringMap.empty "a" in
    let new_ops = StringMap.add "b" (AssignOp (string_of_int a)) operations in
    let (a', _) = eval new_ops StringMap.empty "a" in
    a'
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

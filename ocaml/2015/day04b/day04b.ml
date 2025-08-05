open Printf
open Sys

let usage progname =
  eprintf "usage: %s <key>\n" progname;
  exit 1

let process_key key =
  let rec check_key key n =
    let try_key = key ^ (string_of_int n) in
    let digest = Digest.MD5.string try_key in
    let hex_bytes = Digest.MD5.to_hex digest in
    if String.starts_with ~prefix:"000000" hex_bytes then
      n
    else
      check_key key (n + 1)
  in check_key key 1

let () =
  let args = Sys.argv
  in if Array.length args < 2 then
       usage args.(0) ;
     let key = args.(1) in
     let result = process_key key in
     printf "result = %d\n" result

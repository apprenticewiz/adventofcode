open OS.Process
open TextIO

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun process filename =
  let 
    fun find_pos ([], _, _) = 0
      | find_pos (x::xs, pos, floor) =
          if floor < 0 then
            pos
          else
            let val nextFloor =
                case x of
                  #"(" => floor + 1
                | #")" => floor - 1
                | _ => floor
            in find_pos (xs, pos + 1, nextFloor)
            end
    val inputStream = openIn filename handle IO.Io { name = n, function = f, cause = c } =>
      (printErr ("error opening file `" ^ n ^ ".\n");
       exit failure)
    val content = inputAll inputStream
  in
    find_pos (String.explode content, 0, 0)
  end

val _ =
  let
    val args = CommandLine.arguments ()
  in
    if (List.length args) < 1 then
      (printErr ("usage: " ^ CommandLine.name () ^ " <input file>\n");
       exit failure)
    else
      let
        val filename = List.hd args
        val result = process filename
      in
        print ("result = " ^ (Int.toString result) ^ "\n")
      end
  end

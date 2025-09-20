open OS.Process
open TextIO

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun scanLine (line, acc) =
  let
    val codeLen = String.size line
    fun iter i nil = i
      | iter i (x::rest) =
          case x of
            #"\\" => iter (i + 2) rest
          | #"\"" => iter (i + 2) rest
          | _ => iter (i + 1) rest
    val encLen = iter 0 (String.explode line)
  in
    acc + 2 + (encLen - codeLen)
  end

fun process filename =
  let
    val inputStream = openIn filename handle IO.Io { name = n, function = f, cause = c } =>
      (printErr ("error opening file `" ^ n ^ ".\n");
       exit failure)
    val content = inputAll inputStream
  in
    (List.foldl scanLine 0 (String.tokens (fn c => c = #"\n") content))
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

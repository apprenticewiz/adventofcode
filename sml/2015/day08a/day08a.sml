open OS.Process
open TextIO

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun scanLine (line, acc) =
  let
    val codeLen = String.size line
    val quoted = List.tl (List.take (String.explode line, String.size line - 1))
    fun iter i nil = i
      | iter i [x] = i + 1
      | iter i (x::y::rest) =
          case x of
            #"\\" =>
              (case y of
                 #"\\" => iter (i + 1) rest
               | #"\"" => iter (i + 1) rest
               | #"x" => iter (i + 1) (List.drop (rest, 2))
               | _ => iter (i + 1) (y::rest)
              )
          | _ => iter (i + 1) (y::rest)
    val memLen = iter 0 quoted
  in
    acc + (codeLen - memLen)
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

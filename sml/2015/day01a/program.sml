open OS.Process
open TextIO

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun process filename =
  let 
    fun f (ch, floor) =
      case ch of
        #"(" => floor + 1
      | #")" => floor - 1
      | _ => floor;
    val inputStream = openIn filename handle IO.Io { name = n, function = f, cause = c } =>
      (printErr ("error opening file `" ^ n ^ ".\n");
       exit failure)
    val content = inputAll inputStream
  in
    (List.foldl f 0 (String.explode content))
  end

val _ =
  let
    val args = CommandLine.arguments ()
  in
    if (List.length args) < 3 then
      (printErr "usage: ./run.sh <input file>\n" ;
       exit failure)
    else
      let
        val filename = List.nth (args, 2)
        val result = process filename
      in
        print ("result = " ^ (Int.toString result) ^ "\n")
      end
  end

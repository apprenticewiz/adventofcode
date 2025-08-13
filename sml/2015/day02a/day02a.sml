open OS.Process
open TextIO

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun process filename =
  let 
    fun f (line, acc) =
      let
        fun convertToInt x =
            case Int.fromString x of
              SOME n => n
            | NONE => 0
        val dims = List.map convertToInt (String.tokens (fn c => c = #"x") line)
        val l = List.nth (dims, 0)
        val w = List.nth (dims, 1)
        val h = List.nth (dims, 2)
        val area1 = l * w
        val area2 = l * h
        val area3 = w * h
        val surfaceArea = (2 * area1) + (2 * area2) + (2 * area3)
        val minArea = Int.min (area1, Int.min (area2, area3))
      in
        acc + surfaceArea + minArea
      end
    val inputStream = openIn filename handle IO.Io { name = n, function = f, cause = c } =>
      (printErr ("error opening file `" ^ n ^ ".\n");
       exit failure)
    val content = inputAll inputStream
  in
    (List.foldl f 0 (String.tokens (fn c => c = #"\n") content))
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

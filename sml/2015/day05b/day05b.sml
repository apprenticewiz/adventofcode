open OS.Process
open TextIO

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun process filename =
  let 
    fun checkLine (line, acc) =
      let
        fun prop1 s =
          if (String.size s) < 4 then false
          else
            let
              val s1 = String.extract (s, 0, SOME 2)
              val s2 = String.extract (s, 2, NONE)
            in
              if (String.isSubstring s1 s2) then true
              else prop1 (String.extract (s, 1, NONE))
            end

        fun prop2 s =
        let
          fun checkChars [] = false
            | checkChars [x] = false
            | checkChars [x, y] = false
            | checkChars (x::y::z::xs) = if x = z then true else checkChars (y::z::xs)
        in checkChars (String.explode s)
        end

      in
        if (prop1 line) andalso (prop2 line) then acc + 1 else acc
      end
    val inputStream = openIn filename handle IO.Io { name = n, function = f, cause = c } =>
      (printErr ("error opening file `" ^ n ^ ".\n");
       exit failure)
    val content = inputAll inputStream
  in
    (List.foldl checkLine 0 (String.tokens (fn c => c = #"\n") content))
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

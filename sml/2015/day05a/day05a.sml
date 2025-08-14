open OS.Process
open TextIO

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun process filename =
  let 
    fun checkLine (line, acc) =
      let
        fun prop1 s =
        let
          fun countVowels (ch, n) =
            if ch = #"a" orelse ch = #"e" orelse ch = #"i" orelse ch = #"o" orelse ch = #"u" then n + 1 else n
        in (List.foldl countVowels 0 (String.explode s)) >= 3
        end

        fun prop2 s =
        let
          fun checkChars [] = false
            | checkChars [x] = false
            | checkChars (x::y::xs) = if x = y then true else checkChars (y::xs)
        in checkChars (String.explode s)
        end

        fun prop3 s =
          not (String.isSubstring "ab" s) andalso not (String.isSubstring "cd" s)
            andalso not (String.isSubstring "pq" s) andalso not (String.isSubstring "xy" s)

      in
        if (prop1 line) andalso (prop2 line) andalso (prop3 line) then acc + 1 else acc
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

open OS.Process
open TextIO

datatype Op = Assign of string
            | Not of string
            | And of string * string
            | Or of string * string
            | LeftShift of string * Word.word
            | RightShift of string * Word.word

fun printErr s = (output (stdErr, s); flushOut stdErr)

fun get pairs key = List.find (fn (k, _) => String.compare (k, key) = EQUAL) pairs

fun update pairs key value = (key, value) :: (List.filter (fn (k, _) => String.compare (k, key) <> EQUAL) pairs)

fun evaluate ops cache expr =
  case (Int.fromString expr) of 
       SOME n => (Word32.fromInt n, cache)
     | NONE =>
         (case (get cache expr) of
               SOME (_, v) => (v, cache)
             | NONE =>
                 (case (get ops expr) of
                       SOME (_, oper) =>
                           (let val (r, c) =
                                 case oper of
                                      Assign src => evaluate ops cache src
                                    | Not src =>
                                        let val (a, c1) = evaluate ops cache src
                                        in (Word32.notb a, c1)
                                        end
                                    | And (src1, src2) =>
                                        let val (a, c1) = evaluate ops cache src1
                                            val (b, c2) = evaluate ops c1 src2
                                        in (Word32.andb (a, b), c2)
                                        end
                                    | Or (src1, src2) =>
                                        let val (a, c1) = evaluate ops cache src1
                                            val (b, c2) = evaluate ops c1 src2
                                        in (Word32.orb (a, b), c2)
                                        end
                                    | LeftShift (src, amt) =>
                                        let val (a, c1) = evaluate ops cache src
                                        in (Word32.<< (a, amt), c1)
                                        end
                                    | RightShift (src, amt) =>
                                        let val (a, c1) = evaluate ops cache src
                                        in (Word32.>> (a, amt), c1)
                                        end
                                val masked = Word32.andb (r, 0wxFFFF)
                                val newCache = update c expr masked
                            in (masked, newCache)
                            end)
                     | NONE => raise Fail ("op for expression " ^ expr ^ " is undefined")))

fun parseLine line =
  let val tokens = String.tokens (fn c => Char.isSpace c) line
  in case tokens of
          src :: "->" :: dest :: _ => (dest, Assign src)
        | "NOT" :: src :: "->" :: dest :: _ => (dest, Not src)
        | src1 :: "AND" :: src2 :: "->" :: dest :: _ => (dest, And (src1, src2))
        | src1 :: "OR" :: src2 :: "->" :: dest :: _ => (dest, Or (src1, src2))
        | src :: "LSHIFT" :: amtStr :: "->" :: dest :: _ =>
            (dest, LeftShift (src, (Word32.fromInt (Option.valOf (Int.fromString amtStr)))))
        | src :: "RSHIFT" :: amtStr :: "->" :: dest :: _ =>
            (dest, RightShift (src, (Word32.fromInt (Option.valOf (Int.fromString amtStr)))))
        | _ => raise Fail ("malformed input line: " ^ line)
  end

fun process filename =
  let 
    val inputStream = openIn filename handle IO.Io { name = n, function = f, cause = c } =>
      (printErr ("error opening file `" ^ n ^ ".\n");
       exit failure)
    val content = inputAll inputStream
    val lines = List.filter (fn s => size s > 0) (String.tokens (fn c => c = #"\n") content)
    val operations = List.foldl (fn (line, acc) => (parseLine line) :: acc) [] lines
    val (a, _) = evaluate operations [] "a"
  in Word32.toInt a
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

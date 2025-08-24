open OS.Process
open TextIO

val ROW_MAX = 1000
val COL_MAX = 1000

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun makeGrid init = Array.array (ROW_MAX*COL_MAX, init)

fun parseLine line =
  let
    val tokens = String.tokens (fn c => Char.isSpace c) line
    fun parseCoord s =
      case String.tokens (fn c => c = #",") s of
          [xs, ys] =>
            (case (Int.fromString xs, Int.fromString ys) of
                (SOME x, SOME y) => (x, y)
              | _ => raise Fail "failed to parse integer value")
        | _ => raise Fail ("failed to parse coordinate: " ^ s)
  in
    case tokens of
        "toggle" :: c1 :: "through" :: c2 :: _ => ("toggle", parseCoord c1, parseCoord c2)
      | "turn" :: "on" :: c1 :: "through" :: c2 :: _ => ("turn on", parseCoord c1, parseCoord c2)
      | "turn" :: "off" :: c1 :: "through" :: c2 :: _ => ("turn off", parseCoord c1, parseCoord c2)
      | _ => raise Fail ("unexpected input: " ^ line)
  end

fun perform grid (action, (r1, c1), (r2, c2)) =
  let
    fun set (row, col) =
      let
        val idx = (row * COL_MAX) + col
        val oldValue = Array.sub (grid, idx)
        val newValue =
          case action of
              "turn on" => true
            | "turn off" => false
            | "toggle" => not oldValue
            | _ => oldValue
      in
        Array.update (grid, idx, newValue)
      end
  in
    let fun loopRow row =
        if row > r2 then ()
        else (let fun loopCol col =
                      if col > c2 then ()
                      else (set (row, col) ; loopCol (col + 1))
              in loopCol c1 end;
              loopRow (row + 1))
    in loopRow r1 end
  end

fun process filename =
  let 
    val inputStream = openIn filename handle IO.Io { name = n, function = f, cause = c } =>
      (printErr ("error opening file `" ^ n ^ ".\n");
       exit failure)
    val grid = makeGrid false
    val content = inputAll inputStream
    val lines = List.filter (fn s => size s > 0) (String.tokens (fn c => c = #"\n") content)
    fun loop [] = ()
      | loop (line::rest) =
        let
          val inst = parseLine line
        in
          perform grid inst;
          loop rest
        end
  in
    loop lines ;
    Array.foldl (fn (value, acc) => if value then (acc + 1) else acc) 0 grid
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

open OS.Process
open TextIO

signature KEY = sig
  type key
  val compare : key * key -> order
end

functor BST (Key : KEY) = struct

  datatype tree =
      Empty
    | Node of tree * Key.key * tree

  val new = Empty

  fun singleton x = Node(Empty, x, Empty)

  fun insert (x, Empty) = Node(Empty, x, Empty)
    | insert (x, Node(left, y, right)) =
        case Key.compare (x, y) of
            LESS => Node(insert(x, left), y, right)
          | GREATER => Node(left, y, insert(x, right))
          | EQUAL => Node(left, y, right)

  fun member (x, Empty) = false
    | member (x, Node(left, y, right)) =
        case Key.compare (x, y) of
            LESS => member (x, left)
          | GREATER => member (x, right)
          | EQUAL => true

  fun size Empty = 0
    | size (Node(left, _, right)) = 1 + size(left) + size(right)

end

signature POSITION = sig
  datatype position = Position of int * int
  type key = position
  val compare : key * key -> order
end

structure Position : POSITION = struct
  datatype position = Position of int * int
  type key = position

  fun compare (Position(x1, y1), Position(x2, y2)) =
    case Int.compare(x1, x2) of
        EQUAL => Int.compare(y1, y2)
      | ord => ord
end

structure PositionSet = BST(Position)

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun process filename =
  let 
    fun f (ch, (Position.Position(santaX, santaY), positions)) =
      let
        val newSanta = case ch of
                         #"^" => Position.Position(santaX, santaY + 1)
                       | #"v" => Position.Position(santaX, santaY - 1)
                       | #"<" => Position.Position(santaX - 1, santaY)
                       | #">" => Position.Position(santaX + 1, santaY)
                       | _    => Position.Position(santaX, santaY)
        val newPositions = PositionSet.insert (newSanta, positions)
      in
        (newSanta, newPositions)
      end
    val inputStream = openIn filename handle IO.Io { name = n, function = f, cause = c } =>
      (printErr ("error opening file `" ^ n ^ ".\n");
       exit failure)
    val content = inputAll inputStream
    val (_, finalPositions) = List.foldl f (Position.Position(0, 0), PositionSet.new) (String.explode content)
  in
    PositionSet.size finalPositions
  end

val _ =
  let
    val args = CommandLine.arguments ()
  in
    if (List.length args) < 1 then
      (printErr ("usage: " ^ CommandLine.name () ^ " <input file>\n") ;
       exit failure)
    else
      let
        val filename = List.hd args
        val result = process filename
      in
        print ("result = " ^ (Int.toString result) ^ "\n")
      end
  end

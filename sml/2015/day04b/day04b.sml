open OS.Process
open TextIO

fun md5_digest(input : String.string) : String.string =
let
  fun rotateLeft (x : word, c : word) : word =
    Word.orb (Word.<< (x, c), Word.>> (x, 0w32 - c))
  val r : Word.word Vector.vector =
    Vector.fromList [
      0w7, 0w12, 0w17, 0w22, 0w7, 0w12, 0w17, 0w22, 0w7, 0w12, 0w17, 0w22, 0w7, 0w12, 0w17, 0w22,
      0w5, 0w9, 0w14, 0w20, 0w5, 0w9, 0w14, 0w20, 0w5, 0w9, 0w14, 0w20, 0w5, 0w9, 0w14, 0w20,
      0w4, 0w11, 0w16, 0w23, 0w4, 0w11, 0w16, 0w23, 0w4, 0w11, 0w16, 0w23, 0w4, 0w11, 0w16, 0w23,
      0w6, 0w10, 0w15, 0w21, 0w6, 0w10, 0w15, 0w21, 0w6, 0w10, 0w15, 0w21, 0w6, 0w10, 0w15, 0w21 ]
  val k : Word.word Vector.vector =
    Vector.fromList [
      0wxd76aa478, 0wxe8c7b756, 0wx242070db, 0wxc1bdceee, 0wxf57c0faf, 0wx4787c62a,
      0wxa8304613, 0wxfd469501, 0wx698098d8, 0wx8b44f7af, 0wxffff5bb1, 0wx895cd7be,
      0wx6b901122, 0wxfd987193, 0wxa679438e, 0wx49b40821, 0wxf61e2562, 0wxc040b340,
      0wx265e5a51, 0wxe9b6c7aa, 0wxd62f105d, 0wx02441453, 0wxd8a1e681, 0wxe7d3fbc8,
      0wx21e1cde6, 0wxc33707d6, 0wxf4d50d87, 0wx455a14ed, 0wxa9e3e905, 0wxfcefa3f8,
      0wx676f02d9, 0wx8d2a4c8a, 0wxfffa3942, 0wx8771f681, 0wx6d9d6122, 0wxfde5380c,
      0wxa4beea44, 0wx4bdecfa9, 0wxf6bb4b60, 0wxbebfbc70, 0wx289b7ec6, 0wxeaa127fa,
      0wxd4ef3085, 0wx04881d05, 0wxd9d4d039, 0wxe6db99e5, 0wx1fa27cf8, 0wxc4ac5665,
      0wxf4292244, 0wx432aff97, 0wxab9423a7, 0wxfc93a039, 0wx655b59c3, 0wx8f0ccc92,
      0wxffeff47d, 0wx85845dd1, 0wx6fa87e4f, 0wxfe2ce6e0, 0wxa3014314, 0wx4e0811a1,
      0wxf7537e82, 0wxbd3af235, 0wx2ad7d2bb, 0wxeb86d391 ]
  val h0 = ref 0wx67452301
  val h1 = ref 0wxefcdab89
  val h2 = ref 0wx98badcfe
  val h3 = ref 0wx10325476
  val inputLen = Word.fromInt (String.size input)
  val bitLen : LargeWord.word = (Word.toLarge inputLen) * 0w8
  val padLen =
    let val rem = Word.mod (inputLen + 0w1, 0w64)
    in 0w1 + Word.mod (0w56 - rem, 0w64)
    end
  val paddedLen = inputLen + padLen + 0w8
  val msg : Word8.word Array.array = Array.array (Word.toInt paddedLen, 0w0)
  val inputBytes : Word8.word List.list = List.map (fn c => Word8.fromInt (Char.ord c)) (String.explode input)
  fun copyInput ([], _) = ()
    | copyInput (x::xs, i) = (Array.update (msg, i, x); copyInput (xs, i + 1))
  fun padInput i =
    if i = 8 then ()
    else (
      Array.update (msg,  Word.toInt(inputLen + padLen) + i,
        Word8.fromLargeWord (LargeWord.>> (bitLen, Word.fromInt (8 * i))));
         padInput (i + 1))
  fun iterate offset =
    if offset >= (Word.toInt paddedLen) then ()
    else
      let
        val w : Word.word Array.array = Array.array (16, 0w0)
        val a = ref (!h0)
        val b = ref (!h1)
        val c = ref (!h2)
        val d = ref (!h3)
        val temp = ref 0w0
        fun wrappingAdd (a, b) = Word.andb (a + b, 0wxffffffff)
        fun initW 16 = ()
          | initW i =
              let
                val w0 = Word.fromLargeWord (Word8.toLargeWord (Array.sub (msg, offset + i * 4)))
                val w1 = Word.fromLargeWord (Word8.toLargeWord (Array.sub (msg, offset + i * 4 + 1)))
                val w2 = Word.fromLargeWord (Word8.toLargeWord (Array.sub (msg, offset + i * 4 + 2)))
                val w3 = Word.fromLargeWord (Word8.toLargeWord (Array.sub (msg, offset + i * 4 + 3)))
                val wi = Word.orb (w0, Word.orb (Word.<< (w1, 0w8), Word.orb (Word.<< (w2, 0w16), Word.<< (w3, 0w24))))
              in
                (Array.update (w, i, wi); initW (i + 1))
              end
        fun innerLoop 64 = ()
          | innerLoop i =
              let
                val (f, g) =
                  if i < 16 then
                    (Word.orb (Word.andb (!b, !c), Word.andb (Word.notb (!b), !d)), i)
                  else if i < 32 then
                    (Word.orb (Word.andb (!d, !b), Word.andb (Word.notb (!d), !c)), Int.mod (5 * i + 1, 16))
                  else if i < 48 then
                    (Word.xorb (!b, Word.xorb (!c, !d)), Int.mod (3 * i + 5, 16))
                  else
                    (Word.xorb (!c, (Word.orb (!b, Word.notb (!d)))), Int.mod (7 * i, 16))
              in
                (temp := !d;
                 d := !c;
                 c := !b;
                 b := wrappingAdd (!b,
                        rotateLeft (wrappingAdd (!a,
                                      wrappingAdd (f,
                                        wrappingAdd (Vector.sub (k, i), Array.sub (w, g)))),
                                    Vector.sub (r, i)));
                 a := !temp;
                 innerLoop (i + 1))
              end
    in
      (initW 0;
       innerLoop 0; 
       h0 := wrappingAdd (!h0, !a);
       h1 := wrappingAdd (!h1, !b);
       h2 := wrappingAdd (!h2, !c);
       h3 := wrappingAdd (!h3, !d);
       iterate (offset + 64))
    end
  fun wordToHex w =
  let
    val hexDigits = "0123456789abcdef"
    fun nibble n = String.sub (hexDigits, Word8.toInt n)
    fun byteToHex b = String.implode [nibble (Word8.>> (b, 0w4)), nibble (Word8.andb (b, 0wx0f))]
    fun getByte i = Word8.fromLargeWord (Word.toLargeWord (Word.andb (Word.>> (w, Word.fromInt (8 * i)), 0wxff)))
  in
    String.concat [byteToHex (getByte 0), byteToHex (getByte 1), byteToHex (getByte 2), byteToHex (getByte 3)]
  end
  fun digestToHex (h0, h1, h2, h3) =
    String.concat [wordToHex h0, wordToHex h1, wordToHex h2, wordToHex h3]
in
  (copyInput (inputBytes, 0);
   Array.update (msg, Word.toInt inputLen, 0wx80); 
   padInput 0;
   iterate 0;
   digestToHex (!h0, !h1, !h2, !h3))
end

fun printErr s =
  (output (stdErr, s); flushOut stdErr);

fun process key =
let
  fun f n =
  let
    val tryKey = key ^ (Int.toString n)
    val digest = md5_digest tryKey
  in
    if String.extract (digest, 0, SOME 6) = "000000" then n
    else f (n + 1)
  end
in f 0
 end

val _ =
  let
    val args = CommandLine.arguments ()
  in
    if (List.length args) < 1 then
      (printErr ("usage: " ^ CommandLine.name () ^ " <key>\n") ;
       exit failure)
    else
      let
        val key = List.hd args
        val result = process key 
      in
        print ("result = " ^ (Int.toString result) ^ "\n")
      end
  end


fun readlines file = let
  val in_stream = TextIO.openIn file;
  fun loop stream =
    case TextIO.inputLine stream of
         SOME line => hd (String.tokens (fn c => c = #"\n") line) :: loop stream
       | NONE      => []
in
  (loop in_stream) before (TextIO.closeIn in_stream)
end;

fun sort [] = []
  | sort (x::[]) = [x]
  | sort (x::y::[]) = if x < y then [x, y] else [y, x]
  | sort (xs) = let
    val split = (xs, ((length xs) div 2));
    val sortedL = sort (List.take split)
    val sortedR = sort (List.drop split)
    fun merge l [] = l
      | merge [] r = r
      | merge (xl::l) (xr::r) = if xl < xr
               then xl::(merge l (xr::r))
               else xr::(merge (xl::l) r)
in
  merge sortedL sortedR
end;

fun pairsplit delim str = let
  val tokens = String.tokens (fn c => c = delim) str;
in
  (hd tokens, hd (tl tokens))
end;

fun printIntValue name i = (
  print name;
  print ": ";
  print (Int.toString i);
  print "\n"
  );

fun sum (x, y) = x + y;
fun mult (x, y) = x * y;

val sumList = List.foldr sum 0;
val multList = List.foldr mult 1;

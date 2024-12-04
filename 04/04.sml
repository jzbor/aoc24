use "../utils.sml";


val defaultFile = "./input.txt";
val argDescription = [("file", true, true)];
val argDescription = [
  {
    name = "file",
    required = true,
    argument = true,
    default = SOME "input.txt"
  }
];


(*** PART I ***)

fun readPuzzle infile = let
  val lines = readlines infile;
in
  Vector.map (Vector.fromList o String.explode) (Vector.fromList lines)
end;

fun vecToList (vec: 'a vector) = Vector.foldr (fn (a,l) => a::l) [] vec;

fun horizontalLines puzzle = vecToList (Vector.map vecToList puzzle);

fun verticalLines puzzle = let
  fun getFromLine i line = Vector.sub (line, i);
  fun getColumn i = Vector.map (getFromLine i) puzzle;
  fun collector (i, acc) = (getColumn i)::acc;
  val lines = foldr collector [] (range 0 (Vector.length (Vector.sub (puzzle, 0))));
in
  map vecToList lines
end;

fun diagonalLines1 puzzle = let
  val width = Vector.length (Vector.sub (puzzle, 0));
  val height = Vector.length puzzle;
  fun getVal (x, y) = Vector.sub (Vector.sub (puzzle, y), x);
  fun getLineFrom (x, y) = if x = width orelse y = height
                           then []
                           else (getVal (x, y))::(getLineFrom (x + 1, y + 1));
  fun collector (start, acc) = (getLineFrom start)::acc;
  val bottomIndices = ListPair.zip ((List.map (fn _ => 0) (range 1 height)), (range 1 height));
  val bottomHalf = foldr collector [] bottomIndices;
  val topIndices = ListPair.zip ((range 1 width), (List.map (fn _ => 0) (range 1 width)));
  val topHalf = foldr collector [] topIndices;
  val mid = [getLineFrom (0, 0)];
in
  topHalf@mid@bottomHalf
end;

fun diagonalLines2 puzzle = let
  val width = Vector.length (Vector.sub (puzzle, 0));
  val height = Vector.length puzzle;
  fun getVal (x, y) = Vector.sub (Vector.sub (puzzle, y), x);
  fun getLineFrom (x, y) = if x < 0 orelse y = height
                           then []
                           else (getVal (x, y))::(getLineFrom (x - 1, y + 1));
  fun collector (start, acc) = (getLineFrom start)::acc;
  val bottomIndices = ListPair.zip ((List.map (fn _ => (width - 1)) (range 1 height)), (range 1 height));
  val bottomHalf = foldr collector [] bottomIndices;
  val topIndices = ListPair.zip ((range 0 (width - 1)), (List.map (fn _ => 0) (range 0 (width - 1))));
  val topHalf = foldr collector [] topIndices;
  val mid = [getLineFrom (width - 1, 0)];
in
  topHalf@mid@bottomHalf
end;

fun allLines puzzle = let
  val horizontal = horizontalLines puzzle;
  val vertical = verticalLines puzzle;
  val diagonal1 = diagonalLines1 puzzle;
  val diagonal2 = diagonalLines2 puzzle;
in
  horizontal@(map List.rev horizontal)@vertical@(map List.rev vertical)@diagonal1@(map List.rev diagonal1)@diagonal2@(map List.rev diagonal2)
end;

fun countXMAS (#"X"::(#"M"::(#"A"::(#"S"::xs)))) = 1 + (countXMAS xs)
  | countXMAS (x::xs) = countXMAS xs
  | countXMAS [] = 0;

fun countLines lines = foldr op+ 0 (map countXMAS lines);

fun calc input () = foldr op+ 0 (map countXMAS (allLines input));


(*** PART II ***)
fun getWindow puzzle (x, y) = let
  fun getVal (x, y) = Vector.sub (Vector.sub (puzzle, y), x);
  val fields = [[(x, y), (x + 1, y), (x + 2, y)],
                [(x, y + 1), (x + 1, y + 1), (x + 2, y + 1)],
                [(x, y + 2), (x + 1, y + 2), (x + 2, y + 2)]];
in
  map (map getVal) fields
end;

fun getWindows puzzle = let
  val width = Vector.length (Vector.sub (puzzle, 0));
  val height = Vector.length puzzle;
  val xrange = range 0 (width - 2);
  val yrange = range 0 (height - 2);
  fun windowsForRow y = foldr (fn (x, l) => getWindow puzzle (x, y)::l) [] xrange;
in
  foldr (fn (y, l) => (windowsForRow y)@l) [] yrange
end;

fun checkWindow win = let
  fun getVal (x, y) = List.nth (List.nth (win, y), x);
  val coordsForward = [[(0, 0), (1, 1), (2, 2)], [(0, 2), (1, 1), (2, 0)]];
  val coords = coordsForward@(map List.rev coordsForward);
  val words = map (map getVal) coords;
  fun checkWord word = (word = [#"M", #"A", #"S"]);
in
  (List.length (List.filter checkWord words)) > 1
end;

fun countWindows puzzle () = List.length (List.filter checkWindow (getWindows puzzle));


(*** MAIN ***)
fun run input = (
  runCalc "Part 1" (calc input);
  runCalc "Part 2" (countWindows input)
  );

fun main () = let
  val args = parseArgs (CommandLine.name (), CommandLine.arguments ()) argDescription;
  val filename = getRequiredArg args "file";
  val input = readPuzzle filename;
in
  run input
end;

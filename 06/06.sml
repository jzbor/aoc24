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
fun isGuardSymbol #"^" = true
  | isGuardSymbol #">" = true
  | isGuardSymbol #"v" = true
  | isGuardSymbol #"<" = true
  | isGuardSymbol _ = false;

fun guardSymToDir #"^" = (0, ~1)
  | guardSymToDir #">" = (1, 0)
  | guardSymToDir #"v" = (0, 1)
  | guardSymToDir #"<" = (~1, 0);

fun nextDirection (0, ~1) = (1, 0)
  | nextDirection (1, 0) = (0, 1)
  | nextDirection (0, 1) = (~1, 0)
  | nextDirection (~1, 0) = (0, ~1);

fun applyDirection ((x, y), (dx, dy)) = (x + dx, y + dy);

fun readFile infile = let
  val lines = readlines infile;
  val guardMap = map String.explode lines;
  val width = List.length (List.nth (guardMap, 0));
  val height = List.length guardMap;
  val xCoords = range 0 width;
  val yCoords = range 0 height;
  val guardMapWithX = map (fn line => ListPair.zip (xCoords, line)) guardMap;
  val guardMapWithY = ListPair.zip (yCoords, guardMapWithX);
  val guardMap = map (fn (y, line) => (map (fn (x, e) => ((x, y), e)) line)) guardMapWithY;
  val mapped = List.concat guardMap;
  val obstacles = map (fn (coord, _) => coord) (List.filter (fn (coord, c) => c = #"#") mapped);
  val (guardCoord, guardSym) = valOf (List.find (fn (coord, c) => isGuardSymbol c) mapped);
  val guard = (guardCoord, guardSymToDir guardSym);
in
  ((width, height), guard, obstacles)
end;

fun guardOutOfBounds bounds guard = let
  val (width, height) = bounds;
  val ((x, y), _) = guard;
in
  x < 0 orelse y < 0 orelse x >= width orelse y >= height
end;

fun printStep ((x, y), dir) = print ("("^(Int.toString x)^","^(Int.toString y)^")\n");
fun printPos (x, y) = print ("("^(Int.toString x)^","^(Int.toString y)^")\n");

fun step guard obstacles = let
  val (pos, dir) = guard;
  val nextPos = applyDirection guard;
  fun isBlocked pos = Option.isSome (List.find (fn p => p = pos) obstacles);
in
  if isBlocked nextPos
  then step (pos, nextDirection dir) obstacles
  else (nextPos, dir)
end;

fun steps (bounds, guard, obstacles) = let
  val nextStep = step guard obstacles;
in
  if guardOutOfBounds bounds guard
  then []
  else guard::(steps (bounds, nextStep, obstacles))
end;

fun calc inputs () = (List.length o uniq o fst o ListPair.unzip o steps) inputs;

(*** PART II ***)
fun isLoop (bounds, guard, obstacles) pastPositions extraObstacle = let
  val obstacles = extraObstacle::obstacles;
  val nextStep = step guard obstacles;
in
  if guardOutOfBounds bounds guard
  then false
  else if Option.isSome (List.find (fn s => s = guard) pastPositions)
  then true
  else isLoop (bounds, nextStep, obstacles) (guard::pastPositions) extraObstacle
end;

fun countLoops (bounds, guard, obstacles) = let
  val (width, height) = bounds;
  val (guardPos, _) = guard;
  val allCoords = range2d (0, width) (0, height);
  fun restriction coord = coord <> guardPos andalso not (Option.isSome
    (List.find (fn e => e = coord) obstacles));
  val allowedCoords = List.filter restriction allCoords;
  val loops = List.filter (isLoop (bounds, guard, obstacles) []) allowedCoords;
in
  List.length loops
end;

fun calcLoops inputs () = countLoops inputs;

(*** MAIN ***)
fun run input = (
  runCalc "Part 1" (calc input);
  runCalc "Part 2" (calcLoops input)
  );

fun main () = let
  val args = parseArgs (CommandLine.name (), CommandLine.arguments ()) argDescription;
  val filename = getRequiredArg args "file";
  val input = readFile filename;
in
  run input
end;

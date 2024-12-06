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
  val obstacleList = map (map (fn (_, c) => c = #"#")) guardMap;
  val obstacles = Vector.fromList ((map Vector.fromList) obstacleList);
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

fun isBlocked obstacles (x, y) = Vector.sub (Vector.sub(obstacles, y), x)
  handle Subscript => false;

fun step guard obstacles = let
  val (pos, dir) = guard;
  val nextPos = applyDirection guard;
in
  if isBlocked obstacles nextPos
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
fun update2DVec vec (x, y) newVal = let
  val row = Vector.sub (vec, y);
  val updatedRow = Vector.update (row, x, newVal);
  val updatedVec = Vector.update (vec, y, updatedRow);
in
  updatedVec
end;

fun isLoop (bounds, guard, obstacles) pastPositions = let
  val nextStep = step guard obstacles;
in
  if guardOutOfBounds bounds guard
  then false
  else if Option.isSome (List.find (fn s => s = guard) pastPositions)
  then true
  else isLoop (bounds, nextStep, obstacles) (guard::pastPositions)
end;

fun checkLoopForObstacle (bounds, guard, obstacles) extraObstacle = let
  val newObstacles = update2DVec obstacles extraObstacle true;
in
  isLoop (bounds, guard, newObstacles) []
end;

fun countLoops (bounds, guard, obstacles) = let
  val (width, height) = bounds;
  val (guardPos, _) = guard;
  val allCoords = range2d (0, width) (0, height);
  val (stepCoords, _) = ListPair.unzip (steps (bounds, guard, obstacles));
  fun restriction coord = coord <> guardPos
    andalso not (isBlocked obstacles coord)
    andalso Option.isSome (List.find (fn e => e = coord) stepCoords); (* brought speedup of ~ x6 *)
  val allowedCoords = List.filter restriction allCoords;
  val loops = List.filter (checkLoopForObstacle (bounds, guard, obstacles)) allowedCoords;
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

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
fun readFile infile = let
  val lines = ((map (map (valOf o Int.fromString o str))) o (map String.explode) o readlines) infile;
  val vec = (Vector.fromList o (map Vector.fromList)) lines;
in
  vec
end;

fun getHeight trailmap (x, y) = Vector.sub (Vector.sub (trailmap, y), x);

fun trailheads trailmap = let
  val width = (Vector.length o Vector.sub) (trailmap, 0);
  val height = Vector.length trailmap;
  val coords = range2d (0, width) (0, height);
  fun borderFilter (x, y) = x = 0 orelse y = 0 orelse x = (width - 1) orelse y = (height - 1);
  fun borderFilter _ = true;
  fun trailheadFilter coord = (getHeight trailmap coord) = 0;
  val trailheadCoords = ((List.filter trailheadFilter) o (List.filter borderFilter)) coords;
in
  trailheadCoords
end;

fun neighbours trailmap (x, y) = let
  val width = (Vector.length o Vector.sub) (trailmap, 0);
  val height = Vector.length trailmap;
  val nbs = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)];
  fun borderFilter (x, y) = x >= 0 andalso y >= 0 andalso x < width andalso y < height;
in
  List.filter borderFilter nbs
end;

fun endPositionsFrom trailmap (x, y) = let
  val currentLevel = getHeight trailmap (x, y);
  val nbs = neighbours trailmap (x, y);
  fun levelFilter coords = (getHeight trailmap coords) - currentLevel = 1;
  val validNbs = List.filter levelFilter nbs;
in
  if currentLevel = 9
  then [(x, y)]
  else (List.concat o (map (endPositionsFrom trailmap))) validNbs
end;

fun score trailmap trailhead = (List.length o uniq) (endPositionsFrom trailmap trailhead);

fun calc1 trailmap () = let
  val heads = trailheads trailmap;
  val scores = map (score trailmap) heads;
in
  sumList scores
end;


(*** PART II ***)
fun score2 trailmap trailhead = List.length (endPositionsFrom trailmap trailhead);

fun calc2 trailmap () = let
  val heads = trailheads trailmap;
  val scores = map (score2 trailmap) heads;
in
  sumList scores
end;


(*** MAIN ***)
fun run input = (
  runCalc "Part 1" (calc1 input);
  runCalc "Part 2" (calc2 input)
  );

fun main () = let
  val args = parseArgs (CommandLine.name (), CommandLine.arguments ()) argDescription;
  val filename = getRequiredArg args "file";
  val input = readFile filename;
in
  run input
end;

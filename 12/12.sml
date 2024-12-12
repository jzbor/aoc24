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
  fun whitespaces c = c = #" ";
  val fields = ((map String.explode) o readlines) infile;
in
  fields
end;

fun fieldArray fields = let
  val withBool = map (map (fn e => (e, false))) fields;
  val arr = (Array.fromList o (map Array.fromList)) withBool;
in
  arr
end;

fun get arr (x, y) = Array.sub ((Array.sub (arr, y)), x);
fun set arr (x, y) v = Array.update ((Array.sub (arr, y)), x, v);
fun bounds arr = (Array.length (Array.sub (arr, 0)), Array.length arr);
fun getVeggie arr coord = fst (get arr coord);
fun getVisited arr coord = snd (get arr coord);

fun crawl arr (x, y) = if getVisited arr (x, y)
                       then (0, 0)
                       else let
                         val veggie = getVeggie arr (x, y);
                         val _ = set arr (x, y) (veggie, true);
                         fun veggieFilter coord = getVeggie arr coord = veggie;
                         val nbs = ((List.filter veggieFilter) o (neighbours2D (bounds arr))) (x, y);
                         val fences = 4 - (List.length nbs);
                         val (innerFields, innerFences) = ListPair.unzip (map (crawl arr) nbs);
in (1 + (sumList innerFields), fences + (sumList innerFences)) end;

fun calc1 fields () = let
  val arr = fieldArray fields;
  val (width, height) = bounds arr;
  val coords = range2d (0, width) (0, height);
  val values = map (crawl arr) coords;
  fun score (fields, fences) = fields * fences;
in
  sumList (map score values)
end;


(*** PART II ***)
fun checkForeignNeighbour arr origCoord nCoord = let
  val veggie = getVeggie arr origCoord;
  fun visitedFilter coord = getVisited arr coord;
  fun veggieFilter coord = getVeggie arr coord = veggie handle Subscript => false;
  fun veggieNonFilter coord = not (veggieFilter coord);
  val nbs = ((List.filter visitedFilter) o (List.filter veggieFilter) o neighbours2DUB) origCoord;
  val nbsnbs = ((List.filter veggieNonFilter) o neighbours2DUB) nCoord;
  fun registeredAsEdge n = List.exists (isNeighbour2D n) nbs;
in not (List.exists registeredAsEdge nbsnbs) end;

fun fixCommonEdge arr origCoord nCoord = let
  val veggie = getVeggie arr origCoord;
  fun visitedFilter coord = getVisited arr coord;
  fun veggieFilter coord = getVeggie arr coord = veggie handle Subscript => false;
  fun veggieNonFilter coord = not (veggieFilter coord);
  val nbs = ((List.filter visitedFilter) o (List.filter veggieFilter) o neighbours2DUB) origCoord;
  val nbsnbs = ((List.filter veggieNonFilter) o neighbours2DUB) nCoord;
  fun registeredAsEdge n = List.exists (isNeighbour2D n) nbs;
in
  List.length (List.filter registeredAsEdge nbsnbs) > 1
end;

fun crawl2 arr (x, y) = if getVisited arr (x, y)
                       then (0, 0)
                       else let
                         val veggie = getVeggie arr (x, y);
                         val _ = set arr (x, y) (veggie, true);
                         fun veggieFilter coord = getVeggie arr coord = veggie handle Subscript => false;
                         fun koFilter coord = checkForeignNeighbour arr (x, y) coord;
                         val (nbs, others) = ((List.partition veggieFilter) o neighbours2DUB) (x, y);
                         val sides = (List.length o (List.filter (checkForeignNeighbour arr (x, y)))) others;
                         val fixes = (List.length o (List.filter (fixCommonEdge arr (x, y)))) others;
                         val (innerFields, innerSides) = ListPair.unzip (map (crawl2 arr) nbs);
in (1 + (sumList innerFields), sides + (sumList innerSides) - fixes) end;

fun calc2 fields () = let
  val arr = fieldArray fields;
  val (width, height) = bounds arr;
  val coords = range2d (0, width) (0, height);
  val values = map (crawl2 arr) coords;
  fun score (fields, fences) = fields * fences;
in
  sumList (map score values)
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

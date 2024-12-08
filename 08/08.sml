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
  val lines = ((map String.explode) o readlines) infile;
  val width = (List.length o hd) lines;
  val height = List.length lines;
  val withCoords = ListPair.zip (range2d (0, width) (0, height), List.concat lines);
  val nodes = List.filter (fn (_, c) => c <> #".") withCoords;
in
  (nodes, (width, height))
end;

fun labels nodes = (uniq o (map snd)) nodes;

fun nodesForLabel label = (map fst) o (List.filter (fn c => snd c = label))

fun antiNodesForTuple ((x1, y1), (x2, y2)) = let
  val dx = x2 - x1;
  val dy = y2 - y1;
  val antiNode1 = (x1 - dx, y1 - dy);
  val antiNode2 = (x2 + dx, y2 + dy);
in
  [antiNode1, antiNode2]
end;

fun antiNodes (nodes, bounds) label = let
  val perms = ((List.filter op<>) o (map listToTuple)) (permutations (nodesForLabel label nodes) 2);
  val antiNodes = (uniq o List.concat o (map antiNodesForTuple)) perms;
in
  antiNodes
end;

fun allAntiNodes (nodes, bounds) = let
  val (width, height) = bounds;
  val nodeLabels = labels nodes;
  val antis = (uniq o List.concat o (map (antiNodes (nodes, bounds)))) nodeLabels;
  fun filterRange (x, y) = x >= 0 andalso y >= 0 andalso x < width andalso y < height;
  val antisInRange = List.filter filterRange antis;
in
  antisInRange
end;

fun calc1 input () = (List.length o allAntiNodes) input;

(*** PART II ***)
fun antiNodesForTuple2 (((x1, y1), (x2, y2)), factor) = let
  val dx = factor * (x2 - x1);
  val dy = factor * (y2 - y1);
  val antiNode1 = (x1 - dx, y1 - dy);
  val antiNode2 = (x2 + dx, y2 + dy);
in
  [antiNode1, antiNode2]
end;

fun antiNodes2 (nodes, bounds) label = let
  val (width, height) = bounds;
  val perms = ((List.filter op<>) o (map listToTuple)) (permutations (nodesForLabel label nodes) 2);
  fun withFactors perm = map (fn n => (perm, n)) (range 0 (if width > height then width else height));
  val permsWithFactors = List.concat (map withFactors perms);
  val antiNodes = (uniq o List.concat o (map antiNodesForTuple2)) permsWithFactors;
in
  antiNodes
end;

fun allAntiNodes2 (nodes, bounds) = let
  val (width, height) = bounds;
  val nodeLabels = labels nodes;
  val antis = (uniq o List.concat o (map (antiNodes2 (nodes, bounds)))) nodeLabels;
  fun filterRange (x, y) = x >= 0 andalso y >= 0 andalso x < width andalso y < height;
  val antisInRange = List.filter filterRange antis;
in
  antisInRange
end;

fun calc2 input () = (List.length o allAntiNodes2) input;


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

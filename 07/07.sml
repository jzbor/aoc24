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
  val lines = readlines infile;
  fun getMeasurement line = (valOf o Int.fromString o hd) (String.tokens (fn c => c = #":") line);
  fun getOperands line = ((map valOf) o (map Int.fromString) o tl o (String.tokens (fn c => c = #" "))) line;
in
  ListPair.zip (map getMeasurement lines, map getOperands lines)
end;

fun calcCombination (x::y::[]) (oper::[]) = oper (x, y)
  | calcCombination (x::y::xs) (oper::ops) = calcCombination (oper (x, y)::xs) ops;

fun permutations _ 0 = [[]]
  | permutations components n = let
    val tailPermutations = permutations components (n - 1)
    fun permutationsWith comp = map (fn perm => comp::perm) tailPermutations;
in
  (List.concat o (map permutationsWith)) components
end;

fun checkLine operations (expected, operands) = let
  val nOperators = (List.length operands) - 1;
  val opPermutations = permutations operations nOperators;
  val results = map (calcCombination operands) opPermutations;
in
  Option.isSome (List.find (fn n => n = expected) results)
end;

val lineFilter1 = (List.filter (checkLine [op+, op*]));

fun calc1 inputs () = ((foldr op+ 0) o fst o ListPair.unzip o lineFilter1) inputs;


(*** PART II ***)
fun concatenation (a, b) = (valOf o Int.fromString) ((Int.toString a)^(Int.toString b));

val lineFilter2 = (List.filter (checkLine [op+, op*, concatenation]));

fun calc2 inputs () = ((foldr op+ 0) o fst o ListPair.unzip o lineFilter2) inputs;


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

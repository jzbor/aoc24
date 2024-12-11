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
  val stones = ((map valOf) o (map Int.fromString) o (String.tokens whitespaces) o hd o readlines) infile;
in
  stones
end;

fun blink [] = []
  | blink (0::xs) = 1::blink xs
  | blink (x::xs) = let
    val digits = Int.toString x;
    fun left digits = (valOf o Int.fromString o String.substring) (digits, 0, (String.size digits) div 2);
    fun right digits = (valOf o Int.fromString o String.extract) (digits, (String.size digits) div 2, NONE);
in
  if ((String.size digits) mod 2) = 0
  then (left digits :: right digits :: blink xs)
  else (x * 2024) :: blink xs
end;

fun compN f 1 = f
  | compN f n = f o (compN f (n - 1));

fun calc1 stones () = List.length (compN blink 25 stones);


(*** PART II ***)
val memTable: ((int*int), int) TreeMap.tmap ref = ref TreeMap.empty;
fun setMemo pair = memTable := TreeMap.insertPairIdx (!memTable) pair;
fun getMemo k = TreeMap.findPairIdx (!memTable) k;

val left = (valOf o Int.fromString o (fn d => String.substring (d, 0, (String.size d) div 2)) o Int.toString)
val right = (valOf o Int.fromString o (fn d => String.extract (d, (String.size d) div 2, NONE)) o Int.toString);
fun evenNrDigits x = ((String.size (Int.toString x)) mod 2) = 0

fun stonesAtDepth 0 _ = 1
  | stonesAtDepth n 0 = stonesAfterBlinks (n - 1) 1
  | stonesAtDepth n x = if evenNrDigits x
                        then (stonesAfterBlinks (n - 1) (left x)) + (stonesAfterBlinks (n - 1) (right x))
                        else stonesAfterBlinks (n - 1) (x * 2024)
and stonesAfterBlinks n x = case getMemo (n, x) of
                                 SOME v => v
                               | NONE => let val v = stonesAtDepth n x; in setMemo ((n, x), v); v end;



fun calc stones n () = List.length (compN blink n stones);
fun calcFast stones n () = (sumList o List.map (stonesAfterBlinks n)) stones;


(*** MAIN ***)
fun run input = (
  runCalc "Part 1 (25 iter)" (calc input 25);
  runCalc "Part 1 (25 iter, fast)" (calcFast input 25);
  runCalc "Part 2 (75 iter, fast)" (calcFast input 75)
  );

fun main () = let
  val args = parseArgs (CommandLine.name (), CommandLine.arguments ()) argDescription;
  val filename = getRequiredArg args "file";
  val input = readFile filename;
in
  run input
end;

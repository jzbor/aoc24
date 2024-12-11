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
  (* print ("blink: "^(Int.toString ((List.length xs) + 1))^"\n"); *)
  if ((String.size digits) mod 2) = 0
  then (left digits :: right digits :: blink xs)
  else (x * 2024) :: blink xs
end;

fun compN f 1 = f
  | compN f n = f o (compN f (n - 1));

fun calc1 stones () = List.length (compN blink 25 stones);

(*** PART II ***)
fun stonesAfterBlinks 0 _ = 1
  | stonesAfterBlinks n 0 = stonesAfterBlinks (n - 1) 1
  | stonesAfterBlinks n x = let
    val digits = Int.toString x;
    fun left digits = (valOf o Int.fromString o String.substring) (digits, 0, (String.size digits) div 2);
    fun right digits = (valOf o Int.fromString o String.extract) (digits, (String.size digits) div 2, NONE);
in
  if ((String.size digits) mod 2) = 0
  then (stonesAfterBlinks (n - 1) (left digits)) + (stonesAfterBlinks (n - 1) (right digits))
  else stonesAfterBlinks (n - 1) (x * 2024)
end;

fun calc2 stones () = (sumList o List.map (stonesAfterBlinks 75)) stones;


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

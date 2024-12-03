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
fun matchToken tk s = if String.isPrefix tk (String.implode s)
                      then SOME (List.drop (s, String.size tk))
                      else NONE

val matchMulTk = matchToken "mul";

fun matchDigits [] = ([], [])
  | matchDigits (d::rest) = if Char.isDigit d
                            then (fn (dgts, r) => (d::dgts, r)) (matchDigits rest)
                            else ([], d::rest);

fun parseDigits input = let
  val (digits, rest) = (matchDigits input);
in
  case digits of
       [] => NONE
     | dgts => SOME (Int.fromString (String.implode dgts), rest)
end;

fun matchChar m [] = NONE
  | matchChar m (c::rest) = if m = c
                            then SOME rest
                            else NONE

fun parseMulInstruction input = let
  val rest_opt = matchMulTk input;
  val rest_opt = Option.mapPartial (matchChar #"(") rest_opt;
  val dgts1_rest_opt = Option.mapPartial parseDigits rest_opt;
  val rest_opt = Option.map (fn (_, r) => r) dgts1_rest_opt;
  val rest_opt = Option.mapPartial (matchChar #",") rest_opt;
  val dgts2_rest_opt = Option.mapPartial parseDigits rest_opt;
  val rest_opt = Option.map (fn (_, r) => r) dgts2_rest_opt;
  val rest_opt = Option.mapPartial (matchChar #")") rest_opt;
  val dgts1_opt = Option.mapPartial (fn (d, _) => d) dgts1_rest_opt;
  val dgts2_opt = Option.mapPartial (fn (d, _) => d) dgts2_rest_opt;
in
  case rest_opt of NONE => NONE
     | SOME rest => SOME ((valOf dgts1_opt, valOf dgts2_opt), rest)
end;

fun parseChars1 [] = []
  | parseChars1 (c::rest) = case parseMulInstruction (c::rest) of
                                 SOME (dgts, rest) => dgts::(parseChars1 rest)
                               | NONE => parseChars1 rest;

val parse1 = parseChars1 o String.explode;

fun sumUp [] = 0
  | sumUp ((a, b)::xs) = (a * b) + (sumUp xs);

fun calc parse input () = List.foldr (fn ((a, b), acc) => (a * b) + acc) 0 (parse input);


(*** PART II ***)
fun parseSwitchInstruction tk enable input = let
  val rest_opt = matchToken tk input;
  val rest_opt = Option.mapPartial (matchChar #"(") rest_opt;
  val rest_opt = Option.mapPartial (matchChar #")") rest_opt;
in case rest_opt of SOME _ => enable
      | None => not enable
end;


fun parseChars2 enabled [] = []
  | parseChars2 enabled (c::rest) = case parseMulInstruction (c::rest) of
                                        SOME (dgts, rest) => if enabled
                                                             then dgts::(parseChars2 enabled rest)
                                                             else parseChars2 enabled rest
                                      | NONE => case matchToken "do()" (c::rest) of
                                                     SOME rest => parseChars2 true rest
                                                   | NONE  => case matchToken "don't()" (c::rest) of
                                                                   SOME rest => parseChars2 false rest
                                                                 | NONE => parseChars2 enabled rest;

val parse2 = (parseChars2 true) o String.explode;


(*** MAIN ***)
fun run input = (
  runCalc "Part 1" (calc parse1 input);
  runCalc "Part 2" (calc parse2 input)
  );

fun main () = let
  val args = parseArgs (CommandLine.name (), CommandLine.arguments ()) argDescription;
  val filename = getRequiredArg args "file";
  val input = readInput filename;
in
  run input
end;

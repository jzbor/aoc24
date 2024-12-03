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

fun parse1 s = parseChars1 (String.explode s);

fun calc [] = 0
  | calc ((a, b)::xs) = (a * b) + (calc xs);


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

fun parse2 s = parseChars2 true (String.explode s);


fun readInput infile = String.concat (readlines infile);

fun run input = let
  val part1 = calc (parse1 input);
  val part2 = calc (parse2 input);
in
  printIntValue "Solution part 1" part1;
  printIntValue "Solution part 2" part2
end;


fun main () = let
  val args = parseArgs (CommandLine.name (), CommandLine.arguments ()) argDescription;
  val filename = getRequiredArg args "file";
  val input = readInput filename;
in
  run input
end

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


fun readReports file = let
  val lines = readlines file;
  val strReports = map (split #" ") lines;
  val mapToInt = map (fn x => Option.valOf (Int.fromString x));
in
  List.map (fn r => mapToInt r) strReports
end;


fun monotonous cmp [] = true
  | monotonous cmp (x::[]) = true
  | monotonous cmp (x::y::xs) = cmp (x, y) andalso monotonous cmp (y::xs);

val increasing = monotonous op<;
val decreasing = monotonous op>;


fun checkDiff x y = let
  val diff = abs (x - y);
in
  diff > 0 andalso diff <= 3
end;

fun checkReport r = let
  fun checkSteps [] = true
    | checkSteps (y::[]) = true
    | checkSteps (x::y::xs) = checkDiff x y andalso checkSteps (y::xs)
in
  (increasing r orelse decreasing r) andalso checkSteps r
end;

fun checkReports lr = (length (List.filter checkReport lr));


fun checkDampenedReport r = let
  fun checkElement i = checkReport ((List.take (r, i)) @ (List.drop (r, i + 1)));
in
  List.exists checkElement (range 0 (List.length r))
end;

fun checkDampenedReports lr = (length (List.filter checkDampenedReport lr));


fun run infile = let
  val part1 = checkReports (readReports infile);
  val part2 = checkDampenedReports (readReports infile);
in
  printIntValue "Solution part 1" part1;
  printIntValue "Solution part 2" part2
end;


fun main () = let
  val args = parseArgs (CommandLine.name (), CommandLine.arguments ()) argDescription;
  val filename = getRequiredArg args "file";
in
  run filename
end

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
  fun filterRules line = Option.isSome (List.find (fn c => c = #"|") (String.explode line))
  val rulesStr = List.filter filterRules lines;
  fun pairToInt (x, y) = (valOf (Int.fromString x), valOf (Int.fromString y));
  val rules = map pairToInt (map (pairsplit #"|") rulesStr);
  fun filterJobs line = Option.isSome (List.find (fn c => c = #",") (String.explode line))
  val jobsStr = List.filter filterJobs lines;
  val jobs = map (map (valOf o Int.fromString)) (map (String.tokens (fn c => c = #",")) jobsStr);
in
  (rules, jobs)
end;

fun indexOf e l = let
  val tupleOpt = List.find (fn (i, x) => x = e) (enumerate l);
in
  Option.map (fn (i, x) => i) tupleOpt
end;

fun checkRule pages (a, b) = case (indexOf a pages, indexOf b pages) of
                                  (SOME i, SOME j) => i < j
                                | _ => true;

fun checkRules rules pages = List.all (checkRule pages) rules;

fun middleElem l = List.nth (l, (List.length l) div 2);

fun calc (rules, jobs) () = let
  val validJobs = List.filter (checkRules rules) jobs;
  val middles = map middleElem validJobs;
in
  foldr op+ 0 middles
end;

(*** PART II ***)
fun isValidPage rules pagesWithP p = let
  val pagesWithoutP = List.filter (fn x => not (x = p)) pagesWithP;
  fun pred x = checkRules rules [p, x];
in
  List.all pred pagesWithoutP
end;

fun constructFrom rules [] = []
  | constructFrom rules job = let
    val (avail, rest) = List.partition (isValidPage rules job) job;
    val chosen = hd avail;
in
  chosen::(constructFrom rules ((tl avail)@rest))
end;


fun calcFixed (rules, jobs) () = let
  val fixedJobs = map (constructFrom rules) (List.filter (fn j => not (checkRules rules j)) jobs);
  val middles = map middleElem fixedJobs;
in
  foldr op+ 0 middles
end;


(*** MAIN ***)
fun run input = (
  runCalc "Part 1" (calc input);
  runCalc "Part 2" (calcFixed input)
  );

fun main () = let
  val args = parseArgs (CommandLine.name (), CommandLine.arguments ()) argDescription;
  val filename = getRequiredArg args "file";
  val input = readFile filename;
in
  run input
end;

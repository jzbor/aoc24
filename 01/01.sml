use "../utils.sml";


val infile = "./input.txt";


fun readLists file = let
  val lines = readlines file;
  val (listL, listR) = ListPair.unzip (List.map (pairsplit #" ") lines);
  val mapToInt = map (fn x => Option.valOf (Int.fromString x));
in
  (mapToInt listL, mapToInt listR)
end;


fun solve infile = let
  val (listL, listR) = readLists infile;
  val sorted = (sort listL, sort listR);
  fun diff (x, y) = Int.abs (x - y);
in
  sumList (ListPair.map diff sorted)
end;

fun solve2 infile = let
  val (listL, listR) = readLists infile;
  fun count [] x = 0
    | count (hr::tr) x = (if hr = x then 1 else 0) + (count tr x);
  val occurrences = List.map (count listR) listL;
  val multiplied = ListPair.map (fn (a, b) => a * b) (listL, occurrences);
in
  sumList multiplied
end;


fun run infile = let
  val part1 = solve infile;
  val part2 = solve2 infile;
in
  printIntValue "Solution part 1" part1;
  printIntValue "Solution part 2" part2
end;


fun main args = run (hd (CommandLine.arguments args))

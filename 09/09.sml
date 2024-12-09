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
  val lines = ((map (valOf o Int.fromString o str)) o String.explode o hd o readlines) infile;
  fun unpack _ _ [] = []
    | unpack true id (0::xs) = unpack false (id + 1) xs
    | unpack true id (n::xs) = id::(unpack true id (n - 1::xs))
    | unpack false id (0::xs) = unpack true id xs
    | unpack false id (n::xs) = ~1::(unpack false id (n - 1::xs));
in
  unpack true 0 lines
end;

fun insert e [] = [e]
  | insert e (x::xs) = if x < 0
                       then e::xs
                       else x::(insert e xs);

fun isCompacted [] = true
  | isCompacted (x::xs) = if x < 0
                          then List.all (fn e => e < 0) xs
                          else isCompacted xs;

fun lastFileBlock dl (x::xs) = if x >= 0 andalso List.all (fn e => e < 0) xs
                               then (List.rev dl, x)
                               else lastFileBlock (x::dl) xs;

fun compact disk = let
  val (rest, last) = lastFileBlock [] disk;
  val inserted = insert last rest;
in
  if List.exists (fn e => e < 0) inserted
  then compact inserted
  else inserted
end;

fun calc1 disk () = let
  val finalDisk = compact disk;
  val indices = range 0 (List.length finalDisk);
  val scores = ((List.map op*) o ListPair.zip) (indices, finalDisk);
in
  sumList scores
end;

(*** PART II ***)
fun firstNSatisfy _ 0 _ = true
  | firstNSatisfy _ n [] = false
  | firstNSatisfy pred n (x::xs) = if pred x
                                   then firstNSatisfy pred (n - 1) xs
                                   else false;

fun insertFile e n [] = [e]
  | insertFile e n (x::xs) = if x < 0 andalso firstNSatisfy (fn e => e < 0) n (x::xs)
                             then (List.tabulate (n, (fn _ => e)))@(List.drop (x::xs, n))
                             else x::(insertFile e n xs);

fun takeFile l id = let
  fun blockFilter c = c = id;
  fun replaceBlock c = if c = id then ~1 else c;
  val fileSize = (List.length o (List.filter blockFilter)) l;
  val rest = map replaceBlock l;
in
  (rest, (id, fileSize))
end;

fun defrag disk id = let
  val (rest, (last, lastSize)) = takeFile disk id;
  val inserted = insertFile last lastSize rest;
in
  if id > 0
  then defrag inserted (id - 1)
  else inserted
end;

fun calc2 disk () = let
  val finalDisk = defrag disk (maximum disk);
  val indices = range 0 (List.length finalDisk);
  fun filter (_, n) = n >= 0;
  val scores = ((List.map op*) o (List.filter filter) o ListPair.zip) (indices, finalDisk);
in
  sumList scores
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

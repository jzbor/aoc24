(*** Process ***)
fun fail () = (OS.Process.exit OS.Process.failure; ());
fun succeed () = (OS.Process.exit OS.Process.success; ());

fun die msg = (
  print "Error: ";
  print msg;
  print "\n";
  fail ()
  );


(*** Lists and Pairs ***)
fun sort [] = []
  | sort (x::[]) = [x]
  | sort (x::y::[]) = if x < y then [x, y] else [y, x]
  | sort (xs) = let
    val split = (xs, ((length xs) div 2));
    val sortedL = sort (List.take split)
    val sortedR = sort (List.drop split)
    fun merge l [] = l
      | merge [] r = r
      | merge (xl::l) (xr::r) = if xl < xr
               then xl::(merge l (xr::r))
               else xr::(merge (xl::l) r)
in
  merge sortedL sortedR
end;

fun split delim = String.tokens (fn c => c = delim);

fun pairsplit delim str = let
  val tokens = String.tokens (fn c => c = delim) str;
in
  (hd tokens, (hd o tl) tokens)
  handle Empty => (die ("Not a pair: '" ^ str ^ "'"); ("", ""))
end;

val sumList = foldl op+ 0;
val multList = foldl op* 1;

fun range x y = if x = y
                then []
                else x::(range (x + 1) y);

fun rangeDesc x y = if x = 0
                    then []
                    else x::(range (x - 1) y);

fun maximum (x::[]) = x
  | maximum (x::xs) = let
  val innerMax = maximum xs;
in if x > innerMax then x else innerMax end;

fun range2d (fromX, toX) (fromY, toY) = let
  val rangeX = range fromX toX;
  val rangeY = range fromY toY;
in
  List.concat (map (fn y => map (fn x => (x, y)) rangeX) rangeY)
end;


fun enumerate l = ListPair.zip (range 0 (List.length l), l);

fun hdOr other [] = other
  | hdOr other (x::xs) = x;

fun tlOr other [] = other
  | tlOr other (x::xs) = xs;

fun indexOf e l = let
  val tupleOpt = List.find (fn (i, x) => x = e) (enumerate l);
in
  Option.map (fn (i, x) => i) tupleOpt
end;

fun mapConst c = map (fn _ => c);

fun middleElem l = List.nth (l, (List.length l) div 2);

fun uniq [] = []
  | uniq (x::xs) = x::uniq (List.filter (fn y => y <> x) xs);

fun fst (x, _) = x;
fun snd (_, y) = y;

fun permutations _ 0 = [[]]
  | permutations components n = let
    val tailPermutations = permutations components (n - 1)
    fun permutationsWith comp = map (fn perm => comp::perm) tailPermutations;
in
  (List.concat o (map permutationsWith)) components
end;

fun listToTuple (x::y::[]) = (x, y);

fun compareIntPair ((x1, y1), (x2, y2)) = case Int.compare (x1, x2) of
                                               EQUAL => Int.compare (y1, y2)
                                             | other => other;


(*** IO ***)
fun readlines file = let
  val in_stream = TextIO.openIn file
    handle Io => (die ("Unable to open file '" ^ file ^ "'"); TextIO.openIn file);
  fun loop stream =
    case TextIO.inputLine stream of
         SOME line => hdOr "" (String.tokens (fn c => c = #"\n") line) :: loop stream
       | NONE      => []
in
  (loop in_stream) before (TextIO.closeIn in_stream)
end;

fun readInput infile = (concat o readlines) infile;

fun printIntValue name i = (
  print name;
  print ":\t ";
  print (Int.toString i);
  print "\n"
  );


(*** ARGUMENT PARSING ***)
type argspec = {
name: string,
required: bool,
argument: bool,
default: string option
}

fun parseArgs (name, args: string list) (expectedArgs: argspec list) = let
  fun fmtArg (param: argspec) = if #argument param
                     then case #default param of
                               SOME dfl => " <\"" ^ dfl ^ "\">"
                             | NONE => " <" ^ (#name param) ^ ">"
                     else "";
  fun fmtReq param = " --" ^ (#name param) ^ (fmtArg param)
  fun fmtOpt param = " [--" ^ (#name param) ^ (fmtArg param) ^ "]"
  fun printRemainingUsage [] = ()
    | printRemainingUsage ((param: argspec)::rest) = (
    if (#required param andalso not (Option.isSome (#default param)))
    then print (fmtReq param)
    else print (fmtOpt param);
    printRemainingUsage rest
    );
  fun printUsage () = (
    print "Usage: ";
    print name;
    printRemainingUsage expectedArgs;
    print "\n"
    );
  val required = List.filter (fn param => (#required param)) expectedArgs;
  fun findExpected name = List.find (fn param => "--"^(#name param) = name) expectedArgs;
  fun missingParameter name = (
    print "\nMissing Parameter: --";
    print name;
    print "\n";
    printUsage ();
    fail ()
    );
  fun missingArgument name = (
    print "\nMissing Parameter for ";
    print name;
    print "\n";
    printUsage ();
    fail ()
    );
  fun unknownParameter name = (
    print "\nUnknown Parameter: ";
    print name;
    print "\n";
    printUsage ();
    fail ()
    );
  fun parse [] = []
    | parse ("-h"::[]) = (printUsage (); succeed (); [])
    | parse ("--help"::[]) = (printUsage (); succeed (); [])
    | parse (x::[]) = (case findExpected x of
                         SOME param => if #argument param
                                       then (missingArgument x; [])
                                       else [(#name param, NONE)]
                       | NONE => (unknownParameter x; []))
    | parse (x::xa::xs) = (case findExpected x of
                               SOME param => if #argument param
                                             then (#name param, SOME xa)::(parse xs)
                                             else (#name param, NONE)::(parse (xa::xs))
                             | NONE => (unknownParameter x; []));
  val parsed = parse args;
  fun checkArg ((arg: argspec), parsed) = (case List.find (fn (x, _) => x = (#name arg)) parsed of
                                                SOME _ => parsed
                                              | NONE => (case #default arg of
                                                              SOME (d: string) => ((#name arg), SOME d)::parsed
                                                            | NONE =>
                                                                (missingParameter (#name arg); [])));
  fun checkRequired parsedArgs = foldl checkArg parsedArgs required;
in
  checkRequired parsed
end;

fun checkFlag parsedArgs name = let
  fun predicate (x, _) = x = name;
in
  case List.find predicate parsedArgs of
       SOME _ => true
     | NONE => false
end;

fun getArg parsedArgs name = let
  fun predicate (x, _) = x = name;
in case List.find predicate parsedArgs of
        SOME (k, v) => SOME (valOf v)
      | NONE => NONE
end;

fun getRequiredArg parsedArgs name = let
  fun predicate (x, _) = x = name;
  val (_, v) = valOf (List.find predicate parsedArgs);
in
  valOf v
end;



(*** Benchmarking ***)
fun timeToString time = let
  val seconds = Time.toSeconds time;
  val milliseconds = Time.toMilliseconds time;
  val microseconds = Time.toMicroseconds time;
  val nanoseconds = Time.toNanoseconds time;

  val precisionDecider = 10;
  fun tts "s" = if Time.toSeconds time >= precisionDecider
                then (IntInf.toString (Time.toSeconds time)) ^ "s"
                else tts "ms"
    | tts "ms" = if Time.toMilliseconds time > precisionDecider
                 then (IntInf.toString (Time.toMilliseconds time)) ^ "ms"
                 else tts "us"
    | tts "us" = if Time.toMicroseconds time > precisionDecider
                 then (IntInf.toString (Time.toMicroseconds time)) ^ "us"
                 else tts "ns"
    | tts "ns" = (IntInf.toString (Time.toNanoseconds time)) ^ "ns"
    | tts other = "<error: unknown unit '" ^ other ^ "'>";

in
  tts "s"
end;

type timerec = {
  nongc : {
    usr : Time.time,
    sys : Time.time
  },
  gc : {
    usr : Time.time,
    sys : Time.time
  }
}

fun printTimes (times: timerec) = let
  open Time;
  val gcUsr = #usr (#gc times);
  val gcSys = #sys (#gc times);
  val nongcUsr = #usr (#nongc times);
  val nongcSys = #sys (#nongc times);
in
  print "-- TIMES ---------------------------\n";
  print "           [ User + System = Total ]\n";
  print ("    Program: " ^ (timeToString nongcUsr) ^ " + " ^ (timeToString nongcSys) ^ " = " ^ (timeToString (nongcUsr + nongcSys)) ^ "\n");
  print ("    GC:      " ^ (timeToString gcUsr) ^ " + " ^ (timeToString gcSys) ^ " = " ^ (timeToString (gcUsr + gcSys)) ^ "\n");
  print ("    Total:   " ^ (timeToString (gcUsr + nongcUsr)) ^ " + " ^ (timeToString (gcSys + nongcSys)) ^ " = " ^ (timeToString (gcUsr + gcSys + nongcUsr + nongcSys)) ^ "\n");
  print "------------------------------------\n"
end;

fun bench f = let
  val timer = Timer.startCPUTimer ();
  val ret = f ();
  val time = Timer.checkCPUTimes timer;
in
  (ret, time)
end;

fun runCalc label f = let
  val (v, time) = bench f
in
  print "\n";
  printIntValue label v;
  printTimes time
end;


(*** Tree Map ***)
structure TreeMap = struct
  datatype ('a, 'b) tmap = NODE  of ('a * 'b) * ('a, 'b) tmap * ('a, 'b) tmap
                         | LEAF

  val empty = LEAF

  fun insert cmp LEAF new = NODE (new, LEAF, LEAF)
    | insert cmp (NODE ((k, v), left, right)) (kn, vn) = let
      val newNode = case cmp (kn, k) of
                         LESS => NODE ((k, v), insert cmp left (kn, vn), right)
                       | GREATER => NODE ((k, v), left, insert cmp right (kn, vn))
                       | EQUAL => NODE ((kn, vn), left, right);
  in newNode end

  fun find cmp LEAF _ = NONE
    | find cmp (NODE ((k, v), left, right)) kn = let
      val node = case cmp (kn, k) of
                      LESS => find cmp left kn
                    | GREATER => find cmp right kn
                    | EQUAL => SOME v;
  in node end

  fun insertIntIdx tmap = insert Int.compare tmap
  fun findIntIdx tmap = find Int.compare tmap
  fun insertPairIdx tmap = insert compareIntPair tmap
  fun findPairIdx tmap = find compareIntPair tmap
end


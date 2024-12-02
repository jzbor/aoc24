(*** Process ***)
fun fail () = (OS.Process.exit OS.Process.failure; ());
fun succeed () = (OS.Process.exit OS.Process.success; ());

fun die msg = (
  print "Error: ";
  print msg;
  print "\n";
  fail ()
  );


(*** IO ***)
fun readlines file = let
  val in_stream = TextIO.openIn file
    handle Io => (die ("Unable to open file '" ^ file ^ "'"); TextIO.openIn file);
  fun loop stream =
    case TextIO.inputLine stream of
         SOME line => hd (String.tokens (fn c => c = #"\n") line) :: loop stream
       | NONE      => []
in
  (loop in_stream) before (TextIO.closeIn in_stream)
end;

fun printIntValue name i = (
  print name;
  print ": ";
  print (Int.toString i);
  print "\n"
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
  (hd tokens, hd (tl tokens))
  handle Empty => (die ("Not a pair: '" ^ str ^ "'"); ("", ""))
end;

val sumList = List.foldl op+ 0;
val multList = List.foldl op* 1;

fun range x y = if x = y
                then []
                else x::(range (x + 1) y);

fun enumerate l = ListPair.zip (range 0 (List.length l), l);


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
  fun checkRequired parsedArgs = List.foldl checkArg parsedArgs required;
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
        SOME (k, v) => SOME (Option.valOf v)
      | NONE => NONE
end;

fun getRequiredArg parsedArgs name = let
  fun predicate (x, _) = x = name;
  val (_, v) = Option.valOf (List.find predicate parsedArgs);
in
  Option.valOf v
end;

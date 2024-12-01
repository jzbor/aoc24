fun readlines file = let
  val in_stream = TextIO.openIn file;
  fun loop stream =
    case TextIO.inputLine stream of
         SOME line => hd (String.tokens (fn c => c = #"\n") line) :: loop stream
       | NONE      => []
in
  (loop in_stream) before (TextIO.closeIn in_stream)
end;

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

fun pairsplit delim str = let
  val tokens = String.tokens (fn c => c = delim) str;
in
  (hd tokens, hd (tl tokens))
end;

fun printIntValue name i = (
  print name;
  print ": ";
  print (Int.toString i);
  print "\n"
  );

val sumList = List.foldr op+ 0;
val multList = List.foldr op* 1;

fun parseArgs (name, args: string list) expectedArgs = let
  fun fmtParam name true = " <" ^ name ^ ">"
    | fmtParam name false = ""
  fun fmtReq name param = " --" ^ name ^ (fmtParam name param)
  fun fmtOpt name param = " [--" ^ name ^ (fmtParam name param) ^ "]"
  fun printRemainingUsage [] = ()
    | printRemainingUsage ((name, required, param)::rest) = (
    (if required
    then print (fmtReq name param)
    else print (fmtOpt name param));
    printRemainingUsage rest
    );
  fun printUsage name expected = (
    print "Usage: ";
    print name;
    printRemainingUsage expected;
    print "\n"
    );
  val required = List.map (fn (n, _, _) => n)
    (List.filter (fn (_, req, _) => req) expectedArgs);
  fun findExpected name = List.find (fn (n, _, _) => "--"^n = name) expectedArgs;
  fun fail () = (OS.Process.exit OS.Process.failure; ());
  fun missingArgument arg = (
    print "\nMissing Argument: ";
    print arg;
    print "\n";
    printUsage name expectedArgs;
    fail ()
    );
  fun missingParameter arg = (
    print "\nMissing Parameter for ";
    print arg;
    print "\n";
    printUsage name expectedArgs;
    fail ()
    );
  fun unknownArgument arg = (
    print "\nUnknown Argument: ";
    print arg;
    print "\n";
    printUsage name expectedArgs;
    fail ()
    );
  fun parse [] = []
    | parse (x::[]) = (case findExpected x of
                         SOME (n, _, areq) => if areq
                                              then (missingParameter x; [])
                                              else [(n, NONE)]
                       | NONE => (unknownArgument x; []))
    | parse (x::xa::xs) = (case findExpected x of
                               SOME (n, _, areq) => if areq
                                                    then (n, SOME xa)::(parse xs)
                                                    else (n, NONE)::(parse (xa::xs))
                             | NONE => (unknownArgument x; []));
  val parsed = parse args;
  fun checkArg parsedArgs arg = (case List.find (fn (x, _) => x = arg) parsedArgs of
                                     SOME _ => ()
                                   | NONE => missingArgument arg);
  fun checkRequired parsedArgs = (List.map (checkArg parsedArgs) required; ());
in
  parsed before (checkRequired parsed)
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

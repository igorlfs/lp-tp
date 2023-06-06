(* Infrastructure to run the Plc interpreter*)

CM.make "$/basis.cm";
CM.make "$/ml-yacc-lib.cm";

Control.Print.printLength := 1000;
Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;

val interpFile = TextIO.openAppend "Bad-Plc-Output";
val caseIdx = ref 1

fun writeResult r =
  let
    val res = run r
    val idx = !caseIdx
  in
    TextIO.output (interpFile, (Int.toString idx ^ ". " ^ res ^ "\n"));
    caseIdx := !caseIdx + 1
  end;

(* Test interpreter *)
map (fn x => writeResult (#2 x)) bad;


TextIO.closeOut interpFile;

OS.Process.exit OS.Process.success;

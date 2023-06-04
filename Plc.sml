(* Plc interpreter main file *)

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

fun run exp = 
    let
        val e = [];
        val t = teval(exp, []);
        val v = eval(exp, [])
        val tStr = type2string(t);
        val vStr = val2string(v);
    in
        vStr ^ " : " ^ tStr
    end

    handle EmptySeq => "EmptySeq: A sequẽncia de entrada não contêm nenhum elemento"
(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

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
        val t_env = [];
        val v_env = [];
        val t = teval(exp, t_env);
        val v = eval(exp, v_env)
        val tStr = type2string(t);
        val vStr = val2string(v);
    in
        vStr ^ " : " ^ tStr
    end

    handle EmptySeq => "EmptySeq: A sequência de entrada não contêm nenhum elemento"
(* PlcChecker *)

use "Environ.sml";
use "Absyn.sml";

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval ((ConI _), _) = IntT
    | teval ((ConB _), _) = BoolT
    | teval ((ESeq seq), _) = 
        ( 
            case seq of
                SeqT x => SeqT x
                | _ => raise EmptySeq
        )
    | teval ((Var v), (e : plcType env)) = lookup e v 
    | teval ((Let(s, exp1, exp2)), (e : plcType env)) =
        let 
            val newEnv = (s, (teval(exp1, e)))::e
        in
            teval(exp2, newEnv)
        end
    | teval ((Letrec(s1, t1, s2, t2, exp1, exp2)), (e : plcType env)) =
        let
            val newEnv = (s1, t1)::e
        in
            teval(exp2, newEnv)
        end
    | teval ((Prim1(op, exp)), (e : plcType env)) =
        let
            val expType = teval(exp, e);
            val r =
                if op = "-" then if expType = IntT then IntT else raise UnknownType
                else if op = "!" then if expType = BoolT then BoolT else raise UnknownType
                else raise UnknownType
        in
            IntT
        end

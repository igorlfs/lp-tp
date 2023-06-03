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
                SeqT t => t
                | _ => raise EmptySeq
        )
    | teval ((Var v), (e : plcType env)) = lookup e v 
    | teval ((Let(s, exp1, exp2)), (e : plcType env)) =
        let 
            val newEnv = (s, teval(exp1, e))::e
        in
            teval(exp2, newEnv)
        end
    | teval ((Letrec(s1, t1, s2, t2, exp1, exp2)), (e : plcType env)) =
        let
            val newEnv = (s1, t1)::e
        in
            teval(exp2, newEnv)
        end
    | teval ((Prim1(oper, exp)), (e : plcType env)) =
        let
            val expType = teval(exp, e);
            val r = case oper of
                "-" => if expType = IntT then IntT else raise UnknownType
                | "!" => if expType = BoolT then BoolT else raise UnknownType
                | "hd" => 
                    (
                        case expType of
                            SeqT t => t
                            | _ => raise UnknownType
                    )
                | "tl" => 
                    (
                        case expType of 
                            SeqT t => SeqT t
                            | _ => raise UnknownType
                    )
                | "ise" => (*incompleto*)
                    (
                        case expType of
                            SeqT t => BoolT
                            | _ => raise UnknownType
                    )
                | "print" => ListT []
                | _ => raise UnknownType
        in
            r
        end

    | teval ((Prim2(oper, exp1, exp2)), (e : plcType env)) =
        let 
            val exp1Type = teval(exp1, e);
            val exp2Type = teval(exp2, e);
            val r = case oper of
                "&&" => if exp1Type = BoolT andalso exp2Type = BoolT then BoolT else raise UnknownType
                | "::" => 
                (
                    case exp2Type of
                        SeqT t => if exp1Type = t then t else raise UnknownType
                        | _ => raise UnknownType
                )
                | ("+" | "-" | "*" | "/") => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
                | ("<" | "<=") => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
                | ("=" | "!=") => if exp1Type <> exp2Type then raise NotEqTypes else
                    (
                        case exp1Type of
                            IntT => BoolT
                            | BoolT => BoolT
                            | (ListT []) => BoolT
                            | SeqT IntT => BoolT
                            | SeqT BoolT => BoolT
                            | (SeqT (ListT [])) => BoolT (*incompleto, ainda falta validar tuplas , to meio perdido nessa parte ainda*)
                    )
                | ";" => exp2Type
                | _ => raise UnknownType

        in
            r
        end
    | teval ((If(cond, exp1, exp2)), (e : plcType env)) = 
        
        let 
            val cond = teval(exp1, e);
            val exp1Type = teval(exp1, e);
            val exp2Type = teval(exp2, e)
        in 
            if cond <> BoolT then raise IfCondNotBool else
                if exp1Type <> exp2Type then raise DiffBrTypes else exp1Type
        end
    | teval ((Match(exp1, optionList)), (e : plcType env)) = IntT (*fazer match*)

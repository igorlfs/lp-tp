(* PlcInterp *)

use "Environ.sml";
use "Absyn.sml";

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval ((ConI(n)), (_)) = IntV(n)
    | eval ((ConB(b)), (_)) = BoolV(b)
    | eval ((ESeq(_)), (_)) = SeqV []
    | eval ((Var(v)), (e : plcVal env)) = lookup e v
    | eval ((Let(s, exp1, exp2)), (e : plcVal env)) = 
        let
            val newEnv = (s, eval(exp1, e))::e
        in
          eval(exp2, newEnv)
        end
    | eval ((Letrec(s1, _, s2, _, exp1, exp2)), (e : plcVal env)) = (*confirmar ordem dos params*)
        let
            val newEnv = (s1, Clos(s1, s2, exp1, e))::e
        in
            eval(exp2, newEnv)
        end
    | eval (Prim1(oper, exp), (e : plcVal env)) = 
        let
            val expVal = eval(exp, e)
        in
            case expVal of
                IntV(i) => 
                    (
                        case oper of 
                            "-" => IntV(~i)
                            | "print" => 
                                let
                                    val printVal = print(val2string(expVal) ^ "\n")
                                in
                                    ListV([])
                                end
                            | _ => raise Impossible
                    )
                | BoolV(b) =>
                    (
                        case oper of
                            "!" => BoolV(not b)
                            | "print" =>
                                let
                                    val printVal = print(val2string(expVal) ^ "\n")
                                in
                                    ListV([])
                                end                                
                            | _ => raise Impossible
                    )
                | SeqV(seq) =>
                    (
                        case oper of
                            "hd" => 
                                (
                                    case seq of
                                        [] => raise HDEmptySeq
                                        | _ => hd seq
                                )
                            | "tl" => 
                                (
                                    case seq of
                                        [] => raise TLEmptySeq
                                        | _ => SeqV(tl seq)
                                )
                            | "ise" => BoolV(seq = [])
                            | "print" =>
                                let
                                    val printVal = print(val2string(expVal) ^ "\n")
                                in
                                    ListV([])
                                end
                            | _ => raise Impossible
                    )
                | ListV(l) => 
                    (
                        case oper of
                            "print" =>
                                let
                                    val printVal = print(val2string(expVal) ^ "\n")
                                in
                                    ListV([])
                                end
                            | _ => raise Impossible
                    )
                | _ => raise Impossible
        end

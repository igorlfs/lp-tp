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
    | eval ((Prim2(oper, exp1, exp2)), (e : plcVal env)) =
        let
            val exp1Val = eval(exp1, e);
            val exp2Val = eval(exp2, e)
        in
            if oper = ";" then exp2Val else
            (   
                case (exp1Val, exp2Val) of
                    (BoolV(b1), BoolV(b2)) => 
                        (
                            case oper of
                                "&&" => BoolV(b1 andalso b2)
                                | "=" => BoolV(b1 = b2)
                                | "!=" => BoolV(b1 <> b2)
                                | _ => raise Impossible
                        )
                    | (IntV(i1), IntV(i2)) => 
                        (
                            case oper of
                                "+" => IntV(i1 + i2)
                                | "-" => IntV(i1 - i2)
                                | "*" => IntV(i1 * i2)
                                | "/" => IntV(i1 div i2)
                                | "<" => BoolV(i1 < i2)
                                | "<=" => BoolV(i1 <= i2)
                                | "=" => BoolV(i1 = i2)
                                | "!=" => BoolV(i1 <> i2)
                                | _ => raise Impossible
                        )
                    | (BoolV(b), SeqV(seq)) =>
                        (
                            case oper of
                                "::" => SeqV(BoolV(b)::seq)
                                | _ => raise Impossible
                        )
                    | (IntV(i), SeqV(seq)) => 
                        (
                            case oper of
                                "::" => SeqV(IntV(i)::seq)
                                | _ => raise Impossible
                        )
                    | (ListV(l), SeqV(seq)) =>
                        (
                            case oper of
                                "::" => SeqV(ListV(l)::seq)
                                | _ => raise Impossible
                        )
                    | (SeqV(h), SeqV(seq)) =>
                        (
                            case oper of
                                "::" => SeqV(SeqV(h)::seq)
                                | _ => raise Impossible
                        )
                    | _ => raise Impossible
            )
        end
    | eval((If(cond, exp1, exp2)), (e : plcVal e)) =
        let
            val condVal = eval(cond, e)
        in
            (
                case condVal of
                    BoolV(true) => eval(exp1, e)
                    | BoolV(false) => eval(exp2, e)
                    | _ => raise Impossible
            )
        end
    | eval 

(* PlcInterp *)

use "Environ.sml"
use "Absyn.sml"

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
    | eval _ = IntV(5)

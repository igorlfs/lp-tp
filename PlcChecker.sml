(* PlcChecker *)

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
                SeqT t => SeqT t
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
            val newEnv = (s1, FunT(t1, t2))::(s2, t1)::e;
            val exp1Type  = teval(exp1, newEnv);
            val exp2Type = teval(exp2, newEnv)
        in
            if exp1Type = t2 then exp2Type else raise WrongRetType
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
                | "ise" =>
                    (
                        case expType of
                            SeqT _ => BoolT
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
                        SeqT(ListT []) => SeqT exp1Type
                        | SeqT t => if exp1Type = t then SeqT t else raise UnknownType
                        | _ => raise UnknownType
                )
                | ("+" | "-" | "*" | "/") => if exp1Type = IntT andalso exp2Type = IntT then IntT else raise UnknownType
                | ("<" | "<=") => if exp1Type = IntT andalso exp2Type = IntT then BoolT else raise UnknownType
                | ("=" | "!=") => if exp1Type <> exp2Type then raise NotEqTypes else
                    let
                        fun isEqType t = 
                        (
                            case t of 
                                (IntT | BoolT | ListT [] | SeqT IntT | SeqT BoolT | SeqT(ListT [])) => true
                                | ListT typeList => List.all (fn x => isEqType x) typeList
                                | _ => false
                        )
                    in
                        if isEqType exp1Type then BoolT else raise UnknownType
                    end
                | ";" => exp2Type
                | _ => raise UnknownType
        in
            r
        end
    | teval ((If(cond, exp1, exp2)), (e : plcType env)) = 
        let 
            val condType = teval(cond, e);
            val exp1Type = teval(exp1, e);
            val exp2Type = teval(exp2, e)
        in 
            if condType <> BoolT then raise IfCondNotBool else
                if exp1Type <> exp2Type then raise DiffBrTypes else exp1Type
        end
    | teval ((Match(exp1, optionsList)), (e : plcType env)) =
        if List.null optionsList then raise NoMatchResults else
            let 
                val optionsTypes = map (fn tup => teval((#2 tup), e)) optionsList;
                val firstOptionType = hd optionsTypes;
                val allOptionsSameType = List.all (fn x => x = firstOptionType) optionsTypes
            in
                if allOptionsSameType then 
                    let
                        val exp1Type = teval(exp1, e);
                        val optionsExpsTypes = map (fn tup => teval((#2 tup), e)) optionsList;
                        val firstOptionExpType = hd optionsExpsTypes;
                        val allOptionsExpsSameType = List.all (fn x => x = firstOptionExpType) optionsExpsTypes
                    in
                        if allOptionsExpsSameType then exp1Type else raise MatchCondTypesDiff
                    end
                else raise MatchResTypeDiff
            end
    | teval ((Call(exp1, exp2)), (e : plcType env)) =
        let 
            val exp1Type = teval(exp1, e);
            val exp2Type = teval(exp2, e)
        in
            case exp1Type of
                FunT(t1, t2) => if t1 <> exp2Type then raise CallTypeMisM else t2
                | _ => raise NotFunc
        end
    | teval ((List expList), (e : plcType env)) = 
        let
            val listTypes = List.map (fn x => teval(x, e)) expList
        in
            ListT listTypes
        end    
    | teval ((Item(n, exp)), (e : plcType env)) =
        let 
            val expType = teval(exp, e);
        in
            case expType of
                ListT[] => raise ListOutOfRange
                | ListT listTypes => 
                    if n > (List.length listTypes) orelse n < 1 then raise ListOutOfRange else List.nth(listTypes, n - 1)
                | _ => raise OpNonList
        end
    | teval ((Anon(t, s, exp)), (e : plcType env)) =
        let
            val newEnv = (s, t)::e;
            val expType = teval(exp, newEnv)
        in
            FunT(t, expType)
        end

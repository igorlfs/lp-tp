(* Plc interpreter main file *)

CM.make "$/basis.cm";
CM.make "$/ml-yacc-lib.cm";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;


open PlcFrontEnd;

fun run exp = 
    let
        val t_env = [];
        (* val v_env = []; *)
        val t = teval(exp, t_env);
        val v = IntV 5;
        val tStr = type2string t;
        val vStr = val2string v
    in
        vStr ^ " : " ^ tStr
    end
    handle EmptySeq => "EmptySeq: a sequência de entrada não contêm nenhum elemento"
    | UnknownType => "UnknownType: o tipo da expressão passada é desconhecido e não pode ser verificado"
    | NotEqTypes => "NotEqTypes: os tipos utilizados na comparação são diferentes"
    | WrongRetType => "WrongRetType: o tipo de retorno definido na declaração da função não coincide com o tipo retornado pelo corpo da função"
    | DiffBrTypes => "DiffBrTypes: os tipos das expressões nos diferentes caminhos do comando if são incompatíveis"
    | IfCondNotBool => "IfCondNotBool: a condição do comando if não é do tipo booleano"
    | NoMatchResults => "NoMatchResults: a lista de expressões disponíveis em match é vazia"
    | MatchResTypeDiff => "MatchResTypeDiff: o tipo de retorno de um ou mais casos na expressão match é diferente dos demais casos"
    | MatchCondTypesDiff => "MatchCondTypesDiff: o tipo das opções de casamento difere do tipo da expressão passada para match"
    | CallTypeMisM => "CallTypeMisM: o tipo passado como parâmetro para a função é diferente do tipo que ela espera"
    | NotFunc => "NotFunc: o objeto que foi utilizado para se chamar algo não é uma função"
    | ListOutOfRange => "ListOutOfRange: foi feita uma tentativa de acessar um elemento fora dos limites de uma lista"
    | OpNonList => "OpNonList: tentaiva de se acessar um elemento de uma expressão que não é uma lista"
    | SymbolNotFound => "SymbolNotFound: Símbolo não encontradono ambiente"
    | _ => "Erro desconhecido"

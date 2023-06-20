(*Environ*)

exception SymbolNotFound

type 'a env = (string * 'a) list

fun lookup [] _ = raise SymbolNotFound
  | lookup ((k: string, v) :: t) id =
      if k = id then v else lookup t id;

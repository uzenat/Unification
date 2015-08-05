type symb = { name : string ; arity : int ; }
type symbAC = { name : string ; }
type var = { name : string ; }

type term =
    private
	 | Symb of symb * term list
	 | SymbAC of symbAC * term Multi_set.ms
	 | Var of var

			   
exception WrongArity of symb * int
val mk_Symb : symb -> term list -> term
val mk_SymbAC : symbAC -> term list -> term
val mk_Var : string -> term
  
val eq : term -> term -> bool

val compare_term : term -> term -> int
val is_occurs : term -> term -> bool

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
val mk_SymbAC2 : symbAC -> term Multi_set.ms -> term

val mk_Var : string -> term
  
val eq : term -> term -> bool

val compare_term : term -> term -> int
val is_occurs : term -> term -> bool

module AssocMap : sig type t = term val compare : term -> term -> int end
module Si :
  sig
    type key = AssocMap.t
    type 'a t = 'a Map.Make(AssocMap).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
    
val sub_term : Si.key Si.t -> Si.key -> Si.key
val si_of_list : (Si.key * 'a) list -> 'a Si.t

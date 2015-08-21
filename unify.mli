module A :
  sig type t = Term.term val compare : Term.term -> Term.term -> int end
module L :
  sig
    type key = A.t
    type 'a t = 'a Map.Make(A).t
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
    val update : key -> 'a -> 'a list t -> 'a list t
    val si_of_l :
      Term.term Multi_set.elem list t -> string -> Term.term Term.Si.t
  end
val solve_dioph : Term.term -> Term.term -> Diophantienne.VectSet.elt list
val assocvar :
  Term.term ->
  Term.term -> Diophantienne.VectMod.t list -> Term.term Term.Si.t
val purifyac_to_assocvar : Term.term -> Term.term -> Term.term Term.Si.t
val getSymb : 'a Term.Si.t -> 'b Term.Si.t -> ((Term.Si.key * 'a) * 'b) list
val getVar : 'a Term.Si.t -> ((Term.Si.key * Term.Si.key) * 'a) list
exception SymbolClash
exception OccursCheck
val si : 'a Term.Si.t
val unify :
  Term.Si.key ->
  Term.Si.key -> Term.Si.key Term.Si.t -> Term.Si.key Term.Si.t list

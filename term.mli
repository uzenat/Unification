(** type symbole simple **)
type symb = { name : string ; arity : int ; }

(** type symbole AC **)
type symbAC = { name : string ; }

(** type variable **) 
type var = { name : string ; }

(** definition d'un terme **)
type term =
  private
  | Symb of symb * term list
  | SymbAC of symbAC * term Multi_set.ms
  | Var of var
      
(** exception **)			   
exception WrongArity of symb * int
exception CantPurify

(** construit un terme simple **)
val mk_Symb : symb -> term list -> term

(** construit un terme AC **)
val mk_SymbAC : symbAC -> term list -> term

(** construit une variable **)
val mk_Var : string -> term

(** retourne la chaine de caractere du terme **)
val string_of_term : term -> string

(** test l'egalite de deux termes **)
val eq : term -> term -> bool

(** compare deux termes **)
val compare_term : term -> term -> int
val is_occurs : term -> term -> bool

(** module de substitution **)
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

(** substition dans un terme **)
val sub_term : Si.key Si.t -> Si.key -> Si.key

val sub_si : Si.key Si.t -> Si.key Si.t -> Si.key Si.t

(** tranforme la substitution en chaine de caractere **)
val string_of_si : term Si.t -> string

val fusion_si : 'a Si.t -> 'a Si.t -> 'a Si.t

val diff_si : 'a Si.t -> 'b Si.t -> 'a Si.t

val eq2 : Si.key Si.t -> Si.key -> Si.key -> bool

val put_voidAC : term -> term Si.t -> term Si.t

val permut_list : ('a * term) * term -> term Si.t -> (('a * term) * term Si.t) list

val notonlyVi_in : term -> bool

(** purification de deux terme AC **)
val purify : term -> term -> term Si.t * (term * term)

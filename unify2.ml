#load "multi_set2.cmo";;
#load "term2.cmo";;
open Term2;;

  
(**** Definition d'une subsitution ****)

(* module *)
module AssocMap =
  struct
    type t = Term2.term
    let compare = compare_term 
  end;;
module Si = Map.Make (AssocMap)

(* effectue une substitution *)
let sub si var =
  try
    Si.find var si 
  with Not_found -> var;;
let rec sub_term si term = match term with
  | Symb(s, args) -> mk_Symb s (List.map (sub_term si) args)
  | Var s -> sub si term
  | _ -> assert false;;

(**** Procedure de purification ****)

		  
		      
(**** Procedure d'unification ****)

exception SymbolClash;;
exception OccursCheck;;
let si = Si.empty;;

let rec unify s t si =

  let s' = match s with
    | Var _ -> sub_term si s
    | _ -> s
  in
    
  let t' = match s with
    | Var _ -> sub_term si t
    | _ -> t
  in
  
  match s', t' with
  | Var v, Var v' when v.name = v'.name -> si
  | Symb(v, args), Symb(v', args') ->
     if v.name = v'.name && v.arity = v'.arity then
       List.fold_left2 (fun a b c -> unify b c a) si args args'
     else raise SymbolClash
  | Var v, _ ->
     if is_occurs s' t' then raise OccursCheck
     else Si.add s' t' si
  | _ , Var v -> unify t' s' si
  | _ -> assert false;;


  
let x = mk_Var "x";;
let y = mk_Var "y";;
let z = mk_Var "z";;
let u = mk_Var "v";;
let v = mk_Var "v";;

let a = mk_Symb {name="a" ; arity=0} [];;
let b = mk_Symb {name="b" ; arity=0} [];;
let c = mk_Symb {name="c" ; arity=0} [];;
let d = mk_Symb {name="d" ; arity=0} [];;

let f x = mk_Symb {name="f" ; arity=1} [ x ];;
let g x = mk_Symb {name="g" ; arity=1} [ x ];;

let p x y = mk_Symb {name="p" ; arity=2} [ x ; y ];;
let q x y = mk_Symb {name="q" ; arity=2} [ x ; y ];;
  
let r x y z = mk_Symb {name="r" ; arity=3} [ x ; y ; z ];;

let s x y z t = mk_Symb {name="s" ; arity=4} [ x ; y ; z ; t];;


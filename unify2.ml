#load "multi_set2.cmo";;
#load "term2.cmo";;
open Multi_set2
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

(* construit une variable avec un nom et un numero *)
let var_auto s i = mk_Var (s ^ (string_of_int i));;

(* renverse les clef et valeur d'une substitution *)
let reverse si =
  let aux e = match e with (key, valr) -> (valr, key) in
  let aux2 si v = match v with (key, valr) -> Si.add key valr si in
  let bind = Si.bindings si in
  let rev_bind = List.map aux bind in
  List.fold_left aux2 (Si.empty) rev_bind;; 
  
(* purify deux listes d'element *)
let purify l1 l2 =
  let rec aux l lres si i = match l with
    | [] -> si, lres
    | h :: tl ->
       match h with Elem(m, v) ->
	 let new_key = v in
	 let new_var = var_auto "var" i in
	 match (
	   try
	     Some(Si.find new_key si)
	   with Not_found -> None) with
	 | None -> aux tl (lres @ [Elem(m, new_var)]) (Si.add new_key new_var si) (i+1)
	 | Some old_var ->  aux tl (lres @ [Elem(m, old_var)]) (si) (i+1)
  in
  let si = Si.empty in
  let res = aux l1 [] si 1 in
  let lres1 = snd res in
  let si = fst res in
  let res = aux l2 [] si (List.length l1) in
  let lres2 = snd res in
  let si = fst res in
  (reverse si, (lres1, lres2) );;


  
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

let ac1 = mk_SymbAC {name="plus"} [ a; a; a; f a; f a; f b; p a b; p c d ];;
let ac2 = mk_SymbAC {name="plus"} [ a; a; q (f a) (g b); r (p (f a) (f c)) (g (g (g d))) (q a b) ; d ; d ; d ];;

let r = 
  match ac1, ac2 with
    SymbAC(s, Multiset l1), SymbAC(s', Multiset l2) ->
    purify l1 l2;;
  Si.bindings (fst r);;

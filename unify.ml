#load "multi_set.cmo";;
#load "term.cmo";;
#load "diophantienne.cmo";;
open Multi_set
open Term;;
open Diophantienne;;


  
(**** Definition d'une subsitution ****)

(* module *)
module AssocMap =
  struct
    type t = Term.term
    let compare = compare_term
  end;;
module Si = Map.Make (AssocMap);;
  

(* effectue une substitution *)
let sub si var =
  try
    Si.find var si 
  with Not_found -> var;;
let rec sub_term si term = match term with
  | Symb(s, args) -> mk_Symb s (List.map (sub_term si) args)
  | Var s -> sub si term
  | _ -> assert false;;


  
(**** Supression des termes identiques de deux listes ****)

  
let remove_term l1 l2 = 
  let rec remove_term1 m t l lres = match l with
    | [] -> [ Elem(m, t) ], lres
    | Elem(m', v') :: tl ->
       if eq t v' then
	 let new_m = m - m' in
	 let new_m' = m' - m in
	 let r1 = if new_m <= 0 then [] else [ Elem(new_m, t) ] in
	 let r2 = if new_m' <= 0 then (lres @ tl) else lres @ [ Elem(new_m', t) ] @ tl in
	 r1, r2
       else remove_term1 m t tl (lres @ [Elem(m', v')])
  in
  let rec remove_term2 l1 l2 lres = match l1 with
    | [] -> lres, l2
    | Elem(m, v) ::tl ->
       let r = remove_term1 m v l2 [] in
       remove_term2 tl (snd r) (lres @ (fst r))
  in
  remove_term2 l1 l2 [];;
  
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
       match h with
       | Elem(m, Var s) ->
	  aux tl (lres @ [Elem(m, mk_Var s.name)]) si i
       | Elem(m, v) ->
	  let new_key = var_auto "__var__" i in
	  let new_valr = v in
	  let si = Si.add new_key new_valr si in
	  aux tl (lres @ [Elem(m, new_key)]) si (i+1)
  in
  let rmv = remove_term l1 l2 in
  let l1 = fst rmv in
  let l2 = snd rmv in
  let si = Si.empty in
  let r1 = aux l1 [] si 1 in
  let si = fst r1 in
  let lres1 = snd r1 in
  let r2 = aux l2 [] si ( (List.length l1)+1 ) in
  let si = fst r2 in
  let lres2 = snd r2 in
  (si, (lres1, lres2) );;


  
(**** Utilisation des equations diophantienne ****)

(* transforme les deux listes purifie en une equation diophantienne *)
let equat_to_purifylist l1 l2 =
  let aux e = let (Elem(m, _)) = e in m in
  let aux2 l = Coefficient (List.map aux l) in
  Equation(aux2 l1, aux2 l2);;


  
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

let ac1 = mk_SymbAC {name="plus"} [ a ; a ; c ; d ];;
let ac2 = mk_SymbAC {name="plus"} [ b ; b ; f x ];;

let s = solve_dioph ac1 ac2;;
VectSet.elements s;;

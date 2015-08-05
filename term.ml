open Multi_set2;;

(**** Les types ****)

(* definiton d'un symbol *)
type symb = {
  name : string ;
  arity : int ;
}

(* definiton d'une operation AC *)
type symbAC = {
  name : string ;
}

(* definition d'une variable *)
type var = {
  name : string ;
}

(* definiton d'un terme *)
type term =
  | Symb of symb * term list
  | SymbAC of symbAC * term ms
  | Var of var


(**** Les fonctions ****)

(* construit un symbole *)
exception WrongArity of symb * int
let mk_Symb s args =
  let () =
    if List.length args <> s.arity then
      raise (WrongArity(s , List.length args))
  in
  Symb (s, args);;

(* construit une variable *)
let mk_Var s = Var {name=s};;

(* fonction de comparaison de deux termes *)
let lexico comp x y comp2 z w =
  let c = comp x y in
  if c = 0 then comp2 z w
  else c
let rec lexicographic_compare comp l1 l2 = match l1, l2 with
  | [], [] -> 0
  | _ , [] -> 1
  | [], _ -> -1
  | h1::tl1 , h2::tl2 -> lexico comp h1 h2 (lexicographic_compare comp) tl1 tl2;;
let rec compare_term t1 t2 = match t1, t2 with
  | Symb(s1, args1), Symb(s2, args2) -> lexico compare s1 s2 (lexicographic_compare compare_term) args1 args2
  | SymbAC(s1, args1), SymbAC(s2, args2) -> lexico compare s1 s2 (cmpr_Multiset compare_term) args1 args2
  | Var s1, Var s2 -> compare s1 s2
  | Var _, _ -> -1
  | _, Var _ -> 1
  | Symb(s1, _), SymbAC(s2, _) -> -1
  | SymbAC(s1, _), Symb(s2, _) -> 1

(* test l'egalite de deux termes *)
let eq t1 t2 = (compare_term t1 t2) = 0;;

(* test si une variable v est contenue dans un terme t *)
let rec is_occurs v t = match t with
  | Symb(s, args) -> List.exists (is_occurs v) args
  | Var _ -> eq v t
  | _ -> assert false;;

let mk_SymbAC s args =
  let rec aux l l1 l2 = match l with
    | [] -> l1, l2
    | h :: tl ->
       match h with
       | SymbAC(s', v) when s = s' -> aux tl l1 (l2 @ [ h ])
       | x -> aux tl (l1 @ [x]) l2
  in
  let r = aux args [] [] in
  let rec aux2 s ms lac = match lac with
    | [] -> ms
    | h :: tl -> 
      match h with
      | SymbAC(s', v) when s = s' -> aux2 s (fusion compare_term ms v) tl 
      | _ -> assert false
  in 
  let ms = mk_Multiset compare_term (List.sort compare_term (fst r)) in
  SymbAC( s, aux2 s ms (snd r) );;


let a = mk_Symb {name="a" ; arity=0} [];;
let b = mk_Symb {name="b" ; arity=0} [];;
let c = mk_Symb {name="c" ; arity=0} [];;
let d = mk_Symb {name="d" ; arity=0} [];;
let e = mk_Symb {name="e" ; arity=0} [];;

let ac01 = mk_SymbAC {name="plus"} [ a ; b ];;
let ac02 = mk_SymbAC {name="fois"} [ a ; a ; b ];;
let ac1 = mk_SymbAC {name="plus"} [ ac02 ; a ; b ; a ; b ; ac02 ; a ; a ; a ; e ; e ; d ; ac01 ];;
let ac2 = mk_SymbAC {name="plus"} [ ac02 ; ac02 ; e ; d ; e ; ac01 ; a ; b ; a ; b ; a ; a ; a ];;
 eq ac1 ac2;;

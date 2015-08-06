open Multi_set;;

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

(* construit un symbole associatif et commutatif *)
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

let mk_SymbAC2 s ms = SymbAC(s, ms);;



(**** Definition d'une subsitution ****)

(* module *)
module AssocMap =
  struct
    type t = term
    let compare = compare_term
  end;;
module Si = Map.Make (AssocMap);;

(* effectue une substitution *)
let sub_term si term =
  let sub si var =
    try
      Si.find var si 
    with Not_found -> var
  in
  let rec sub_term' si term = match term with
    | Symb(s, args) -> mk_Symb s (List.map (sub_term' si) args)
    | Var s -> sub si term
    | _ -> assert false
  in
  sub_term' si term;;

(* construit une table a partir d'une list d'element (cle, valeur) *)
let rec si_of_list l = match l with
  | [] -> Si.empty
  | (key,valr)::tl -> Si.add key valr (si_of_list tl);;

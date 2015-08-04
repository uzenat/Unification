open Multi_set;;

(**** Les types ****)

(* definiton d'un symbol *)
type symb = {
  name : string ;
  arity : int ;
}

(* definiton d'une operation AC *)
type op = {
  name : string ;
}

(* definition d'une variable *)
type var = {
  name : string ;
}

(* definiton d'un terme *)
type term =
  | Symb of symb * term list
  | Op of op * term ms
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
  | Op(s1, args1), Op(s2, args2) -> lexico compare s1 s2 (cmpr_Multiset compare_term) args1 args2
  | Var s1, Var s2 -> compare s1 s2
  | Var _, _ -> -1
  | _, Var _ -> 1
  | Symb(s1, _), Op(s2, _) -> -1
  | Op(s1, _), Symb(s2, _) -> 1

(* test l'egalite de deux termes *)
let eq t1 t2 = (compare_term t1 t2) = 0;;

(* test si une variable v est contenue dans un terme t *)
let rec is_occurs v t = match t with
  | Symb(s, args) -> List.exists (is_occurs v) args
  | Var _ -> eq v t
  | _ -> assert false;;
  
(* construit un symbol acu *)
let mk_Op s args =
  let rec aux l l1 l2 = match l with
    | [] -> l1, l2
    | h :: tl ->
       match h with
       | Op(s', v) -> aux tl l1 (l2 @ [Op(s',v)])
       | x -> aux tl (l1 @ [x]) l2
  in
  let r = aux args [] [] in
  let e1 = match args with
    | a :: [] -> a
    | _ -> Op( {name=s}, mk_Multiset compare_term (List.sort compare_term (fst r)) )
  in
  let rec aux2 t1 l = match l with
    | [] -> t1
    | h :: tl ->
       match t1, h with
       | Op(s,v), Op(s', v') ->
	  if s = s' then aux2 (Op(s, fusion compare_term v v')) tl
	  else aux2 (Op(s, fusion compare_term v (mk_Multiset compare_term [ Op(s', v') ]) )) tl
       | _ -> assert false
  in
  aux2 e1 (snd r);;

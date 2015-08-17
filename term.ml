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

(* affiche un term *)
let rec string_of_term t =
  let rec string_of_args args = match args with
    | [] -> ""
    | h :: [] -> string_of_term h
    | h :: tl -> (string_of_term h) ^ ", " ^  string_of_args tl
  in
  let rec string_of_argsAC args ac_name = match args with
    | [] -> ""
    | Elem(1, h) :: [] -> string_of_term h
    | Elem(1, h) :: tl -> (string_of_term h) ^ " " ^ ac_name ^ " " ^ (string_of_argsAC tl ac_name)
    | Elem(m, h) :: [] -> (string_of_int m) ^ "." ^ string_of_term h
    | Elem(m, h) :: tl -> (string_of_int m) ^ "." ^ (string_of_term h) ^ " " ^ ac_name ^ " " ^ (string_of_argsAC tl ac_name)
  in
  match t with
  | Symb(s, []) -> s.name
  | Symb(s, args) -> s.name ^ "(" ^ (string_of_args args) ^ ")"
  | SymbAC(s, Multiset args) -> "(" ^ (string_of_argsAC args s.name) ^ ")"
  | Var s -> s.name;;

(* definition de la constante null *)
let null = Symb ({name="@__NULL__@";arity=0}, []);;

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
  | SymbAC(s, Multiset l) -> List.exists (fun x -> let (Elem(m, t')) = x  in (is_occurs v t') ) l;;

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
  
  let l = (List.filter (fun x -> match x with SymbAC(_, Multiset l) when (List.length l = 0) -> false | _ -> true ) (fst r)) in
  let ms = mk_Multiset compare_term (List.sort compare_term l) in
  let l = aux2 s ms (snd r) in
  match l with
  | Multiset [Elem(1, a)] -> a
  | _ -> SymbAC(s, l);;



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
    | SymbAC(s, ms) ->
      let ltmp = list_of_multiset ms in
      let ltmp2 = List.map (sub_term' si) ltmp in
      mk_SymbAC s (List.map (sub_term' si) ltmp2)
  in
  sub_term' si term;;


let sub_si si1 si2 =
  List.fold_left (fun si (k,v) -> Si.add k v si) (Si.empty) (List.map (fun (k,v) -> (k, sub_term si1 v)) (Si.bindings si2));;

(* retourne la chaine de caractere d'une substitution *)
let string_of_si si =
  let string_of_e e = let (k, v) = e in (string_of_term k) ^ " <- " ^ (string_of_term v) in
  let rec string_of_si' l = match l with
    | [] -> ""
    | h::[] -> string_of_e h
    | h :: tl -> (string_of_e h) ^ " ; " ^ (string_of_si' tl)
  in
  "{ " ^ (string_of_si' (Si.bindings si)) ^ " }";;

(* fusionne deux substitutions *)
let fusion_si si1 si2 =
  List.fold_left (fun si (k,v) -> Si.add k v si) si1 (Si.bindings si2);;

(* renvoi la substitution si1 sans les clef appartenant a si2 *) 
let diff_si si1 si2 =
  let l = List.filter (fun (k,v) -> match (try Some(Si.find k si2) with _ -> None) with None -> true | _ -> false) (Si.bindings si1) in
  List.fold_left (fun si (k,v) -> Si.add k v si) (Si.empty) l;;

(* test si t1 et t2 sont egaux a une substitution pres *) 
let eq2 si t1 t2 = eq (sub_term si t1) (sub_term si t2);;



(**** Purification de deux termes AC *****)

exception CantPurify;;
let purify ac1 ac2 = match ac1, ac2 with
  | SymbAC(s1, ms1), SymbAC(s2, ms2) when s1 = s2 ->
    let var_auto s i = mk_Var (s ^ (string_of_int i)) in
    let rec aux l lres si i = match l with
      | [] -> si, lres
      | h :: tl ->
        match h with
        | Elem(m, Var s) ->
	  aux tl (lres @ [Elem(m, mk_Var s.name)]) si i
        | Elem(m, v) ->
          let new_key = var_auto "__var__" i in
          let si = Si.add new_key v si in
          aux tl (lres @ [Elem(m, new_key)]) si (i+1)
    in
    let rmv = del_same compare_term ms1 ms2 in
    let l1 = let (Multiset l) = fst rmv in l in
    let l2 = let (Multiset l) = snd rmv in l in
    let r1 = aux l1 [] (Si.empty) 1 in
    let si = fst r1 in
    let lres1 = snd r1 in
    let r2 = aux l2 [] si ( (List.length l1)+1 ) in
    let si = fst r2 in
    let lres2 = snd r2 in
    si, (mk_SymbAC s1 (list_of_multiset (Multiset lres1)), (mk_SymbAC s2 (list_of_multiset (Multiset lres2))))
  | _ -> raise CantPurify;;



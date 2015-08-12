#load "multi_set.cmo";;
#load "term.cmo";;
#load "diophantienne.cmo";;
open Multi_set;;
open Term;;
open Diophantienne;;

  
(**** Supression des termes identiques de deux listes ****)



(* renvoie les deux liste sans les termes en commun *)  
let remove_term l1 l2 =
  let ms1 = Multiset l1 in
  let ms2 = Multiset l2 in
  let ms1' = Multi_set.diff compare_term ms1 ms2 in
  let ms2' = Multi_set.diff compare_term ms2 ms1 in
  let (Multiset l1', Multiset l2') = ms1', ms2' in l1', l2';;

  
(**** Procedure de purification ****)

(* construit une variable avec un nom et un numero *)
let var_auto s i = mk_Var (s ^ (string_of_int i));;
  
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
	  let si = Si.add new_key v si in
	  aux tl (lres @ [Elem(m, new_key)]) si (i+1)
  in
  let rmv = remove_term l1 l2 in
  let l1 = fst rmv in
  let l2 = snd rmv in
  let r1 = aux l1 [] (Si.empty) 1 in
  let si = fst r1 in
  let lres1 = snd r1 in
  let r2 = aux l2 [] si ( (List.length l1)+1 ) in
  let si = fst r2 in
  let lres2 = snd r2 in
  (si, (lres1, lres2) );;

  
(**** Utilisation des equations diophantienne ****)

(* transforme les deux listes purifie en une equation diophantienne *)
let equat_of_purifylist l1 l2 =
  let aux e = let (Elem(m, _)) = e in m in
  let aux2 l = Coefficient (List.map aux l) in
  Equation(aux2 l1, aux2 l2);;


  
(**** Travail preliminaire 1 :                                               ****
 **** On veut, a partir des deux listes purifies, recuperer une substitution ****
 **** qui substitue a chaque variable purife des deux listes                 ****
 **** la somme ponderer des nouvelles variable creer par les solutions de la ****
 **** base de l'equation diophantienne creer a partir des deux listes        ****) 

module A = struct
  type t = term
  let compare = compare_term
end;;

module L =
struct
  include Map.Make (A)
  let update k v m =
    begin
      let r =
        try Some (find k m)
        with Not_found -> None
      in
      match r with
      | None -> add k [v] m
      | Some x -> add k (x @ [v]) m
    end
end;;

(* - a partir des deux listes purifies, on creer une equation diophantienne
     ET on resoud l'equation diophantienne en generant une base de solutions *)
let solve_dioph l1 l2 = 
  let equ = equat_of_purifylist l1 l2 in
  VectSet.elements (procedure equ) ;;

(* - pour chaque solution de la base on creer une nouvelle variable 
     ET on lui associe sa variable purifie corespondante *)
let assocvar l1 l2 r =
  let rec aux l1 l2 r map i = match r with
    | [] -> map
    | h::tl ->
      match h with VectMod.Vect(a, b) ->
        let rec aux2 p q map i = match p, q with
          | [], [] -> map
          | Elem(m1, var)::tl1, m2::tl2 ->
            if m2 = 0 then
              aux2 tl1 tl2 map (i+1)
            else
              let map' = L.update var (Elem(m2, var_auto "_v" i)) map in
              aux2 tl1 tl2 map' (i+1)
          | _ -> failwith "impossible"
        in
        let map' = aux2 l1 a map i in
        let map' = aux2 l2 b map' i in
        aux l1 l2 tl map' (i+1)
  in
  (aux l1 l2 r (L.empty) 1);;
        

(* - on construit maintenant la substitution *)
let purifylist_to_assocvar l1 l2 =
  let r = solve_dioph l1 l2 in
  assocvar l1 l2 r
  
  
(**** Procedure d'unification ****)

exception SymbolClash;;
exception OccursCheck;;
let si = Si.empty;;

let rec unify s t si =

  let s' = sub_term si s in
  let t' = sub_term si t in
  
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



let zero = mk_Symb {name="__zero__" ; arity=0} [];;

let rec list_to_si a l si = match l with
  | [] -> si
  | Elem(0, var) :: tl -> list_to_si a tl (Si.add var zero si)
  | Elem(m, var) :: tl -> list_to_si a tl (Si.add var a si);; 

let rec getSymb lvar lmap = match lvar with
  | [] -> []
  | (v, cons) :: tl -> ((v, cons) , L.find v lmap) :: getSymb tl lmap;;
    

let a = mk_Symb {name="a" ; arity=0} [];;
let b = mk_Symb {name="b" ; arity=0} [];;

let x = mk_Var "x";;
let y = mk_Var "y";;
let z = mk_Var "z";;

let v1 = mk_Var "v1";;
let v2 = mk_Var "v2";;
let v3 = mk_Var "v3";;



Si.bindings (unify a a (Si.empty));;

let ac1 = mk_SymbAC {name="+"} [ x ; x ; y ; a ];;
let ac2 = mk_SymbAC {name="+"} [ b ; b ; z ];;

let l1 = let (SymbAC (_, Multiset l)) = ac1 in l;;
let l2 = let (SymbAC (_, Multiset l)) = ac2 in l;;

let r = purify l1 l2;;
let si = fst r;;
let l1 = fst (snd r);;
let l2 = snd (snd r);;

let sol = solve_dioph l1 l2;;
let assoc = purifylist_to_assocvar l1 l2;;
L.bindings assoc;;
let list = getSymb (Si.bindings si) assoc;;
let f1 = List.hd list;;
let f2 = List.hd (List.tl list);;
let zero = (mk_Symb {name="__zero__";arity=0} []) ;;


let rec aux2 l si = match l with
  | [] -> []
  | Elem(m, v) :: tl -> Elem(m, sub_term si v) :: aux2 tl si
;;
let rec algo1 var cons hd0 l si = match l with
      | [] -> []
      | hd :: tl ->
        let aux1 x = match x with Elem(m, v) -> (v, zero) in
        let hd2 = match hd with Elem(m, v) -> (v, cons) in
        let tl2 = List.map aux1 tl in
        let r = hd0 @ [hd2] @ tl2 in
        let si' = List.fold_right (fun (x,y) -> Si.add x y) r (si) in
        let value = ( (var, cons) , (si') ) in
        value :: algo1 var cons (hd0 @ [(fst hd2, zero)]) tl si;;



let r = algo1 (fst (fst f1)) (snd (fst f1)) [] (snd f1) (Si.empty);;

let f11 = List.hd r;;
let si11 = snd f11;;

let l2 = snd f2;;
aux2 l2 si11;;
algo1 (fst (fst f2)) (snd (fst f2)) [] (snd f2) (Si.empty);;

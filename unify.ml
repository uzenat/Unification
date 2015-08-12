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



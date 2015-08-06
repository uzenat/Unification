#load "multi_set.cmo";;
#load "term.cmo";;
#load "diophantienne.cmo";;
open Multi_set
open Term;;
open Diophantienne;;

  
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
let equat_of_purifylist l1 l2 =
  let aux e = let (Elem(m, _)) = e in m in
  let aux2 l = Coefficient (List.map aux l) in
  Equation(aux2 l1, aux2 l2);;


  
(**** Travail preliminaire 1 :                                               ****
 **** On veut, a partir des deux listes purifies, recuperer une substitution ****
 **** qui substitue a chaque variable purife des deux listes                 ****
 **** la somme ponderer des nouvelles variable creer par les solutions de la ****
 **** base de l'equation diophantienne creer a partir des deux listes        ****)

(* - a partir des deux listes purifies, on creer une equation diophantienne
     ET on resoud l'equation diophantienne en generant une base de solutions *)
let solve_dioph l1 l2 = 
  let equ = equat_of_purifylist l1 l2 in
  VectSet.elements (procedure equ) ;;

(* - pour chaque solution de la base on creer une nouvelle variable 
     ET on lui associe sa variable purifie corespondante *)
let assocvar l1 l2 r =
  let rec assoc_Var l1 l2 r i = match r with
    | [] -> []
    | h::tl ->
       let rec aux l v i = match l, v with
	 | [],[] -> []
	 | h1::tl1, h2::tl2 ->
	  begin
	    match h1, h2 with
	      Elem(_, var), m -> ( var , [ Elem(m, var_auto "_v" i) ] ) :: aux tl1 tl2 i
	  end
	 | _ -> failwith "impossible" 
       in
       match h with
	 VectMod.Vect(v1, v2) ->
	 aux l1 v1 i @ aux l2 v2 i @ assoc_Var l1 l2 tl (i+1)
  in
  assoc_Var l1 l2 r 1;;

(* - on fusionne les element qui on la meme variable purifier en reunissant les nouvelle variable correspondante *)
let merge l = 
  let rec merge' l1 l2 = match l1, l2 with
    | [], [] -> []
    | l1', [] -> l1'
    | [], l2' -> l2'
    | h1::tl1, h2::tl2 ->
       
       match h1, h2 with
	 (e1, a), (e2, b) ->
	 if compare_term e1 e2 = -1 then
	   h1 :: merge' tl1 (h2::tl2)
	 else if compare_term e1 e2 = 1 then
	   h2 :: merge' (h1::tl1) tl2
	 else (e1, a @ b) :: merge' tl1 tl2
  in
  let cut l =
    let rec scinde' l r n = match l, n with
      | _, 0 -> r, l
      | h::tl, _ -> scinde' tl (r @ [h]) (n-1)
      | _ -> failwith "error"
    in
    (scinde' l [] ((List.length l)/2))
  in
  let rec merge_rec l =
    let r = cut l in
    let e1 = fst r in
    let e2 = snd r in
    let le1 = List.length e1 in
    let le2 = List.length e2 in
    if (le1 > 1) || (le2 > 1) then
      merge' (merge_rec e1) (merge_rec e2)
    else merge' e1 e2
  in
  merge_rec l;;
    
(* - enfin, on pour chaque variable associe est associe une liste de nouvelle variable *)
(* On creer un symbole AC pour cette liste *)
let rec buildAC l = match l with
  | [] -> []
  | (var, l') :: tl -> (var, mk_SymbAC2 {name="+"} (mk_Multiset2 l')) :: fff tl;;

(* - on construit maintenant la substitution *)
let purifylist_to_assocvar l1 l2 =
  let r = solve_dioph l1 l2 in
  let l = assocvar l1 l2 r in
  let l = List.sort (fun x y -> match x, y with (e1, _), (e2, _) -> compare_term e1 e2) l in
  let l = merge l in
  let l = buildAC l in
  si_of_list l;;
  
  
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

open Multi_set;;
open Term;;
open Diophantienne;;

(* #load "multi_set.cmo";; *)
(* #load "term.cmo";; *)
(* #load "diophantienne.cmo";; *)

  
  
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
  let si_of_l l s =
    let tr arg = List.map (fun x -> let (k, v) = x in (k, mk_SymbAC {name=s} (list_of_multiset (Multiset v)))) arg in
    List.fold_left (fun si (x,y) -> Si.add x y si) (Si.empty) (tr (bindings l)) 
end;;



(* resoud l'equation diophantienne associe au deux termes AC *)
let solve_dioph ac1 ac2 =
  let equat_of_purifylist ac1 ac2 = match ac1, ac2 with
  | SymbAC (s1, Multiset l1), SymbAC (s2, Multiset l2) ->
    let aux e = let (Elem(m, _)) = e in m in
    let aux2 l = Coefficient (List.map aux l) in
    Equation(aux2 l1, aux2 l2)
  | _ -> failwith "Error : solv_dioph"
  in
  let equ = equat_of_purifylist ac1 ac2 in
  VectSet.elements (procedure equ) ;;



(* - pour chaque solution de la base on creer une nouvelle variable 
     ET on lui associe sa variable purifie corespondante *)
let assocvar ac1 ac2 r = match ac1, ac2 with
  | SymbAC (s1, Multiset l1), SymbAC (s2, Multiset l2) ->
    let rec aux l1 l2 r map i = match r with
      | [] -> map
      | h::tl ->
        match h with VectMod.Vect(a, b) ->
          let rec aux2 p q map i = match p, q with
            | [], [] -> map
            | Elem(m1, var)::tl1, m2::tl2 ->
              if m2 = 0 then
                aux2 tl1 tl2 map i
              else
                let map' = L.update var (Elem(m2, mk_Var ("_v" ^ (string_of_int i)))) map in
                aux2 tl1 tl2 map' i
            | _ -> failwith "impossible"
          in
          let map' = aux2 l1 a map i in
          let map' = aux2 l2 b map' i in
          aux l1 l2 tl map' (i+1)
    in
    L.si_of_l (aux l1 l2 r (L.empty) 1) s1.name
  | _ -> failwith "Error : no AC term";;
        

(* - on construit maintenant la substitution *)
let purifyac_to_assocvar ac1 ac2 =
  let r = solve_dioph ac1 ac2 in
  assocvar ac1 ac2 r

let getSymb si1 si2 =
  let aux si e = let (k, v) = e in
    ( (k, v) , Si.find k si )
  in
  List.map (aux si2) (Si.bindings si1);;
let getVar si =
  List.map (fun (k,v) -> ((k,k),v)) (Si.bindings si);;

  
  
(**** Procedure d'unification ****)

exception SymbolClash;;
exception OccursCheck;;
let si = Si.empty;;

(* Renvoi un  unificateur (le premier) de s et t *)
let rec unify s t si =

  let (s, t) = (sub_term si s, sub_term si t) in

  if eq s t then [ si ]
  else

    begin

      
      if (is_occurs s t) || (is_occurs t s) then raise OccursCheck
      else

        

      match s, t with

      (* Unification de deux variables egal *)
      | Var v, Var v' when v.name = v'.name -> [ si ]

      (* Unification de deux symbols *)
      | Symb(v, args), Symb(v', args') ->
        if v.name = v'.name && v.arity = v'.arity then
          let rec aux l1 l2 si' = match l1, l2 with
            | [], [] -> [ si' ]
            | ([], _ | _, []) -> failwith "Error: unify"
            | h1 :: tl1 , h2 :: tl2 ->
              let rec aux' tsi = match tsi with
                | [] -> []
                | h' :: tl' ->
                  (aux tl1 tl2 h') @ aux' tl'
              in
              let u = try unify h1 h2 si' with _ -> [] in
              aux' u
          in
          match (aux args args' si) with
          | [] -> raise SymbolClash
          | res -> res 
        
        else begin raise SymbolClash end

      (* Unification d'une variable avec un term *)
      | Var v, _ ->
        if is_occurs s t then begin raise OccursCheck end
        else [ Si.add s t si ]

      (* Unification d'un term avec une variable *)
      | _ , Var v -> unify t s si

      (* Unification d'un symbol simple avec un symbol AC *)
      | Symb _, SymbAC _ -> begin raise SymbolClash end

      (* Unification d'un symbol AC avec un symbol simple *)
      | SymbAC _, Symb _ -> begin raise SymbolClash end

      (* Unification de deux symbol AC --> algo unification modulo AC *)
      | SymbAC _, SymbAC _ ->


        (* procedure pour l'unification modulo ac *)
        let rec unifyAC l sigma real_sigma = match l with
          | [] -> 
            let r = sub_si sigma real_sigma in
            Some [ r ]
          | h :: tl ->

            let var = fst (fst h) in
            let sym = snd (fst h) in

            let sum = sub_term sigma (snd h) in
            let sum = sub_term real_sigma sum in
            let sym = sub_term real_sigma sym in

           if eq s t then unifyAC tl sigma real_sigma
            else

            match sym, sum with

            | Var s, _ when (s.name.[0] = '_') ->
              print_endline "NE RENTRE JAMAIS";

              if is_occurs sym sum then None
              else
                begin
                  let sigma' = Si.add sym sum sigma in
                  unifyAC tl sigma' real_sigma
                end
                
            | _, Var t when (t.name.[0] = '_') ->
                            
              if is_occurs sum sym then None
              else
                begin
                  if sym = (sub_term sigma sum) then print_endline "NE RENTRE JAMAIS"
                  else ();
                  let sigma' = Si.add sum sym sigma in
                  unifyAC tl sigma' real_sigma
                end

            | _ , SymbAC (_, Multiset []) -> None
      
            | _ , SymbAC _ when (notonlyVi_in sum) ->
              let new_sigma  = put_voidAC sum (sigma) in
              let new_sum = sub_term new_sigma sum in
              let sigma' = (try Some (unify sym new_sum real_sigma) with _ -> None) in
              begin
                match sigma' with
                | None -> None
                | Some s' ->
                  let rec aux l = match l with
                    | [] -> []
                    | h' :: tl' ->
                      begin
                        match unifyAC tl new_sigma h' with
                        | None -> aux tl'
                        | Some l' -> l' @ aux tl'
                      end
                  in
                  match (aux s') with
                  | [] -> None
                  | ll -> Some ll
              end

            | _ , SymbAC _ ->
              let permut = permut_list ((var,sym), sum) sigma in
              let permut = List.filter (fun ((k,v),si) -> eq sym (sub_term si sum)) permut in

              let aux some x =
                let r = (unifyAC tl (let (_,si) = x in si)) real_sigma in
                match r, some with
                | None, None -> None
                | _, None -> r
                | None , _ -> some
                | Some s, Some s' -> Some (s @ s')


              in
              List.fold_left aux None permut

            | (Var _, _ | _, Var _ | Symb _, _) ->

              begin
                match (try Some (unify sym sum real_sigma) with _ -> None) with
                | None -> None
                | Some new_sigma ->
                  let rec aux l = match l with
                    | [] -> []
                    | h' :: tl' ->
                      begin
                        match unifyAC tl sigma h' with
                        | None -> aux tl'
                        | Some l' -> l' @ aux tl'
                      end
                  in
                  match (aux new_sigma) with
                  | [] -> None
                  | ll -> Some ll
              end

            | SymbAC _, _ -> failwith "Error"
              
        in
        
        let t = purify s t in
        let ac1 = fst (snd t) in
        let ac2 = snd (snd t) in
        match ac1, ac2 with
        | SymbAC _, SymbAC _ ->
          begin
            let si1 = fst t in
            let si2 = purifyac_to_assocvar ac1 ac2 in
            let list = getSymb si1 si2 in
            let dif = diff_si si2 si1 in
            let list = list @ (getVar dif) in
            match (unifyAC list (Si.empty) si) with
            | None -> failwith "Pas unifiable"
            | Some s -> List.map (fun x -> sub_si x x) s
          end
        | _ ->
          let si1 = fst t in
          let ac1 = sub_term si1 ac1 in
          let ac2 = sub_term si1 ac2 in
          unify ac1 ac2 si
    end;;

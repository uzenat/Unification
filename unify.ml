
#load "multi_set.cmo";;  
#load "term.cmo";;  
#load "diophantienne.cmo";;  


open Multi_set;;
open Term;;
open Diophantienne;;

  
  
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
  | _ -> failwith "error : Not AC term"
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

  let (s, t) = match s, t with
    | Var _, Var _ ->
      (sub_term si s, sub_term si t)
    | Var _ , _ ->
      (sub_term si s, t)
    | _, Var _ ->
      (s, sub_term si t)
    | _ -> s, t
  in

  match s, t with

  (* Unification de deux variables egal *)
  | Var v, Var v' when v.name = v'.name -> si

  (* Unification de deux symbols *)
  | Symb(v, args), Symb(v', args') ->
    if v.name = v'.name && v.arity = v'.arity then
      List.fold_left2 (fun a b c -> unify b c a) si args args'
    else begin raise SymbolClash end

  (* Unification d'une variable avec un term *)
  | Var v, _ ->
    if is_occurs s t then begin raise OccursCheck end
    else Si.add s t si

  (* Unification d'un term avec une variable *)
  | _ , Var v -> unify t s si

  (* Unification d'un symbol simple avec un symbol AC *)
  | Symb(_, _), SymbAC(_, _) -> begin raise SymbolClash end

  (* Unification d'un symbol AC avec un symbol simple *)
  | SymbAC(_, _), Symb(_, _) -> begin raise SymbolClash end

  (* Unification de deux symbol AC --> algo unification modulo AC *)
  | ac1, ac2 -> 

    let put_voidAC ac si = match ac with
      | SymbAC(s, Multiset args) ->
        let rec put_voidAC' l si s = match l with
          | [] -> si
          | Elem(m, Var v) :: tl when (v.name.[0] = '_') ->
            put_voidAC' tl (Si.add (mk_Var v.name) (mk_SymbAC {name=s} []) si) s
          | _ :: tl -> put_voidAC' tl si s
        in
        put_voidAC' args si s.name
      | _ -> failwith "Error : not AC term"
    in

    let permut_list ac si = match ac with
      | ( (var, cons), SymbAC(s, Multiset l) ) ->
        let rec algo1 var cons hd0 l si = match l with
          | [] -> []
          | hd :: tl ->
            let aux1 x = let Elem(m, v) = x in (v, mk_SymbAC s []) in
            let hd2 = let Elem(m, v) = hd in (v, cons) in
            let tl2 = List.map aux1 tl in
            let r = hd0 @ [hd2] @ tl2 in
            let si' = List.fold_right (fun (x,y) -> Si.add x y) r (si) in
            let value = ( (var, cons) , (si') ) in
            value :: algo1 var cons (hd0 @ [(fst hd2, mk_SymbAC s [])]) tl si
        in
        algo1 var cons [] l si 
      | _ -> failwith "Error : not AC term"
    in

    let is_unifiable s t si =
      try
        Some (unify s t si)
      with _ -> None
    in

    let notonlyVi_in ac = match ac with
      | SymbAC(s, Multiset l) ->
        List.exists (fun x -> match x with | Elem(_, Var s) when (s.name.[0] = '_') -> false | _ -> true) l
      | _ -> failwith "Error : no AC term"
    in


    (* place les element de si' dans si *)
    let rec fusion_si2 si si' =
      let rec aux si l = match l with
        | [] -> si
        | (k, h) :: tl ->
          begin
            match (try Some (Si.find k si) with _ -> None) with
            | None ->
              begin
                match (try Some (Si.find h si) with _ -> None) with
                | None -> aux (Si.add k h si) tl
                | Some h' -> aux (Si.add k h' si) tl
              end
            | Some s ->
              begin
                match (try Some (unify h s (Si.empty)) with _ -> None) with
                | None -> raise SymbolClash
                | Some s' -> aux (fusion_si2 si s') tl
              end
          end
      in
      aux si (Si.bindings si')
    in

    let rec algo2 l sigma real_sigma = match l with
      | [] -> Some [ sub_si sigma real_sigma ]
      | h :: tl ->

        let var = fst (fst h) in
        let sym = snd (fst h) in

        let sum = sub_term sigma (snd h) in
        let sum = sub_term real_sigma (sum) in

        match sym, sum with
        | Var s, _ when (s.name.[0] = '_') ->
          let sigma' = Si.add sym sum sigma in
          algo2 tl sigma' real_sigma
        | Var s, Var t when (t.name.[0] = '_') ->
          let sigma' = Si.add sum sym sigma in
          algo2 tl sigma' real_sigma
        | Var s, Var t ->
          begin
            match (try Some (Si.find sym real_sigma) with _ -> None) with
            | None ->
              begin
                match (try Some (Si.find sum real_sigma) with _ -> None) with
                | None ->
                  let real_sigma' = Si.add sym sum real_sigma in
                  algo2 tl sigma real_sigma'
                | Some sum' ->
                  let real_sigma' = Si.add sym sum' real_sigma in
                  algo2 tl sigma real_sigma'
              end 
            | Some sum' ->
              let real_sigma' = Si.add sum sum' real_sigma in
              algo2 tl sigma real_sigma'
          end
        | Var s, Symb(_, _) ->
          if is_occurs sym sum then None
          else
            begin
              match (try Some (Si.find sym real_sigma) with _ -> None) with
              | None ->
                let real_sigma' = Si.add sym sum real_sigma in
                algo2 tl sigma real_sigma'
              | Some sum' ->
                begin
                  match sum' with
                  | Var ss ->
                    let real_sigma' = Si.add sum' sum real_sigma in
                    algo2 tl sigma real_sigma'
                  | _ ->
                    begin
                      match (try Some (unify sum sum' (Si.empty)) with _ -> None) with
                      | None -> None
                      | Some ss' -> algo2 tl sigma (fusion_si real_sigma ss')
                    end
                end
            end
        | Var s, SymbAC (ss, ms) when (notonlyVi_in sum) ->
          let new_sigma  = put_voidAC sum (sigma) in
          let new_sum = sub_term new_sigma sum in
          let sigma' = (try Some (unify sym new_sum (Si.empty)) with _ -> None) in
          begin
            match sigma' with
            | None -> None
            | Some s' ->
              try algo2 tl new_sigma (fusion_si2 real_sigma s')
              with _ -> None
          end
        | Var s, SymbAC (ss, ms)->
          let permut = permut_list ((var,sym), sum) sigma in
          let permut = List.filter (fun ((k,v),si) -> match (is_unifiable v (sub_term si sum) (Si.empty)) with None -> false | _ -> true) permut in
          let aux some x =
            let r = (algo2 tl (let (_,si) = x in si)) real_sigma in
            match r, some with
            | None, None -> None
            | Some s, None -> Some s
            | None , Some s' -> Some s'
            | Some s, Some s' -> Some (s @ s')
          in
          List.fold_left aux None permut
        | Symb(s, args), Var t when (t.name.[0] = '_') ->
          let sigma' = Si.add sum sym sigma in
          algo2 tl sigma' real_sigma
        | Symb(s, args), Var t ->
          begin
            match (try Some (Si.find sum real_sigma) with _ -> None) with
            | None ->
              let real_sigma' = Si.add sum sym real_sigma in
              algo2 tl sigma real_sigma'
            | Some sym' ->
              begin
                match sym' with
                | Var ss ->
                  let real_sigma' = Si.add sym' sym real_sigma in
                  algo2 tl sigma real_sigma'
                | _ ->
                  begin
                    match (try Some (unify sym sym' (Si.empty)) with _ -> None) with
                    | None -> None
                    | Some ss' -> algo2 tl sigma (fusion_si real_sigma ss')
                  end
              end
          end

        | Symb(s, _), Symb(t, _) ->
          begin
            let sigma' = (try Some (unify sym sum (Si.empty)) with _ -> None) in
            match sigma' with
            | None -> None
            | Some new_sigma ->
              algo2 tl sigma real_sigma
          end
        | Symb(_,_), SymbAC (ss, ms) when (notonlyVi_in sum) ->
          let new_sigma  = put_voidAC sum (sigma) in
          let new_sum = sub_term new_sigma sum in
          let sigma' = (try Some (unify sym new_sum (Si.empty)) with _ -> None) in
          begin
            match sigma' with
            | None -> None
            | Some s' -> algo2 tl new_sigma (fusion_si real_sigma s')
          end
        | Symb(_,_), SymbAC (_, _) ->
          let permut = permut_list ((var,sym), sum) sigma in
          let permut = List.filter (fun ((k,v),si) -> match (is_unifiable v (sub_term si sum) (Si.empty)) with None -> false | _ -> true) permut in
          let aux some x =
            let r = (algo2 tl (let (_,si) = x in si)) real_sigma in
            match r, some with
            | None, None -> None
            | Some s, None -> Some s
            | None , Some s' -> Some s'
            | Some s, Some s' -> Some (s @ s')
          in
          List.fold_left aux None permut          
        | _ -> failwith (" " ^ (string_of_term sym) ^ " = " ^ (string_of_term sum))
    in

    let t = purify ac1 ac2 in
    let ac1 = fst (snd t) in
    let ac2 = snd (snd t) in
    let si1 = fst t in
    let si2 = purifyac_to_assocvar ac1 ac2 in
    let list = getSymb si1 si2 in
    let dif = diff_si si2 si1 in
    let list = list @ (getVar dif) in

    let res = match (algo2 list (Si.empty) (Si.empty)) with None -> failwith "Pas unifiable" | Some s -> s in

    List.hd (List.map (fun x -> sub_si x x) res);;

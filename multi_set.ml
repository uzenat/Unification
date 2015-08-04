(**** Multi-ensemble ****)

(* definition du type *)
type 'a ms =
  | Multiset of 'a ms list
  | Elem of int * 'a;;

  
(* donne la taille d'un multi-ensemble *)
let len_Multiset ms = match ms with
  | Multiset v -> List.length v
  | _ -> -1
  
(* compare deux multi-ensemble *)
let cmpr_Multiset cmpr ms1 ms2 =
  let rec cmpr_Multiset' cmpr l1 l2 = match l1, l2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | h1 :: tl1, h2 :: tl2 ->
       match h1, h2 with
       | Elem(m, v), Elem(m', v') ->
	  let c = cmpr v v' in
	  if c <> 0 then c
	  else
	    if m < m' then -1
	    else if m > m' then 1
	    else cmpr_Multiset' cmpr tl1 tl2 
       | Multiset v, Multiset v' ->
	  let c = cmpr_Multiset' cmpr v v' in
	  if c <> 0 then c
	  else cmpr_Multiset' cmpr tl1 tl2
       | Elem(_,_), Multiset _ -> -1
       | Multiset _, Elem(_,_) -> 1
  in
  match ms1, ms2 with
  | Elem(m, v), Elem(m', v') ->
     let c = cmpr v v' in
     if c <> 0 then c
     else
       if m < m' then -1
       else if m > m' then 1
       else 0
  | Multiset v, Multiset v' -> cmpr_Multiset' cmpr v v'
  | Elem(_,_), Multiset _ -> -1
  | Multiset _, Elem(_,_) -> 1

(* lissage *)
let flatten_Multiset ms =
  let rec flatten_Multiset' ms = match ms with
    | [] -> []
    | h :: tl ->
       match h with
       | Elem(m, v) ->
	  Elem(m, v) :: (flatten_Multiset' tl)
       | Multiset v -> (flatten_Multiset' v) @ (flatten_Multiset' tl)
  in
  match ms  with
  | Multiset v -> Multiset (flatten_Multiset' v)
  | x -> x

  
(* fusionne deux multi-ensemble *)
let fusion cmpr s1 s2 = 
  let rec fusion' cmpr s1 s2 = match s1, s2 with
    | [], [] -> []
    | [], s2' -> s2'
    | s1', [] -> s1'
    | h1::tl1, h2::tl2 ->
       match h1, h2 with
       | Elem( m1, v1), Elem( m2, v2) ->
	  if cmpr v1 v2 = -1 then h1 :: fusion' cmpr tl1 (h2::tl2)
	  else if cmpr v1 v2 = 1 then h2 :: fusion' cmpr (h1::tl1) tl2
	  else Elem(m1 + m2, v1) :: fusion' cmpr tl1 tl2
       | _ -> failwith "impossible"
  in
  match s1, s2 with
  | Multiset l1, Multiset l2 -> Multiset (fusion' cmpr l1 l2)
  | Elem(m, v), Multiset l2 -> Multiset (fusion' cmpr [Elem(m,v)] l2)
  | Multiset l1, Elem(m',v') -> Multiset (fusion' cmpr l1 [Elem(m',v')])
  | e1, e2 -> Multiset (fusion' cmpr [e1] [e2])


(* scinde un multi ensemble en deux *)
let scinde l =
  let rec scinde' l r n = match l, n with
    | _, 0 -> r, l
    | h::tl, _ -> scinde' tl (r @ [h]) (n-1)
    | _ -> failwith "error"
  in
  match l with
  | Multiset v ->
     let r = (scinde' v [] ((List.length v)/2)) in
     Multiset (fst r), Multiset (snd r)
  | x -> x, Multiset [];;

(* rearrange le multiensemble par multipliciter *)
let rec fusion_rec cmpr l =
  let r = scinde l in
  let e1 = fst r in
  let e2 = snd r in
  let le1 = len_Multiset e1 in
  let le2 = len_Multiset e2 in
  if (le1 > 1) || (le2 > 1) then
    fusion cmpr (fusion_rec cmpr e1) (fusion_rec cmpr e2)
  else fusion cmpr e1 e2;;
  
(* construit un multi-ensemble a partir d'une liste de type 'a *)
let mk_Multiset cmpr l =
  let rec aux l = match l with
    | [] -> []
    | h :: tl -> Elem(1,h) :: (aux tl)
  in
  fusion_rec cmpr (Multiset(aux l));;

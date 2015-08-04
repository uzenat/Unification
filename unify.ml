open Term;;

  
(**** Definition d'une subsitution ****)

(* module *)
module AssocMap =
  struct
    type t = Term.term
    let compare = compare_term 
  end;;
module Si = Map.Make (AssocMap)

(* effectue une substitution *)
let sub si var =
  try
    Si.find var si 
  with Not_found -> var;;
let rec sub_term si term = match term with
  | Symb(s, args) -> mk_Symb s (List.map (sub_term si) args)
  | Var s -> sub si term
  | _ -> assert false;;


  
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


  
let x = mk_Var "x";;
let y = mk_Var "y";;
let z = mk_Var "z";;
let u = mk_Var "v";;
let v = mk_Var "v";;

let a = mk_Symb {name="a" ; arity=0} [];;
let b = mk_Symb {name="b" ; arity=0} [];;
let c = mk_Symb {name="c" ; arity=0} [];;
let d = mk_Symb {name="d" ; arity=0} [];;

let f x = mk_Symb {name="f" ; arity=1} [ x ];;
let g x = mk_Symb {name="g" ; arity=1} [ x ];;

let p x y = mk_Symb {name="p" ; arity=2} [ x ; y ];;
let q x y = mk_Symb {name="q" ; arity=2} [ x ; y ];;
  
let r x y z = mk_Symb {name="r" ; arity=3} [ x ; y ; z ];;

let s x y z t = mk_Symb {name="s" ; arity=4} [ x ; y ; z ; t];;

(* Exemple 1: p(a, x) = p(z, f(z)) *)
  
let p1 = p a x;;
let p2 = p z (f z) ;;

let solution = Si.empty;;
let solution = Si.add z a solution;;
let solution = Si.add x (f a) solution;;

let resultat = unify p1 p2 (Si.empty);;

let bool = Si.equal eq solution resultat;;

  assert bool;;
  
(* Exemple 2: r(g(x), f(y), a) = r(y, f(b), a) *)

let p1 = r (g x) (f y) a ;;
let p2 = r y (f b) a ;;
Si.bindings (unify p1 p2 (Si.empty));;
let bool =
  try 
    let r = unify p1 p2 (Si.empty) in
    false
  with SymbolClash -> true;;
  
  assert bool;;


    
(* Exemple 3: r(x, y, z) = r(q(v, v), q(x, x), q(y, y)) *)

let p1 = r x y z;;
let p2 = r (q v v) (q x x) (q y y);;

let solution = Si.empty;;
let solution = Si.add x (q v v) solution;;
let solution = Si.add y (q (q v v) (q v v)) solution;;
let solution = Si.add z (q (q (q v v) (q v v)) (q (q v v) (q v v))) solution;;

let resultat = unify p1 p2 (Si.empty);;

let bool = Si.equal eq solution resultat;;

  assert bool;;

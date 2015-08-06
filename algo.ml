#load "multi_set.cmo";;
#load "term.cmo";;
#load "diophantienne.cmo";;    
open Multi_set;;
open Term;;
open Diophantienne;;
  
let a = mk_Symb {name="a" ; arity=0} [];;
let b = mk_Symb {name="b" ; arity=0} [];;

let x = mk_Var "x";;
let y = mk_Var "y";;
let z = mk_Var "z";;

let u = mk_Var "u";;
let w = mk_Var "w";;

let p1 = mk_SymbAC {name="+"} [ x ; x ; y ; a ];;
let p2 = mk_SymbAC {name="+"} [ b ; y ];;

let subs = [ (u, a) ; (w, b) ];;

let p3 = mk_SymbAC {name="+"} [ x ; u ];;
let p4 = mk_SymbAC {name="+"} [ w ; y ];;

let c1 = [ 2 ; 1 ; 1 ] ;;
let c2 = [ 2 ; 1 ] ;;

let equ = Equation(Coefficient(c1),Coefficient(c2));;
let r = VectSet.elements (procedure equ);;
let gen_var s i = mk_Var (s ^ (string_of_int i));;
let int_to_elem v i = Elem(i, v);;
let rec assoc_Var r i = match r with
  | [] -> []
  | h::tl ->
     match h with
       VectMod.Vect(v1, v2) ->
       (List.map (int_to_elem (gen_var "_v" i)) v1,
	List.map (int_to_elem (gen_var "_v" i)) v2) :: assoc_Var tl (i+1);;

let v1 = mk_Var "v1";;
let v2 = mk_Var "v2";;
let v3 = mk_Var "v3";;
let v4 = mk_Var "v4";;

let t1 = (Elem(1, v3), Elem(1, v4));;
let t2 = (Elem(1, v1), Elem(1, v2));;
let t3 = (Elem(1, v2), Elem(1, v4));;
let t4 = (Elem(1, v1), Elem(1, v3));;

let si = [(x, t1) ; (u, t2) ; (w, t3) ; (y, t4) ];;

let s01 = [(v1, a) ; (v2, b)];;
let s02 = [(v1, a) ; (v3, b)];;
let s03 = [(v1, a) ; (v4, b)];;
let s04 = [(v2, a) ; (v3, b)];;
let s05 = [(v2, a) ; (v4, b)];;
let s06 = [(v3, a) ; (v4, b)];;

let s07 = [(v2, a) ; (v1, b)];;
let s08 = [(v3, a) ; (v1, b)];;
let s09 = [(v4, a) ; (v1, b)];;
let s010 = [(v3, a) ; (v2, b)];;
let s011 = [(v4, a) ; (v2, b)];;
let s012 = [(v4, a) ; (v3, b)];;

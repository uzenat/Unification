open Multi_set;;
open Term;;
open Diophantienne;;
open Unify;;

let x = mk_Var "x";;
let x1 = mk_Var "x1";;
let x2 = mk_Var "x2";;
let x3 = mk_Var "x3";;
let x4 = mk_Var "x4";;
let x5 = mk_Var "x5";;
let x6 = mk_Var "x6";;
let x7 = mk_Var "x7";;
let x8 = mk_Var "x8";;
let x9 = mk_Var "x9";;

let y = mk_Var "y";;
let z = mk_Var "z";;
let t = mk_Var "t";;
let u = mk_Var "u";;
let w = mk_Var "w";;

let a = mk_Symb {name="a" ; arity=0}  []
let a1 = mk_Symb {name="a1" ; arity=0}  []
let a2 = mk_Symb {name="a2" ; arity=0}  []
let a3 = mk_Symb {name="a3" ; arity=0}  []
let a4 = mk_Symb {name="a4" ; arity=0}  []

let b = mk_Symb {name="b" ; arity=0}  []
let b1 = mk_Symb {name="b1" ; arity=0}  []
let b2 = mk_Symb {name="b2" ; arity=0}  []
let b3 = mk_Symb {name="b3" ; arity=0}  []
let b4 = mk_Symb {name="b4" ; arity=0}  []

let c = mk_Symb {name="c" ; arity=0}  []
let c1 = mk_Symb {name="c1" ; arity=0}  []
let c2 = mk_Symb {name="c2" ; arity=0}  []
let c3 = mk_Symb {name="c3" ; arity=0}  []
let c4 = mk_Symb {name="c4" ; arity=0}  []

let d = mk_Symb {name="d" ; arity=0}  []
let d1 = mk_Symb {name="d1" ; arity=0}  []
let d2 = mk_Symb {name="d2" ; arity=0}  []
let d3 = mk_Symb {name="d3" ; arity=0}  []
let d4 = mk_Symb {name="d4" ; arity=0}  []

let e = mk_Symb {name="e" ; arity=0}  []
let e1 = mk_Symb {name="e1" ; arity=0}  []
let e2 = mk_Symb {name="e2" ; arity=0}  []
let e3 = mk_Symb {name="e3" ; arity=0}  []
let e4 = mk_Symb {name="e4" ; arity=0}  []

let f x = mk_Symb {name="f" ; arity=1}  [x]
let g x = mk_Symb {name="g" ; arity=1}  [x]



let plus s = mk_SymbAC {name="+"} s;;
let fois s = mk_SymbAC {name="."} s;;
let moins s = mk_SymbAC {name="-"} s;;



let test s t  =
 
  let t1 = Sys.time () in
  let uu = unify s t (Si.empty) in
  let t2 = Sys.time () in
  let u = List.map (fun x -> sub_si x x) uu in
  let nf = List.filter (fun x -> not (eq2 x s t)) u in
  print_endline ((string_of_term s) ^ " = " ^ (string_of_term t));
  print_endline ("Temps : " ^ (string_of_float ((-.) t2 t1)));
  print_endline ("Nombre de resultat : " ^ (string_of_int(List.length u)));
  print_endline ((string_of_term (sub_term (List.hd u) s)) ^ " = " ^ (string_of_term (sub_term (List.hd u) t)));
  print_endline ("Nombre de resultat faux : " ^ (string_of_int (List.length nf)));
  List.iter (fun x -> print_endline (string_of_si x)) nf;
  print_endline "";;


let () =

  (* Test avec uniquement des constantes et des variables *)

  (* test 1 *)
  let s1 = plus [ x ; b ] in
  let t1 = plus [ y ; a ] in
  test s1 t1;

  (* test 2 *)
  let s2 = plus [ x ; y ] in
  let t2 = plus [ b ; a ] in
  test s2 t2;

  (* test 3 *)
  let s3 = moins [ x ; y ; z ; t ] in
  let t3 = moins [ a ; b ; c ; d ] in
  test s3 t3;

  (* test 4 *)
  let s4 = plus [ x ; x ; y ; a ; a ] in
  let t4 = plus [ z ; b ; b ; c ; c ; u ; u ] in
  test s4 t4;

  (* test 5 *)
  let s5 = plus [ y ; y ; a ; a ; b ; b ; b ; b ; x ; x ; x ; x ; c ; c ] in
  let t5 = plus [ t ; t ; t ; t ; d ; d ; d ; d ; e ; e ; w ; w ; w ; w ] in
  test s5 t5;

  (* test 6 *)
  let s6 = plus [ x1 ; x2 ; x3 ; a1 ; a2 ; a3 ] in
  let t6 = plus [ x5 ; x6 ; x8 ; x9 ; b1 ; b3 ; b4 ] in
  test s6 t6;

 

  (* Test avec variables, ctes et des fonction contenant des ctes` *)

  (* test 7 *)
  let s7 = plus [ x ; f a ] in
  let t7 = plus [ y ; f b ] in
  test s7 t7;

  (* test 8 *)
  let s8 = plus [ x ; f (f (f (f a))) ] in
  let t8 = plus [ y ; f (f (f (f a))) ] in
  test s8 t8;

  (* test 9 *)
  let s9 = plus [ x ; f (f (f (f b))) ] in
  let t9 = plus [ y ; f (f (f (f a))) ] in
  test s9 t9;

  
  
  (* Test avec tout *)

  (* test 10 *)
  let s10 = moins [ f y ; f a ] in
  let t10 = moins [ f x ; f b ] in
  test s10 t10;

  (* test 11 *)
  let s11 = plus [ x ; f x ] in
  let t11 = plus [ y ; f y ] in
  test s11 t11;

  (* test 12 *)
  let s12 = plus [ x ; y ] in
  let t12 = plus [ f a ; f b ] in
  test s12 t12;

  (* test 13 *)
  let s13 = fois [ f a ; f b ] in
  let t13 = fois [ f y ; f x ] in
  test s13 t13;

  (* test 14 *)
  let s14 = fois [ f (f (f (f (f x)))) ; z ] in
  let t14 = fois [ f (f (f (f (f a)))) ; x ] in
  test s14 t14;

  (* test 15 *)
  let s15 = plus [ f (f (f (f (f x)))) ; z ; f w ; w ] in
  let t15 = plus [ f (f (f (f (f a)))) ; x ; f y ; y] in
  test s15 t15;

  (* test 16 *)
  let s16 = plus [ f (f x) ; z ; f w ; w ; a ; a ; f (f (f (y))) ; f (f w) ; f (f t ) ; t ] in
  let t16 = plus [ f (f y) ; x ; f y ; y ; b ; b ; f (f x) ; f (f (f (f (f a))))] in
  test s16 t16;

  (* test 17 *)
  let s17 = plus [ f t ; f (f y) ; f (f (f w)) ; t ; t ; y ] in
  let t17 = plus [ f w ; f (f u) ; f (f (f (f x))) ; u ; u ; w ] in
  test s17 t17;

  (* test 18 *)
  let s18 = plus [ s13 ; s3 ] in
  let t18 = plus [ t13 ; t3 ] in
  test s18 t18;

  (* test 19 *)
  let s19 = plus [ s10 ; t14 ] in
  let t19 = plus [ s14 ; t10 ] in
  test s19 t19;

  (* test 20 *)
  let s20 = moins [ fois [ x ; x ] ; fois [ y ; y ] ] in
  let t20 = moins [ fois [ a ; a ] ; fois [ b ; b ] ] in
  test s20 t20;

  

;;
 



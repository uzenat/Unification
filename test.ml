open Multi_set;;
open Term;;
open Diophantienne;;
open Unify;;

let x = mk_Var "x";;
let y = mk_Var "y";;
let z = mk_Var "z";;

let a = mk_Symb {name="a" ; arity=0}  []
let b = mk_Symb {name="b" ; arity=0}  []
let c = mk_Symb {name="c" ; arity=0}  []
let d = mk_Symb {name="d" ; arity=0}  []

let p x = mk_Symb {name="p" ; arity=1}  [x]
let q x = mk_Symb {name="q" ; arity=1}  [x]
let r x = mk_Symb {name="r" ; arity=1}  [x]

let s x y = mk_Symb {name="s" ; arity=2}  [x; y]
let t x y = mk_Symb {name="t" ; arity=2}  [x; y]
let u x y = mk_Symb {name="u" ; arity=2}  [x; y]


let op1 = mk_SymbAC {name="+"} [ a ; b ; c ; d ];;
let op2 = mk_SymbAC {name="+"} [ c ; d ; b ; a ];;
let op3 = mk_SymbAC {name="+"} [ a ; b ];;
let op4 = mk_SymbAC {name="+"} [ a ; a ; a; b ];;
let op5 = mk_SymbAC {name="+"} [ mk_SymbAC {name="+"} [ a ; b ] ; d ; c ];;
let op6 = mk_SymbAC {name="+"} [ a ];;

let ac1 = mk_SymbAC {name="."} [ p x ; p y ; x ; y ];;
let ac2 = mk_SymbAC {name="."} [ a ; p b ; p y ; q x ; z ];;

let () =
  
  print_string "TEST 1 : ";
  assert (eq op1 op2);
  print_endline "DONE";

  print_string "TEST 2 : ";
  assert (not (eq op3 op2));
  print_endline "DONE";

  print_string "TEST 3 : ";
  assert (not (eq op3 op4));
  print_endline "DONE";

  print_string "TEST 4 : ";
  assert (not (eq op4 op2));
  print_endline "DONE";

  print_string "TEST 5 : ";
  assert (eq op5 op1);
  print_endline "DONE";

  print_string "TEST 6 : ";
  assert (eq (mk_SymbAC {name="plus"} [a]) a);
  print_endline "DONE";

  print_string "TEST 7 : ";
  assert (eq (mk_SymbAC {name="plus"} [a;mk_SymbAC {name="plus"} []]) a);
  print_endline "DONE";

  print_string "TEST 8 : ";
  assert (eq op6 a);
  print_endline "DONE";

  print_string "TEST 9 : ";
  assert (eq2 (unify ac1 ac2 (Si.empty)) ac1 ac2);
  print_string "DONE";

  print_endline ""

;;




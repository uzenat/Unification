#load "multi_set.cmo";;
#load "term.cmo";;
#load "diophantienne.cmo";;
open Multi_set;;
open Term;;
open Diophantienne;;
    
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

let () =
  assert (eq op1 op2);
  assert (not (eq op3 op2));
  assert (not (eq op3 op4));
  assert (not (eq op4 op2));
  assert (eq op5 op1);
  assert (eq (mk_SymbAC {name="plus"} [a]) a);
  assert (eq (mk_SymbAC {name="plus"} [a;mk_SymbAC {name="plus"} []]) a);
  assert (eq op6 a);;




type 'a elem = Elem of int * 'a
type 'a ms = Multiset of 'a elem list
val len_Multiset : 'a ms -> int
val cmpr_Multiset : ('a -> 'b -> int) -> 'a ms -> 'b ms -> int
val diff : ('a -> 'a -> int) -> 'a ms -> 'a ms -> 'a ms
val fusion : ('a -> 'a -> int) -> 'a ms -> 'a ms -> 'a ms
val scinde : 'a ms -> 'a ms * 'a ms
val fusion_rec : ('a -> 'a -> int) -> 'a ms -> 'a ms
val mk_Multiset : ('a -> 'a -> int) -> 'a list -> 'a ms
val mk_Multiset2 : 'a elem list -> 'a ms

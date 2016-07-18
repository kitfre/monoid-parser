open Core.Std

type symbol = char
type state = int
type elem = state -> Maybe state

type dfa = { states : state list;
             start : state;
             finals : state list;
             delta : symbol -> state -> state option;
             accept : elem -> bool;
           }

val createElem : dfa -> symbol -> elem
val fword : symbol list -> dfa -> elem list
val compose' : ('a -> 'b option) -> ('b -> 'c option) -> ('a -> 'c option)
val mparse : elem list -> dfa -> bool
val parse : string -> dfa -> bool
val createAccept' : state list -> state -> (elem -> bool)

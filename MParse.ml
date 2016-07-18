open Core.Std

type symbol = char
type state = int
type elem = (state -> state option)

type dfa = { states : state list; 
             start : state; 
             finals : state list;
             delta : (symbol -> state -> state option); 
             accept : elem -> bool; 
           }

(* Utilitity methods *)

(* compose' helper method *)
let compose' f g = (fun x -> match f x with
    | Some x -> g x
    | None -> None
    )

let createAccept' fs start =
    let extract s = match s with
        | Some x -> x
        | _      -> 0
    in
        (fun fa -> List.mem fs (extract (fa start)))

(* creates monoid element from element of alphabet *)
let createElem dfa s = dfa.delta s

(* converts input word to monoid representation *)
let fword xs dfa = List.map xs ~f:(createElem dfa)

(* mparse sequential algorithm *)
let rec mparse xs dfa = match xs with
    | [x] -> dfa.accept x
    | (x :: y :: ys) -> mparse ((compose' x y) :: ys) dfa

(* mother function, parses from dfa *)
let parse s dfa = mparse (fword (String.to_list s) dfa) dfa

(* Example structures *)

(* test dfa delta function *)
let testdel sym st = match (sym, st) with
    | ('a', 1) -> Some 2
    | ('a', 2) -> Some 2
    | ('b', 1) -> Some 1
    | ('b', 2) -> Some 1
    | (_, _)   -> None

let test_dfa = { states = [1;2];
                 start = 1;
                 finals = [1];
                 delta = testdel;
                 accept = (createAccept' [1] 1);
               }
            

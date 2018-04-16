module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end

(* FIX ME !!! *)
module type MAKEFST =
    functor (Pair : PAIR) -> VAL

module MakeFst : MAKEFST =
    functor (Pair : PAIR) ->
        struct
            let x = fst (Pair.pair)
        end


module type MAKESND =
    functor(Pair : PAIR) -> VAL

module MakeSnd : MAKESND =
    functor (Pair : PAIR) -> 
struct
    let x = snd (Pair.pair)
end


module Pair : PAIR = struct let pair = ( 21, 42 ) end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)


let() = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x

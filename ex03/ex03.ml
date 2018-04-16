module type FRACTIONNAL_BITS = sig val bits : int end


module type FIXED = sig
    type t
    val of_float : float -> t
    val of_int : int -> t
    val to_float : t -> float
    val to_int : t -> int
    val to_string : t -> string
    val zero : t
    val one : t
    val succ : t -> t
    val pred : t -> t
    val min : t -> t -> t
    val max : t -> t -> t
    val gth : t -> t -> bool
    val lth : t -> t -> bool
    val gte : t -> t -> bool
    val lte : t -> t -> bool
    val eqp : t -> t -> bool (** physical equality *)
    val eqs : t -> t -> bool (** structural equality *)
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val foreach : t -> t -> (t -> unit) -> unit
end

module type MAKE =
    functor (Fractionnal_bits : FRACTIONNAL_BITS) ->
    FIXED

module Make : MAKE =
    
    functor (Fractionnal_bits : FRACTIONNAL_BITS) ->
        struct
            type t = int  
            let of_float f = int_of_float(f *. 2. ** float(Fractionnal_bits.bits))
            let of_int i = i * int_of_float(2. ** float(Fractionnal_bits.bits))
            let to_float a = float_of_int(a) /. 2. ** float(Fractionnal_bits.bits)
            let to_int a = int_of_float(to_float a)
            let to_string a = string_of_float (to_float a)
            let zero = 0
            let one = of_int 1
            let succ a = a + 1
            let pred a = a - 1
            let min a b = if b > a then b else a
            let max a b = if b < a then b else a
            let gth a b = a > b
            let lth a b = a < b 
            let gte a b = a >= b 
            let lte a b = a <= b 
            let eqp a b = a == b 
            let eqs a b = a = b
            let add a b = of_float(to_float(a) +. to_float(b))
            let sub a b = a - b
            let mul a b = of_float(to_float(a) *.to_float(b))
            let div a b =  of_float(to_float(a) /. to_float(b))
            let rec foreach a b funk = 
                if (gth a b)
                    then ()
                else
                    (funk a; foreach (succ a) b funk)
        end

       

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
    let x8 = Fixed8.of_float 21.10 in
    let y8 = Fixed8.of_float 21.32 in
    let r8 = Fixed8.add x8 y8 in
    print_endline (Fixed8.to_string r8);
    Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
    let a8 = Fixed8.of_int 8 in
    let z8 = Fixed8.pred a8 in
    let b8 = (Fixed8.mul a8 x8) in
    print_endline (Fixed8.to_string a8);
    print_endline (Fixed8.to_string z8);
    print_endline (Fixed8.to_string(Fixed8.sub a8 (Fixed8.min z8 a8)));
    print_endline (Fixed8.to_string(Fixed8.sub a8 (Fixed8.max z8 a8)));
    print_endline (string_of_bool(Fixed8.eqp a8 a8));
    print_endline (string_of_bool(Fixed8.eqs a8 b8));
    print_endline (string_of_bool(Fixed8.gth a8 b8));
    print_endline (string_of_bool(Fixed8.lth a8 b8));
    print_endline (string_of_bool(Fixed8.gte a8 b8));
    print_endline (string_of_bool(Fixed8.lte a8 b8));
    print_endline (Fixed8.to_string(Fixed8.mul a8 b8));
    print_endline (Fixed8.to_string(Fixed8.div a8 b8))

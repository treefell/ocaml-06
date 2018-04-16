module StringHash =
    struct
        type t = string
        let equal a b =  a = b
        let hash str = 
            let rec adler32 hash1 hash2 count =
                if count > (String.length - 1)
                    then (hash2 * int_of_float(2**16) ) land hash1
                else
                    adler32 (hash1 + (String.get str count) mod 65521)
                            ((hash2 + hash1) mod 65521) (count + 1)
                             (* hashing string with function*)
            in
            adler32 0 1 0
    end

module StringHashtbl = Hashtbl.Make(StringHash)


let () =
    let ht = StringHashtbl.create 5 in
    let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
    let pairs = List.map (fun s -> (s, String.length s)) values in
    List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
    SringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht

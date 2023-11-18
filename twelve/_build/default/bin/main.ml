type 'a rle =
    | One of 'a
    | Many of int * 'a;;

(* hello from erik *)
let rec spread_many many acc = 
        let count, element = many in 
        if count > 0 then spread_many (count - 1, element) (element :: acc) else acc
;;

let decode list = 
        let rec decode_internal acc = function
                | [] -> acc 
                | hd :: rest -> 
                                match hd with 
                                | One a -> decode_internal (a :: acc) rest
                                | Many (count, element) -> decode_internal (spread_many (count, element) acc) rest 
        in decode_internal [] list;;

let result = List.rev (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);;

List.iter print_endline result;;


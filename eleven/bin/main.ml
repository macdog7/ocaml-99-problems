type 'a rle = 
        | One of 'a
        | Many of int * 'a;;

let encode list = 
        let rec encode_internal count acc = function 
                | [] -> acc 
                | [single] -> if count = 0 then ((One single) :: acc) else (Many (count + 1, single) :: acc)
                | first :: (second :: _ as rest) -> if first = second then encode_internal (count + 1) acc rest else 
                        encode_internal 0 ((if count = 0 then One first else Many (count + 1, first)) :: acc) rest in 
        encode_internal 0 [] list
;;

let printarr = List.iter (fun el -> match el with 
| Many (a, t) -> Printf.printf "Count: %d, Element: %s \n" a t 
| One a -> print_endline ("Count: n/a, Element: " ^ a)
);; 

let _ = printarr (List.rev (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]));;




let pack list = 
        let rec aux current acc = function 
                | [] -> [] 
                | [single] -> (single :: current) :: acc
                | a :: (b :: _ as t) -> 
                                if a = b then aux (a :: current) acc t 
                                else aux [] ((a :: current) :: acc) t in
        aux [] [] list;; 

let result = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

let print = List.iter print_endline;;

let _ = List.iter (fun el -> print_endline "["; print el; print_endline "]") result;; 


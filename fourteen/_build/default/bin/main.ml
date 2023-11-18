let rec duplicate acc = function 
        | [] -> acc  
        | [single] -> single :: single :: acc 
        | h :: t -> duplicate (h :: h :: acc) t
;;

let result = duplicate [] ["a"; "b"; "c"; "c"; "d"];;

let _ = List.iter print_endline (List.rev result);; 

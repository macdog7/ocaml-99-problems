let drop list number = 
        let rec drop_n acc current number = function
                | [] -> acc 
                | [single] -> if current + 1 = number then acc else (single :: acc) 
                | hd :: tail -> if current + 1 = number then drop_n acc 0 number tail else drop_n (hd :: acc) (current + 1) number tail
        in 
        List.rev (drop_n [] 0 number list);;

let _ = List.iter print_endline (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3);;


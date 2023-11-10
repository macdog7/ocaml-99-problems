let rec find_element n arr index =
        match arr with 
        | [] -> None 
        | hd :: _ when index = n -> Some hd 
        | _ :: rest -> find_element n rest (index + 1) 
;;


let test = [5;10;15;20] in 
let el = find_element 5 test 0 in 
match el with
| None -> print_endline "none"
| Some x -> print_int x 
;;


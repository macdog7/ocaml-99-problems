let rec reverse list acc = match 
list with
| [] -> acc
| h :: t -> reverse t (h :: acc)
;;


let result = reverse ["test"; "blah"; "dog"] [] in 
List.iter print_endline result
;;


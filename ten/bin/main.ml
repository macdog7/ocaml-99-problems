let encode list = 
        let rec traverse count acc = function 
                | [] -> acc 
                | [single_element] -> (count + 1, single_element) :: acc 
                | first :: (second :: _ as rest) -> if first = second then traverse (count + 1) acc rest else traverse 0 ((count + 1, first) :: acc) rest
        in 
        traverse 0 [] list;; 



let result = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in 
List.iter (fun el -> let count, element = el in Printf.printf "Count: %d, Element: %s \n" count element) result;;


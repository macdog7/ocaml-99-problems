let compress list = 
        let rec smash acc = function 
                | []  -> acc 
                | [first] -> first :: acc
                | first :: (second :: _ as rest) -> if first = second then smash acc rest else smash (first :: acc) rest
        in 
List.rev (        smash [] list);;

let _ = List.iter print_endline (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);;


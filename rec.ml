let arr = [1; 2; 3; 4; 5]

let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest

let ans = sum arr

let rec len lst = match lst with
    [] -> 0
  | first :: rest -> 1 + len rest

let len1 = len arr
let len2 = List.length arr

let rec even lst = match lst with
    [] -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest else even rest

let evenArr = even arr

let rec concat lst = match lst with
    [] -> ""
  | first::rest -> first ^ concat rest

let str = concat ["あ"; "い"; "う"; "え"; "お"]

let rec minimun ls = match ls with
    [] -> max_int
  | first::rest -> 
    let min_rest = minimun rest in
    if first <= min_rest
    then first
    else min_rest

let ans = minimun [5; 6; 2; 8; 0]

let rec equal_length ls1 ls2 = match (ls1, ls2) with
    ([], []) -> true
  | ([], first2::rest2) -> false
  | (first1::rest1, [] ) -> false
  | (first1::rest1, first2::rest2 ) -> equal_length rest1 rest2

let a = equal_length [1;2] [1;2] = true
let b = equal_length [1;2;3] [1;2] = false
let c = equal_length [1;2] [1;2;3] = false
let c = equal_length [] [] = true

let sum_list ls  = 
  let rec sum_num ls total0 = match ls with
      []-> []
    |first::rest -> total0 + first::sum_num rest (total0+first) 
  in sum_num ls 0

let ans = sum_list [3;2;1;4]

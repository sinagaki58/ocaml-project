let rec insert ls n = match ls with
    [] -> n::[]
  | first::rest -> if first >= n then n::ls else first::insert rest n

let rec insert_sort ls = match ls with
    [] -> []
  | first::rest -> insert (insert_sort rest) first

let arr = [5; 6; 2; 8; 0]

let ans = insert [] 6
let ans = insert arr 0
let ans = insert arr 2
let ans = insert arr 6

let ans = insert_sort arr


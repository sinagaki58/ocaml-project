type person_t = { 
  name : string;	(* 名前 *) 
  shincho : float;	(* 身長 *) 
  taiju : float;	(* 体重 *) 
  tsuki : int;		(* 誕生月 *) 
  hi : int;		(* 誕生日 *) 
  ketsueki : string;	(* 血液型 *) 
} 

let person1 = 
  {name = "浅井"; 
   shincho = 1.72; 
   taiju = 58.5; 
   tsuki = 9; 
   hi = 17; 
   ketsueki = "A" 
  } 

let person2 = { 
  name = "宮原"; 
  shincho = 1.63; 
  taiju = 55.0; 
  tsuki = 6; 
  hi = 30; 
  ketsueki = "B" 
} 

let person3 = { 
  name = "中村"; 
  shincho = 1.68; 
  taiju = 63.0; 
  tsuki = 6; 
  hi = 6; 
  ketsueki = "O" 
} 

let person_list = [person1; person2; person3]

(* 血液型がA型の人数を数える *)
let rec count_ketsueki_A ls = match ls with
    [] -> 0
  | person_t::rest -> if person_t.ketsueki = "A" then 1 + count_ketsueki_A rest
    else count_ketsueki_A rest

let ketsueki_a =  count_ketsueki_A person_list

(* 乙女座かどうか判定する *)
let is_otomeza birth = match birth with
    (8, d) -> if d >= 23 then true else false
  | (9, d) -> if d <= 23 then true else false
  | (a, b) ->false

let rec otomeza ls = match ls with
    [] -> []
  | person_t as p::rest -> if is_otomeza (p.tsuki, p.hi) = true then p :: otomeza rest else otomeza rest

let otomeza_person =  otomeza person_list

(* 血液型ごとに人数をカウントする *)
let rec count_blood_type ls = match ls with
    [] -> (0,0,0,0)
  | person_t as p::rest ->
    let (a, b, o, ab) = count_blood_type rest in
    if p.ketsueki = "A" then (a+1, b, o, ab)
    else if p.ketsueki = "B" then (a, b+1, o, ab)
    else if p.ketsueki = "O" then (a, b, o+1, ab)
    else (a, b, o, ab+1) 

let saita_ketsueki lst = 
  let (a, b, o, ab) = count_blood_type lst in 
  let saidai = max (max a b) (max o ab) in 
  if saidai = a then "A" 
  else if saidai = o then "O" 
  else if saidai = b then "B" 
  else "AB" 

let ans = saita_ketsueki person_list

(* 名前のリストを返す高階関数 *)
let get_name person = match person with person_t -> person_t.name

let rec name_list ls f  = match ls with
    []-> []
  | person_t as first::rest -> f first::name_list rest f

let ans = name_list person_list get_name

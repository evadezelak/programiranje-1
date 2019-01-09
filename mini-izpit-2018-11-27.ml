(* -------- 1 -------- *)

let rec vsota seznam = 
  let rec vsota' stevec seznam =
  match seznam with
  | [] -> failwith "List too short"
  | x :: [] -> stevec + x
  | x :: xs -> vsota' (stevec + x) xs
  in vsota' 0 seznam

(* -------- 2 -------- *)

let rec urejen seznam = 
  match seznam with
  | [] -> true
  | x :: [] -> true
  | x :: y :: [] ->
    if x <= y then
      true
    else 
      false
  | x :: y :: xs ->
    if x <= y then
      urejen (y :: xs)
    else 
      false

(* -------- 3 -------- *)

let rec vstavi x seznam = 
  match seznam with
  | [] -> [x]
  | y :: xs ->
    if y <= x then
      y :: vstavi x xs
    else 
      [x] @ (y :: xs)

let rec uredi seznam = 
  match seznam with
  | [] -> []
  | x :: [] -> [x]
  | x :: y :: xs ->
    if x >= y then
      uredi(vstavi x (y :: xs))
    else
      x :: uredi (y :: xs)

(* -------- 4 -------- *)

let rec uredi1 cmp seznam = 
  match seznam with
  | [] -> []
  | x :: [] -> [x]
  | x :: y :: xs ->
    if cmp x y then
      x :: uredi1 cmp (y :: xs)
    else 
      uredi1 cmp (vstavi x (y :: xs))


(* -------- 5 -------- *)

type priority = 
  | Top 
  | Group of int

type status = 
  | Staff 
  | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)

let rec vkrcavanje list =
  let rec vkrcavanje' acc list =
    match list with
    | [] -> acc
    | {status; name} :: passengers ->
      match status with
        | Staff -> vkrcavanje' ({status; name} :: acc) passengers
        | Passenger(_) -> vkrcavanje' ({status; name} :: acc) passengers

  in vkrcavanje' [] list


(* -------- 7 -------- *)

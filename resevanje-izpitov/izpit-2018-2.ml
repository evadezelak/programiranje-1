(*PRVA NALOGA*)

(*primer a*)
let rec uporabi f element = 
  element |> f

(*primer b*)
let rec ibaropu element f =
  element |> f 

(*primer c*)
let rec zacetnih n seznam =
  let rec zacetnih' acc n seznam =
    if n <= 0 then Some (List.rev acc)
    else
      match seznam with
      | [] -> None
      | x :: xs -> zacetnih' (x :: acc) (n-1) xs
  in zacetnih' [] n seznam


(*DRUGA NALOGA*)

type 'a neprazen_sez = 
  | Konec of 'a 
  | Sestavljen of 'a * 'a neprazen_sez 

(*primer a*)
let rec prvi seznam =
  match seznam with
  | [] -> None
  | x :: xs -> Some x

let rec zadnji seznam =
  match seznam with
  | [] -> None
  | x :: [] -> Some x
  | x :: xs -> zadnji xs

(*primer b*)
let rec dolzina seznam =
  let rec dolzina' stevec seznam =
    match seznam with
    | [] -> stevec
    | x :: xs -> dolzina' (stevec + 1) xs
  in dolzina' 0 seznam
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

let test = Sestavljen(5, Sestavljen(4, Sestavljen(6, Konec 7)))

(*primer a*)
let rec prvi seznam =
  match seznam with
  | Konec x 
  | Sestavljen(x, _) -> Some x

let rec zadnji seznam =
  match seznam with
  | Konec x -> x
  | Sestavljen(x, xs) -> zadnji xs

(*primer b*)
let rec dolzina seznam =
  let rec dolzina' stevec seznam =
    match seznam with
    | Konec x -> stevec + 1
    | Sestavljen(x, xs) -> dolzina' (stevec + 1) xs
  in dolzina' 0 seznam

(*primer c*)
let rec pretvori_v_seznam neprazen =
  match neprazen with
  | Konec x -> [x]
  | Sestavljen (x, xs) -> x :: (pretvori_v_seznam xs)

(*primer d*)
let rec zlozi f s neprazen =
  match neprazen with
  | Konec x -> f s x
  | Sestavljen (x, xs) -> zlozi f (f s x) xs
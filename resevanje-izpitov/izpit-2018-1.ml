(*PRVA NALOGA*)

(*primer a*)
let rec izpisi_vsa_stevila seznam =
  match seznam with 
  | [] -> ()
  | x :: xs -> print_int x ; izpisi_vsa_stevila xs

(*primer b*)
let rec map2_opt f seznam1 seznam2 = 
  let rec map' acc f seznam1 seznam2 =
    match seznam1, seznam2 with
    | [], [] -> Some (List.rev acc)
    | [], _ | _, [] -> None
    | x :: xs, y :: ys -> map' ((f x y) :: acc) f xs ys
  in map' [] f seznam1 seznam2


(*DRUGA NALOGA*)

(*primer a*)
type filter_tree =
  | Node of int * filter_tree * filter_tree
  | Leaf of int list

let primer = Node(10, 
  Node(5, Leaf [1], Leaf []), 
  Node(15, Leaf [], Leaf [19; 20])
  )

(*primer b*)
let rec vstavi x drevo =
  match drevo with
  | Node(y, levi, desni) -> 
    if x <= y then Node(y, vstavi x levi, desni)
    else Node(y, levi, vstavi x desni)
  | Leaf(ys) -> Leaf(x :: ys)

(*primer c*)
let rec vstavi_seznam seznam drevo =
  List.fold_right vstavi seznam drevo

(*primer d*)
(*let rec preveri drevo =
  match drevo with
  | Node(y, levi, desni) -> *)
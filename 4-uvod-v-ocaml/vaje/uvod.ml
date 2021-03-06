
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [penultimate_element] vrne predzadnji element danega seznama. V
 primeru prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec penultimate_element list = 
  match list with
  | [] -> failwith "List too short." (* če je seznam prazen, je prekratek*)
  | x :: [] -> failwith "List too short." (*če ima seznam en element, je prekratek*)
  | x :: y :: [] -> x (*seznam ima dva elementa - kul*)
  | x :: y :: ys -> penultimate_element (y :: ys) 

let rec penultimate_element = function (* da bomo meli manj spremenljivk - uporabimo lahko podčrtaje*)
  | [] | _ :: [] -> failwith "List too short." (*če ima seznam en element, je prekratek*)
  | x :: _ :: [] -> x (*seznam ima dva elementa - kul*)
  | _ :: y :: ys -> penultimate_element (y :: ys) 


(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k list = (*tukaj ne moremo uporabiti function, ker matchamo dva argumenta*)
  match k, list with
  | _, [] -> failwith "List too short."
  | k, x :: xs when k <= 0 -> x (* če je k manjši ali enak nič, vrnemo ničti element, sicer gremo naprej rekurzivno*)
  | k, x :: xs -> get (k-1) xs

let rec get k = function (*ugotovimo, da ubistvu ne rabimo biti odvisni od obeh argumentov, zato lahko damo function*)
  | [] -> failwith "List too short."
  | x :: xs when k <= 0 -> x (* če je k manjši ali enak nič, vrnemo ničti element, sicer gremo naprej rekurzivno*)
  | x :: xs -> get (k-1) xs


(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
  | [] -> []
  | x :: [] -> x :: x :: []
  | x :: xs -> x :: x :: double xs 

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k list =
  match k, list with
  | k, list when (k <= 0) -> ([], list)
  | k, [] -> ([], [])
  | k, x :: xs -> 
    let (left_list, right_list) = divide (k-1) xs in
    (x :: left_list, right_list)


(*----------------------------------------------------------------------------*]
 Funkcija [delete k list] iz seznama izbriše [k]-ti element. V primeru
 prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec delete k list =
  match k, list with
  | _, [] -> failwith "List too short."
  | 0, x :: xs -> xs
  | k, x :: xs -> x :: delete (k-1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [slice i k list] sestavi nov seznam, ki vsebuje elemente seznama
 [list] od vključno [i]-tega do izključno [k]-tega. Predpostavimo, da sta [i] in
 [k] primerna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rec slice i k list = 
  match i, k, list with
  | _, _, [] -> []
  | 0, 0, _ -> []
  | 0, _, x :: xs -> x :: slice i (k-1) xs
  | i, k, x :: xs -> slice (i-1) (k-1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k list = 
  match x, k, list with
  | _, _, [] -> failwith "List too short"
  | x, 0, list -> x :: list
  | x, k, list when k < 0 -> x :: list
  | x, k, y :: ys -> y :: insert x (k-1) ys

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list =
  match n, list with
  | 0, list -> list
  | n, [] -> []
  | 1, x :: xs -> xs @ [x]
  | n, x :: xs -> rotate (n-1) (xs@[x])

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x list =
  match x, list with
  | x, [] -> []
  | x, y :: ys when x = y -> remove x ys
  | x, y :: ys -> y :: remove x ys

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec reverse = function
  | [] -> []
  | x :: [] -> [x]
  | x :: xs -> reverse xs @ [x]

let rec is_palindrome list = 
  list = reverse list

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components list1 list2 =
  match list1, list2 with
  | list1, [] -> []
  | [], list2 -> []
  | x :: xs, y :: ys -> (max x y) :: max_on_components xs ys


(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec najvecja seznam = 
  match seznam with
  | [] -> failwith "List too short"
  | x :: [] -> x
  | x :: xs -> max x (najvecja xs)

let rec second_largest = ()

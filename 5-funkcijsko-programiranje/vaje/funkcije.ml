(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec reverse = function
  | [] -> []
  | x :: xs -> reverse xs @ [x]

let rec reverse list =
  let rec reverse' acc list = 
    match list with 
    | [] -> acc
    | x :: xs -> reverse' ([x] @ acc) xs
  in reverse' [] list

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat x n = 
  if n <= 0 then
    []
  else
    x :: repeat x (n - 1)

let rec repeat x n = (*takšne funkcije uporabljamo, da ne pride do stack overflowa*)
  let rec repeat' x n acc =
    if n <= 0 then
      acc
    else
      let new_acc = x :: acc in
      repeat' x (n - 1) new_acc
  in
  repeat' x n []

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let rec range n =
  if n < 0 then
    []
  else
    (range (n - 1)) @ [n]


let rec range n = 
  let rec range' n acc = 
    if n < 0 then 
      acc (*s tem vrnemo vse, kar smo naredili do sedaj, to je akumulator*)
    else
      range' (n - 1) (n :: acc)
  in 
  range' n []


let rec test_bad n acc = (*hočemo, da nam naredi seznam dolžine n, le da bo v tem primeru akumulator dodajal na konec, kar bo počasnejše*)
    if n < 0 then
      []
    else
      test_bad (n - 1) (acc @ [0])

let rec test_good n acc = (*hočemo, da nam naredi seznam dolžine n, tukaj bomo dodajali na začetek*)
    if n < 0 then
      []
    else
      test_good (n - 1) (0 :: acc)

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f(x0); f(x1); f(x2); ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f = function
| [] -> []
| x :: xs -> f x :: map f xs  (*f x pomeni f(x), ker pišemo brez oklepajev*)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map_tlrec f list =
  let rec map_tlrec' acc f list = 
    match list with
    | [] -> acc
    | x :: xs -> map_tlrec' (f x :: acc) f xs
  in
  reverse(map_tlrec' [] f list)
  

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] sprejme seznam in funkcijo dveh argumentov ter vrne seznam
 preslikanih vrednosti seznama, kjer kot drugi argument funkcije podamo indeks
 elementa v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let rec mapi f list =
  let rec mapi' acc stevec f list =
    match list with
    | [] -> acc
    | x :: xs -> mapi' (f x stevec :: acc) (stevec + 1) f xs
  in
  reverse(mapi' [] 0 f list)

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let rec zip list1 list2 =
  let rec zip' acc list1 list2 =
    match list1, list2 with
      | [], [] -> acc
      | _, [] -> failwith "Different lengths of input lists."
      | [], _ -> failwith "Different lengths of input lists."
      | x :: xs, y :: ys -> zip' ((x, y) :: acc) xs ys
  in
  reverse(zip' [] list1 list2)

(*----------------------------------------------------------------------------*]
 Funkcija [zip_enum_tlrec] sprejme seznama [x_0; x_1; ...] in [y_0; y_1; ...]
 ter vrne seznam [(0, x_0, y_0); (1, x_1, y_1); ...]. Funkcija je repno
 rekurzivna. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip_enum_tlrec ["a"; "b"; "c"] [7; 3; 4];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4)]
[*----------------------------------------------------------------------------*)

let rec zip_enum_tlrec list1 list2 = 
  let rec zip_enum_tlrec' acc stevec list1 list2 =
    match list1, list2 with
      | [], [] -> acc
      | _, [] -> failwith "Different lengths of input lists."
      | [], _ -> failwith "Different lengths of input lists."
      | x :: xs, y :: ys -> zip_enum_tlrec' ((stevec, x, y) :: acc) (stevec + 1) xs ys
  in
  reverse(zip_enum_tlrec' [] 0 list1 list2)

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = ()

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec seznam_parov =
  let rec unzip_tlrec' acc1 acc2 seznam_parov = 
    match seznam_parov with
    | [] -> (reverse acc1, reverse acc2)
    | x :: xs -> unzip_tlrec' (fst x :: acc1) (snd x :: acc2) xs
  in
  unzip_tlrec' [] [] seznam_parov

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f list = 
  match list with
  | [] -> failwith "Premalo elementov"
  | x :: [] -> failwith "Premalo elementov"
  | x :: y :: [] -> f x y
  | x :: y :: xs -> fold_left_no_acc f ((f x y) :: xs) 

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let rec apply_sequence f x n =
  let rec apply' acc f x n =
    match f, x, n with
    | f, x, n when n < 1 -> acc
    | f, x, n -> apply' (f x :: acc) f(f x) (n - 1)
  in
  reverse(apply' [] f x n)

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter f list =
  let rec filter' acc f list =
    match list with
    | [] -> acc
    | x :: xs ->
      if f x = true then
        filter' (x :: acc) f xs
      else
        filter' acc f xs
  in filter' [] f list |> reverse


(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists f list =
  let rec exists' acc f list =
    match list with
    | [] -> 
      if List.length acc >= 1 then
        true
      else 
        false
    | x :: xs ->
      if f x = true then
        exists' (x :: acc) f xs
      else
        exists' acc f xs
  in exists' [] f list

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first f default list = 
  let rec first' acc f default list =
    match list with
    | [] -> default
    | x :: xs ->
      if f x = true then
        x
      else 
        first' acc f default xs
  in first' [] f default list
(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = 
    | Empty
    | Node of 'a tree * 'a * 'a tree

(*type 'a tree = 
    |Empty
    | Leaf 'a
    | Node of 'a tree = 'a * 'a tree

Leaf x = Node (Empty, x, Empty)
*)

let leaf x = Node(Empty, x, Empty)

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let test_tree =
    let left_tree = Node(leaf 0, 2, Empty) in
    let right_tree = Node( leaf 6, 7, leaf 11) in
    Node(left_tree, 5, right_tree)

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror tree = 
    match tree with 
    | Empty -> Empty
    | Node(levi, x, desni) -> Node(mirror desni, x, mirror levi)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec size = function
    | Empty -> 0
    | Node(levi, x, desni) -> 1 + size levi + size desni

let tl_rec_size tree =
    let rec size' acc queue = (*sedaj se drevo deli na dva dela, zato moramo imeti še neko vrsto, ki si bo zapomnila te stvari*)
        (*pogledamo kateri je naslednji element v vrsti za obravnavo*)
        match queue with
        | [] -> acc
        | t :: ts -> (
        (*obravnavamo drevo*)
            match t with
            | Empty -> size' acc ts (*prazno drevo samo odstranimo iz vrste*)
            | Node(levi, x, desni) -> (*size' (acc + 1) (levi :: desni :: ts) to bi bilo na krajsi nacin*)
                let new_acc = acc + 1 in (*obravnavamo vozlišče*)
                let new_queue = levi :: desni :: ts in (*dodamo poddrevesa v vrsto*)
                size' new_acc new_queue
        )
    in
    (*zaženemo pomožno funkaciji*)
    size' 0 [tree]

let rec height tree = 
    match tree with
    | Empty -> 0
    | Node(levi, x, desni) -> 1 + max (height levi) (height desni)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f tree = 
    match tree with
    | Empty -> Empty
    | Node(levi, x, desni) -> Node(map_tree f levi, f x, map_tree f desni)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree tree = 
    let rec list' acc tree =
        match tree with
        | Empty -> acc
        | Node(levi, x, desni) -> list' (x :: acc) levi @ list' [] desni
    in list' [] tree

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)
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

let rec is_bst tree = urejen (list_of_tree tree) 


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec member x tree =
    let rec member' x  = function
        | [] -> false
        | y :: ys -> 
            if y = x then
                true
            else
                member' x ys
    in member' x (list_of_tree tree)

let rec insert n tree =
    match tree with
    | Empty -> Node(Empty, n, Empty)
    | Node(levi, x, desni) ->
        if n < x then
            let nov_levi = insert n levi in
            Node(nov_levi, x, desni)
        else
            let nov_desni = insert n desni in
            Node(levi, x, nov_desni)
      

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 x tree =
    let rec member' x  = function
        | [] -> false
        | y :: ys -> 
            if y = x then
                true
            else
                member' x ys
    in member' x (list_of_tree tree)

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let rec minimum tree = 
    let rec minimum' = function
    | [] -> failwith "Prazno drevo"
    | x :: [] -> x
    | x :: xs -> min x (minimum' xs)
    in
    minimum' (list_of_tree tree)

let rec lepi_minimum tree = 
    match tree with 
    | Empty -> None
    | Node(Empty, x, desni) -> Some x
    | Node(levi, x, desni) -> lepi_minimum levi


let rec succ tree = 
    match tree with
    | Empty -> None
    | Node(levi, x, Empty) -> None
    | Node(levi, x, desni) -> lepi_minimum desni


let rec lepi_maksimum tree = 
    match tree with
    | Empty -> None
    | Node(levi, x, Empty) -> Some x
    | Node(levi, x, desni) -> lepi_maksimum desni

let rec pred tree = 
    match tree with 
    | Empty -> None
    | Node(Empty, x, desni) -> None
    | Node(levi, x, desni) -> lepi_maksimum levi





(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete x tree = 
    match tree with
    | Empty -> Empty (*prazno drevo*)
    | Node(Empty, y, Empty) when x = y -> (* leaf case*) Empty (*odstraanimo list, če je enak x*)
    | Node(Empty, y, desni) when x = y ->(*one sided*) desni
    | Node(levi, y, Empty) when x = y ->(*one sided*) levi
    | Node(levi, y, desni) when x <> y -> (*recurse deeper*)
        if x > y then
            Node(levi, y, delete x desni)
        else
            Node(delete x levi, y, desni)
    | Node(levi, y, desni) -> (*super fun case :D*)
        match succ tree with (*poiščemo succesorja*)
        | None -> failwith "Kako je to mogoče?!" (*vemo da se to sploh ne bo moglo zgoditi, ker smo none primer obravnavali že zgoraj*)
        | Some z -> Node(levi, z, delete z desni) (*v desno drevo  gremo posikati succesorja in ga prenesemo gor*)



(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict = 
    let left_tree = Node(Empty, ("a", 0), Empty) in
    let right_tree = Node(leaf ("c",-2), ("d", 2), Empty) in
    Node(left_tree, ("b", 1) , right_tree)

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key dict = 
    match dict with
    | Empty -> None
    | Node(levi, (k, v), desni) when k = key -> Some v
    | Node(levi, (k, v), desni) ->
        if k > key then
            dict_get key levi
        else
            dict_get key desni

      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_dict = function
    | Empty -> ()
    | Node(ld, (k,v), rd) -> 
        (print_dict ld);
        (print_string(k));
        (print_string(":"));
        (print_int(v));
        (print_string("\n"));
        (print_dict rd)

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert kljuc vrednost slovar = 
    match slovar with
    | Empty ->  leaf(kljuc, vrednost)
    | Node(levi, (k, v), desni) ->
        if k = kljuc then Node(levi, (kljuc, vrednost), desni)
        else if k < kljuc then
            Node(levi, (k, v), dict_insert kljuc vrednost desni)
        else
            Node(dict_insert kljuc vrednost levi, (k, v), desni)
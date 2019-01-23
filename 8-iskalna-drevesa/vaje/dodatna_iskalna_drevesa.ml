(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DODATNE VAJE 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node(Empty, x, Empty)

(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)

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

let rec list_of_tree tree = 
  let rec list' acc tree =
    match tree with
      | Empty -> acc
      | Node(levi, x, desni) -> list' (x :: acc) levi @ list' [] desni
  in list' [] tree

let rec is_bst tree = urejen (list_of_tree tree)

let bst_of_list seznam = List.fold_right insert seznam Empty

(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst in nato nazaj
 v seznam.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let rec tree_sort seznam = seznam |> bst_of_list |> list_of_tree

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip. Ne pozabite definirati tipa [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type directions =
  | Left
  | Right

let test_tree =
  let left_tree = Node(leaf 0, 2, Empty) in
  let right_tree = Node( leaf 6, 7, leaf 11) in
  Node(left_tree, 5, right_tree)

let rec follow directions tree =
  match directions, tree with
  | _, Empty -> None
  | [], Node(_, x, _) -> Some(x)
  | x :: xs, Node(levo, _, Empty) -> 
    if x = Right then None else follow xs levo
  | x :: xs, Node(Empty, _, desno) ->
    if x = Left then None else follow xs desno
  | x :: xs, Node(levo, _, desno) -> 
    if x = Right then follow xs desno else follow xs levo

(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu glede na navodila,
 ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume 
 kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec prune directions tree =
  match directions, tree with
  | [], _ -> Some Empty
  | _, Empty -> None
  | Left :: tl, Node(l, x, r) ->
    (match prune tl l with
     | None -> None
     | Some new_l -> Some (Node(new_l, x, r)))
  | Right :: tl, Node(l, x, r) ->
    (match prune tl r with
     |None -> None
| Some new_l -> Some (Node(new_l, x, r)))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče. Še vedno
 predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)

type state = Exists | Ghost

type 'a phantom_tree =
  | P_Empty
  | P_Node of 'a phantom_tree * 'a * 'a phantom_tree * state

let rec phantomize tree =
  match tree with
  | Empty -> P_Empty
  | Node(levo, x, desno) ->
    let p_levo = phantomize levo in
    let p_desno = phantomize desno in
    P_Node(p_levo, x, p_desno, Exists)

let rec kill x ptree =
  match ptree with
  | P_Empty -> P_Empty
  | P_Node(levo, y, desno, Exists) ->
    if x = y then P_Node(levo, y, desno, Ghost) 
    else if y > x then P_Node(kill x levo, y, desno, Exists)
    else P_Node(levo, y, kill x desno, Exists)
  | P_Node(levo, y, desno, Ghost) ->
    if y > x then P_Node(kill x levo, y, desno, Exists)
    else P_Node(levo, y, kill x desno, Exists)


(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let rec list_of_ptree ptree =
  let rec list' acc ptree = 
    match ptree with
      | P_Empty -> acc
      | P_Node(levi, x, desni, Exists) -> list' (x :: acc) levi @ list' [] desni
      | P_Node(levi, _, desni, Ghost) -> (list' acc levi) @ (list' [] desni)
  in list' [] ptree

let rec unphantomize ptree = ptree |> list_of_ptree |> bst_of_list
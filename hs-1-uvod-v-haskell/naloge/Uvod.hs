{-
 - Vaja 1: Uvod v Haskell
 -}

-- Funkcija predzadnjiElement vrne predzadnji element seznama l.
predzadnjiElement l = undefined

-- Funkcija poisci naj poišče k-ti element v seznamu l.
-- Zgled:
-- ghci> poisci 2 [0,0,1,0,0,0]
-- 1
poisci k l = undefined

-- Funkcija podvoji naj 'podvoji' seznam l.
-- Zgled:
-- ghci> podvoji [1,2,3,3]
-- [1,1,2,2,3,3,3,3]
-- Namig: Funkcija concat iz seznama seznamov naredil seznam z elementi podseznamov.
podvoji l = undefined

-- Funkcija razdeli k l naj seznam l razdeli na dva seznama. V prvem naj bo prvi k elementov
-- seznama l, v drugem pa vsi ostali. Funkcija naj vrne par teh dveh seznamov.
-- Zgled:
-- ghci> razdeli 2 [1,1,1,2,2,2]
-- ([1,1],[1,2,2,2])
razdeli k l = undefined

-- Funkcija zbrisi naj iz seznama l pobriše k-ti element.
-- Zgled:
-- ghci> zbrisi 3 [0,0,0,1,0,0,0]
-- [0,0,0,0,0,0]
zbrisi k l = undefined

-- Funkcija rezina i k l naj sestavi novi seznam, ki naj vsebuje elemente seznama l od vključno
-- i-tega do -- k-tega (brez k-tega).
-- Zgled:
-- ghci> rezina 3 6 [0,0,0,1,2,3,0,0,0]
-- [1,2,3]
rezina i k l = undefined

-- Napišite funkcijo "vstavi x k l", ki na k-to mesto seznama l vrine x.
-- Na primer:
-- ghci> vstavi 2 5 [0,0,0,0,0,0]
-- [0,0,0,0,0,2,0]
vstavi x k l = undefined

-- Napišite funkcijo "zavrti n l", ki seznam l zavrti za n mest v levo.
-- Na primer:
-- ghci> zavrti 2 [1,2,3,4,5]
-- [3,4,5,1,2]
zavrti n l = undefined

-- Napišite funkcijo "pobrisi x l", ki vrne seznam, ki je kot l,
-- le da smo iz njega pobrisali vse pojavitve elementa x
-- Na primer:
-- ghci> pobrisi 'a' "abrakadabra"
-- "brkdbr"
pobrisi x l = undefined

-- Funkcija jePalindrom naj ugotovi, če je seznam l palindrom.
-- Zgled:
-- ghci> jePalindrom [1,2,3,2,1]
-- True
-- ghci> jePalindrom [1,2,2,1]
-- True
-- ghci> jePalindrom [1,2,3]
-- False
jePalindrom l = undefined

-- Napišite funkcijo "maxPoKomponentah l1 l2", ki vrne seznam, ki ima za
-- elemente večjega od elementov na ustreznih mestih v seznamih l1 in l2.
-- Če je eden od seznamov krajši, naj bo krajši tudi vrnjeni seznam.
-- Na primer:
-- ghci> maxPoKomponentah [1,10,5,6] [2,3,7,4,8]
-- [2,10,7,6]
maxPoKomponentah l1 l2 = undefined

-- Napišite funkcijo "drugiNajvecji l", ki vrne drugi največji element
-- seznama l. Predpostavite lahko, da sta v l vsaj dva različna elementa.
-- Na primer:
-- ghci> drugiNajvecji [1,10,5,6]
-- 6
drugiNajvecji l = undefined

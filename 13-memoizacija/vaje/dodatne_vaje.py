from functools import lru_cache

###############################################################################
# Napisite funkcijo [najdaljse_narascajoce_podazporedje], ki sprejme seznam in
# poisce najdaljse (ne strogo) narascajoce podzaporedje stevil v seznamu.
#
# Na primer: V seznamu [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9] je najdaljse naj vrne
# rezultat [2, 3, 4, 4, 6, 7, 8, 9].
###############################################################################

testen_seznam = [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]

def najdaljse_narascajoce_podzaporedje(sez, min_element=-1, i=0):
        if len(sez) == 0:
                return []
        elif len(sez) == 1:
                return sez
        if(i >= len(sez)):
                return []
        if(sez[i] < min_element):
                return najdaljse_narascajoce_podzaporedje(sez, min_element, i+1)

        brez_prvega = najdaljse_narascajoce_podzaporedje(sez, min_element, i+1)
        s_prvim = [sez[i]] + najdaljse_narascajoce_podzaporedje(sez, sez[i], i+1)
        
        if len(brez_prvega) > len(s_prvim): 
                return brez_prvega
        else:
                return s_prvim



            
###############################################################################
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati

# Robotek se lahko premika le gor, dol, levo in desno, ter ima omejeno količino
# goriva. Napišite funkcijo [pobeg], ki sprejme matriko, ki predstavlja sobo,
# začetno pozicijo in pa število korakov, ki jih robotek lahko naredi z
# gorivom, in izračuna ali lahko robotek pobegne. Soba ima vedno vsaj eno
# polje.
#
# Na primer za laboratorij:
# [[0, 1, 0, 0, 2],
#  [0, 2, 2, 0, 0],
#  [0, 0, 2, 2, 0],
#  [2, 0, 0, 2, 0],
#  [0, 2, 2, 0, 0],
#  [0, 0, 0, 2, 2]]
#
# robotek iz pozicije (3, 1) pobegne čim ima vsaj 5 korakov, iz pozicije (5, 0)
# pa v nobenem primeru ne more, saj je zagrajen.
###############################################################################

soba = [[0, 1, 0, 0, 2],
        [0, 2, 2, 0, 0],
        [0, 0, 2, 2, 0],
        [2, 0, 0, 2, 0],
        [0, 2, 2, 0, 0],
        [0, 0, 0, 2, 2]]


def pobeg(soba, pozicija, koraki):
        max_vrstica = len(soba) - 1
        max_stolpec = len(soba[0]) - 1
        
        # @lru_cache(maxsize=None)
        def pobegni(pozicija, koraki):
                if pozicija[0] > max_vrstica or pozicija[0] < 0:
                        return False
                elif pozicija[1] > max_stolpec or pozicija[1] < 0:
                        return False
                if koraki < 0:
                        return False

                if soba[pozicija[0]][pozicija[1]] == 1:
                        return True

                if soba[pozicija[0]+1][pozicija[1]] == 0:
                        nova_pozicija = (pozicija[0] + 1, pozicija[1])
                        pobeg(soba, nova_pozicija, koraki -1)

                if soba[pozicija[0]][pozicija[1]+1] == 0:
                        nova_pozicija = (pozicija[0], pozicija[1]+1)
                        pobeg(soba, nova_pozicija, koraki -1)
                
                if soba[pozicija[0]-1][pozicija[1]] == 0:
                        nova_pozicija = (pozicija[0]-1, pozicija[1])
                        pobeg(soba, nova_pozicija, koraki -1)

                if soba[pozicija[0]][pozicija[1]-1] == 0:
                        nova_pozicija = (pozicija[0], pozicija[1]-1)
                        pobeg(soba, nova_pozicija, koraki -1)
        return pobegni(pozicija,koraki)



        

        


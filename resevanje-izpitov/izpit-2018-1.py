#ČETRTA NALOGA

sadovnjak = [[2,4,1,1], [3,2,0,5], [8,0,7,2]]

def lisjacek(sadovnjak, stevilo):
    def pomozna(i, j, stevilo):
        if stevilo == 0:
            return 0
        if i == len(sadovnjak)-1 :
            if j == len(sadovnjak[0])-1:
                return sadovnjak[i][j]
            else:
                return sadovnjak[i][j] + pomozna(i, j+1, stevilo-1)
        else:
            if j == len(sadovnjak[0])-1:
                return sadovnjak[i][j] + pomozna(i+1, 0, stevilo-1)
            else:
                desno = pomozna(i, j + 1, stevilo-1)
                nova_vrsta = pomozna(i+1, 0, stevilo-1)
                return sadovnjak[i][j] + max(desno, nova_vrsta)
    return pomozna(0, 0, stevilo)


#lisjacek(sadovnjak, 6)
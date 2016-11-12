dane <- read.csv("data.csv", fileEncoding = "UTF-8")
dane = dane[,2:229]
#write.table(names(dane), "dane.csv", sep=",")

praca = as.numeric(dane[,6])
praca = cbind(praca, as.numeric(dane[,7]))
praca = cbind(praca, as.numeric(dane[,8]))
praca = cbind(praca, as.numeric(dane[,9]))
praca = cbind(praca, as.numeric(dane[,10]))

zatrudnienie <- rep(0, dim(dane)[1])
zatrudnienie = replace(zatrudnienie, praca[,3]==2, 0.5)
zatrudnienie = replace(zatrudnienie, praca[,2]==2, 0.5)
zatrudnienie = replace(zatrudnienie, praca[,1]==2, 1)

zakupy = as.numeric(dane[,13])
zakupy = cbind(zakupy, as.numeric(dane[,14]))
zakupy = cbind(zakupy, as.numeric(dane[,15]))

kupowanie <- rep(0, dim(dane)[1])
kupowanie = replace(kupowanie, zakupy[,3]==1, 0.5)
kupowanie = replace(kupowanie, zakupy[,1]==2, 1)

posilki = as.numeric(dane[,16])
posilki = cbind(posilki, as.numeric(dane[,17]))
posilki = cbind(posilki, as.numeric(dane[,18]))

przygotowanie <- rep(0, dim(dane)[1])
przygotowanie = replace(przygotowanie, posilki[,3]==1, 0.5)
przygotowanie = replace(przygotowanie, posilki[,1]==2, 1)

czestotliwosc <- matrix(c("2-3 razy dziennie", 2.5,
                  "1 raz dziennie", 1,
                  "1 w miesiącu", 1/30,
                  "1 w tygodniu", 4/30,
                  "2-3 razy w miesiącu", 2.5/30,
                  "2-3 razy w tygodniu", 4*2.5/30,
                  "4-5 razy w tygodniu", 4*4.5/30,
                  "6 razy w tygodniu", 4*6/30,
                  "Nigdy", 0,
                  "Rzadziej niż 1 raz w miesiącu", 1/90), ncol=2, byrow = T)
czestotliwosc = as.data.frame(czestotliwosc)

# przelicz("2-3 razy dziennie", 2.5)
# przelicz("1 raz dziennie", 1)
# przelicz("1 w miesiącu", 1/30)
# przelicz("1 w tygodniu", 4/30)
# przelicz("2-3 razy w miesiącu", 2.5/30)
# przelicz("2-3 razy w tygodniu", 4*2.5/30)
# przelicz("4-5 razy w tygodniu", 4*4.5/30)
# przelicz("6 razy w tygodniu", 4*6/30)
# przelicz("Nigdy", 0)
# przelicz("Rzadziej niż 1 raz w miesiącu", 1/90)
# przelicz("Jedna kulka puree", 90)
# przelicz("Jeden cały ziemniak", 85)
# przelicz("1 cały", 170)
# przelicz("1 ćwiartka", 170/4)
# przelicz("1 plaster", 20)
# przelicz("1 połowa", 170/2)
# przelicz("1 pomidor koktajlowy", 20)
# przelicz("świderki", 170/2)
# przelicz("muszelki", 170/2)
# przelicz("spaghetti", 170/2)
# przelicz("kokardki", 170/2)
# przelicz("rurki", 170/2)
# przelicz("wstążki", 170/2)
# przelicz("nitki", 170/2)
# przelicz("Kukurydziane", 30)
# przelicz("Miodowe", 30)
# przelicz("Czekoladowe", 40)
# przelicz("Cynamonowe", 30)
# przelicz("Owsiane", 50)
# przelicz("Muesli", 50)
# przelicz("Granola", 60)
# przelicz("Nie spożywam", 0)
# przelicz("Nie kupuję", 0)

#spozycie = dane[,68]*dane[,69]*dane[,70]
#spozycie = cbind(spozycie, dane[,98]*dane[,99]*dane[,100])
#spozycie = cbind(spozycie, 180*dane[,125])
#spozycie = cbind(spozycie, 240*dane[,147])
#spozycie = cbind(spozycie, as.vector(apply(dane[,169:176], 1, mean)))
#spozycie = cbind(spozycie, as.vector(apply(dane[,191:198], 1, mean)))
#spozycie = cbind(spozycie, as.vector(apply(dane[,214:221], 1, mean)))
#dane = data.frame(dane[,1:5],
#                  zatrudnienie,
#                  dane[,11:12],
#                  kupowanie,
#                  przygotowanie,
#                  dane[,c(19:21, 23:67)],
#                  spozycie[,1],
#                  dane[,c(71:88, 90:97)],
#                  spozycie[,2],
#                  dane[,c(101:115, 117:124)],
#                  spozycie[,3],
#                  dane[,c(126:137, 139:146)],
#                  spozycie[,4],
#                  dane[,c(148:157, 159:165)],
#                  spozycie[,5],
#                  dane[,c(167:168, 177:188, 190)],
#                  spozycie[,6],
#                  dane[,c(199:211, 213)],
#                  spozycie[,7],
#                  dane[,c(222:228)])
#names(dane)[1:length(dane)]
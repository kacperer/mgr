source("przelicz.R", encoding = "UTF-8")
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

for(i in c(68,70,98,100,125,147,169:176,191:198,214:221))
  dane[,i] <- przelicz(i)

spozycie = dane[,68]*dane[,69]*dane[,70]
spozycie = cbind(spozycie, dane[,98]*dane[,99]*dane[,100])
spozycie = cbind(spozycie, 180*dane[,125])
spozycie = cbind(spozycie, 240*dane[,147])
spozycie = cbind(spozycie, as.vector(apply(dane[,169:176], 1, mean)))
spozycie = cbind(spozycie, as.vector(apply(dane[,191:198], 1, mean)))
spozycie = cbind(spozycie, as.vector(apply(dane[,214:221], 1, mean)))

dane = data.frame(dane[,1:5],
                 zatrudnienie,
                 dane[,11:12],
                 kupowanie,
                 przygotowanie,
                 dane[,c(19:21, 23:67)],
                 spozycie[,1],
                 dane[,c(71:88, 90:97)],
                 spozycie[,2],
                 dane[,c(101:115, 117:124)],
                 spozycie[,3],
                 dane[,c(126:137, 139:146)],
                 spozycie[,4],
                 dane[,c(148:157, 159:165)],
                 spozycie[,5],
                 dane[,c(167:168, 177:188, 190)],
                 spozycie[,6],
                 dane[,c(199:211, 213)],
                 spozycie[,7],
                 dane[,c(222:228)])
#names(dane)[1:length(dane)]
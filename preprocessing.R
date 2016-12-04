# Wczytanie danych i funkcji
source("przelicz.R", encoding = "UTF-8")
source("przeskaluj.R", encoding = "UTF-8")
source("normalizuj.R", encoding = "UTF-8")
dane <- read.csv("odpowiedzi.csv", sep=",", fileEncoding = "UTF-8")
dane = dane[,2:229]

# Ograniczenie grupy badanej do osób do 30 roku życia
dane[,2][dane[,2] > 30] <- NA
for(i in 1:dim(dane)[2]){
  dane[,i][is.na(dane[,2])] <- NA
  dane[,i]
}
dane = na.omit(dane)

# Przeliczenie danych na wartości liczbowe
# - konsolidacja i normalizacja wartości kolumn z odpowiedziami dotyczącymi zatrudnienia
praca = as.numeric(dane[,6])
praca = cbind(praca, as.numeric(dane[,7]))
praca = cbind(praca, as.numeric(dane[,8]))
praca = cbind(praca, as.numeric(dane[,9]))
praca = cbind(praca, as.numeric(dane[,10]))

zatrudnienie <- rep(0, dim(dane)[1])
zatrudnienie = replace(zatrudnienie, praca[,3]==2, 0.5)
zatrudnienie = replace(zatrudnienie, praca[,2]==2, 0.5)
zatrudnienie = replace(zatrudnienie, praca[,1]==2, 1)

# - konsolidacja i normalizacja wartości kolumn z odpowiedziami dotyczącymi sposobu dokonywania zakupów
zakupy = as.numeric(dane[,13])
zakupy = cbind(zakupy, as.numeric(dane[,14]))
zakupy = cbind(zakupy, as.numeric(dane[,15]))

kupowanie <- rep(0, dim(dane)[1])
kupowanie = replace(kupowanie, zakupy[,3]==1, 0.5)
kupowanie = replace(kupowanie, zakupy[,1]==2, 1)

# - konsolidacja i normalizacja wartości kolumn z odpowiedziami dotyczącymi sposobu przygotowania posiłków
posilki = as.numeric(dane[,16])
posilki = cbind(posilki, as.numeric(dane[,17]))
posilki = cbind(posilki, as.numeric(dane[,18]))

przygotowanie <- rep(0, dim(dane)[1])
przygotowanie = replace(przygotowanie, posilki[,3]==1, 0.5)
przygotowanie = replace(przygotowanie, posilki[,1]==2, 1)

# - zamiana danych tekstowych na liczbowe wg wartości przypisanych w pliku przelicz.R
for(i in c(1,3:5,19,21,68,70:76,83:88,98,100:102,108:115,125:127,133:137,147:149,155:157,166:176,189:198,212:221,223))
  dane[,i] <- przelicz(i)

# - zamiana pustych komórek na wartości liczbowe w pytaniu o podejmowanie nauki i stosowanie diety
dane[,5][is.na(dane[,5])] <- 0
dane[,21][is.na(dane[,21])] <- 1

# - obiczenie spożycia surowców i produktów na podstawie udzielonych odpowiedzi
# -- ziemniaki (wielkość/typ porcji, ilość porcji, częstotliwość)
spozycie = dane[,68]*dane[,69]*dane[,70]
# -- pomidory (wielkość/typ porcji, ilość porcji, częstotliwość)
spozycie = cbind(spozycie, dane[,98]*dane[,99]*dane[,100])
# -- jabłka (standardowa porcja 180g, częstotliwość), zakładana jedna porcja
spozycie = cbind(spozycie, dane[,125]*180)
# -- pomarańcze (standardowa porcja 240g, częstotliwość), zakładana jedna porcja
spozycie = cbind(spozycie, dane[,147]*240)
# -- makarony (średnia standardowych porcji różnego typu produktów, częstotliwość), zakładana jedna porcja
spozycie = cbind(spozycie, dane[,166]*as.vector(apply(dane[,169:176], 1, mean, na.rm = T)))
# -- płatki śniadaniowe (średnia standardowych porcji różnego typu produktów, częstotliwość), zakładana jedna porcja
spozycie = cbind(spozycie, dane[,189]*as.vector(apply(dane[,191:198], 1, mean, na.rm = T)))
# -- pieczywo (średnia standardowych porcji różnego typu produktów, częstotliwość, ilość porcji)
spozycie = cbind(spozycie, dane[,212]*dane[,222]*as.vector(apply(dane[,214:221], 1, mean, na.rm = T)))
spozycie[is.nan(spozycie)] <- 0

# - konsolidacja wartości kolumn z odpowiedziami wielokrotnego wyboru
opakowanie = as.vector(apply(dane[,73:76], 1, mean, na.rm = T))
opakowanie = cbind(opakowanie, dane[,223])
opakowanie[is.nan(opakowanie)] <- NA

dane[,c(83:88,108:115,133:137,155:157)][is.na(dane[,c(83:88,108:115,133:137,155:157)])] <- 0
dane[,89] = as.numeric(dane[,89])
dane[,116] = as.numeric(dane[,116])
dane[,138] = as.numeric(dane[,138])
dane[,158] = as.numeric(dane[,158])
dane[,c(89,116,138,158)][dane[,c(89,116,138,158)]==1] <- 0
dane[,c(89,116,138,158)][dane[,c(89,116,138,158)]>1] <- 1

produkty = as.vector(apply(dane[,83:89], 1, mean))
produkty = cbind(produkty, as.vector(apply(dane[,108:116], 1, mean, na.rm = T)))
produkty = cbind(produkty, as.vector(apply(dane[,133:138], 1, mean, na.rm = T)))
produkty = cbind(produkty, as.vector(apply(dane[,155:158], 1, mean, na.rm = T)))

# Scalanie przeliczonych danych w jedną bazę
dane = data.frame(dane[,1:5],
                 zatrudnienie,
                 dane[,11:12],
                 kupowanie,
                 przygotowanie,
                 dane[,c(19:21, 23:67)],
                 spozycie[,1],
                 dane[,c(71:72)],
                 opakowanie[,1],
                 dane[,c(78:82)],
                 produkty[,1],
                 dane[,c(90:97)],
                 spozycie[,2],
                 dane[,c(101:107)],
                 produkty[,2],
                 dane[,c(117:124)],
                 spozycie[,3],
                 dane[,c(126:132)],
                 produkty[,3],
                 dane[,c(139:146)],
                 spozycie[,4],
                 dane[,c(148:154)],
                 produkty[,4],
                 dane[,c(159:165)],
                 spozycie[,5],
                 dane[,c(167:168, 177:188, 190)],
                 spozycie[,6],
                 dane[,c(199:211, 213)],
                 spozycie[,7],
                 opakowanie[,2],
                 dane[,c(224:228)])
pytania <- names(dane)

#Eksport treści pytań do pliku Excel
#write.table(names(dane), "nazwy.csv", sep=";", fileEncoding = "windows-1250")

#Nadanie nazw kolumnom
names(dane) = c("metryczka.plec",
                "metryczka.wiek",
                "metryczka.miejsce",
                "metryczka.wyksztalcenie",
                "metryczka.nauka",
                "metryczka.praca",
                "metryczka.osoby",
                "metryczka.dzieci",
                "metryczka.zakupy",
                "metryczka.przygotowanie",
                "metryczka.sytuacja",
                "metryczka.wiedza",
                "metryczka.dieta",
                "fcq.dieta_kalorie",
                "fcq.cena_wartosc",
                "fcq.eko_opakowana",
                "fcq.eko_produkowana",
                "fcq.dieta_tluszcz",
                "fcq.pol_pochodzenie",
                "fcq.eko_bezgmo",
                "fcq.dieta_wit.i.min",
                "fcq.dieta_bezdodatkow",
                "fcq.dieta_odzywcza",
                "fcq.dieta_bialko",
                "fcq.dieta_naturalna",
                "fcq.hed_zdziecinstwa",
                "fcq.hed_odprezajaca",
                "fcq.hed_wygodna",
                "frl.nowe_zawartosc",
                "frl.dieta_odzywcza",
                "frl.cena_info",
                "frl.nowe_etykiety",
                "frl.dieta_naturalna",
                "frl.dieta_bezdodatkow",
                "frl.dieta_wartosc",
                "fns.nowe_ufnosc",
                "fns.nowe_przyjecia",
                "cet.pol_bezrobocie",
                "cet.pol_niedostepne",
                "cet.pol_nieprodukowane",
                "mgr.nowe_marki",
                "mgr.hed_przepis",
                "mgr.hed_przyjemnosc",
                "mgr.hed_niesmaczna",
                "mgr.pol_swieza",
                "mgr.pol_tania",
                "mgr.pol_smaczna",
                "mgr.nowe_sklad",
                "mgr.hed_wrazenie",
                "mgr.hed_zakupy",
                "mgr.hed_szybko",
                "ziemniak.cena",
                "ziemniak.miejsce",
                "ziemniak.swiezosc",
                "ziemniak.rodzaj",
                "ziemniak.kraj",
                "ziemniak.barwa",
                "ziemniak.wyglad",
                "ziemniak.spozycie",
                "ziemniak.okres",
                "ziemniak.sklep",
                "ziemniak.opakowanie",
                "ziemniak.obrobka",
                "ziemniak.potrawy",
                "ziemniak.zdrowotnosc",
                "ziemniak.smak",
                "ziemniak.jakosc",
                "ziemniak.produkty",
                "pomidor.cena",
                "pomidor.miejsce",
                "pomidor.swiezosc",
                "pomidor.rodzaj",
                "pomidor.kraj",
                "pomidor.barwa",
                "pomidor.kwasnosc",
                "pomidor.miekkosc",
                "pomidor.spozycie",
                "pomidor.okres",
                "pomidor.sklep",
                "pomidor.obrobka",
                "pomidor.potrawy",
                "pomidor.zdrowotnosc",
                "pomidor.smak",
                "pomidor.jakosc",
                "pomidor.produkty",
                "jablko.cena",
                "jablko.miejsce",
                "jablko.swiezosc",
                "jablko.rodzaj",
                "jablko.kraj",
                "jablko.barwa",
                "jablko.miekkosc",
                "jablko.kwasnosc",
                "jablko.spozycie",
                "jablko.okres",
                "jablko.sklep",
                "jablko.obrobka",
                "jablko.potrawy",
                "jablko.zdrowotnosc",
                "jablko.smak",
                "jablko.jakosc",
                "jablko.produkty",
                "pomarancza.cena",
                "pomarancza.miejsce",
                "pomarancza.swiezosc",
                "pomarancza.rodzaj",
                "pomarancza.kraj",
                "pomarancza.barwa",
                "pomarancza.miekkosc",
                "pomarancza.kwasnosc",
                "pomarancza.spozycie",
                "pomarancza.okres",
                "pomarancza.sklep",
                "pomarancza.obrobka",
                "pomarancza.potrawy",
                "pomarancza.zdrowotnosc",
                "pomarancza.smak",
                "pomarancza.jakosc",
                "pomarancza.produkty",
                "makaron.cena",
                "makaron.miejsce",
                "makaron.swiezosc",
                "makaron.rodzaj",
                "makaron.kraj",
                "makaron.barwa",
                "makaron.marka",
                "makaron.spozycie",
                "makaron.okres",
                "makaron.sklep",
                "makaron.obrobka",
                "makaron.potrawy",
                "makaron.zdrowotnosc",
                "makaron.smak",
                "makaron.jakosc",
                "platki.cena",
                "platki.miejsce",
                "platki.swiezosc",
                "platki.rodzaj",
                "platki.kraj",
                "platki.blonnik",
                "platki.marka",
                "platki.sklep",
                "platki.spozycie",
                "platki.obrobka",
                "platki.potrawy",
                "platki.zdrowotnosc",
                "platki.smak",
                "platki.jakosc",
                "pieczywo.cena",
                "pieczywo.miejsce",
                "pieczywo.swiezosc",
                "pieczywo.rodzaj",
                "pieczywo.kraj",
                "pieczywo.barwa",
                "pieczywo.ziarna",
                "pieczywo.blonnik",
                "pieczywo.sklep",
                "pieczywo.spozycie",
                "pieczywo.opakowanie",
                "pieczywo.obrobka",
                "pieczywo.potrawy",
                "pieczywo.zdrowotnosc",
                "pieczywo.smak",
                "pieczywo.jakosc")

# Wykluczenie odpowiedzi dotyczących atrybutów jakości produktów,
# w przypadku gdy deklarowano jednocześnie brak spożycia lub niedokonywanie zakupów danego produktu
for(i in c(52:58,61,62)){
  dane[,i][dane[,59] == 0] <- NA
  dane[,i][is.na(dane[,61])] <- NA
  dane[,i][is.na(dane[,62])] <- NA
  dane[,i]
}
for(i in c(69:76,79)){
  dane[,i][dane[,77] == 0] <- NA
  dane[,i][is.na(dane[,79])] <- NA
  dane[,i]
}
for(i in c(86:93,96)){
  dane[,i][dane[,94] == 0] <- NA
  dane[,i][is.na(dane[,96])] <- NA
  dane[,i]
}
for(i in c(103:110,113)){
  dane[,i][dane[,111] == 0] <- NA
  dane[,i][is.na(dane[,113])] <- NA
  dane[,i]
}
for(i in c(120:126,129)){
  dane[,i][dane[,127] == 0] <- NA
  dane[,i][is.na(dane[,129])] <- NA
  dane[,i]
}
for(i in 135:142){
  dane[,i][is.na(dane[,142])] <- NA
  dane[,i][dane[,143] == 0] <- NA
  dane[,i]
}
for(i in 149:157){
  dane[,i][is.na(dane[,157])] <- NA
  dane[,i][dane[,158] == 0] <- NA
  dane[,i]
}

# Normalizacja danych liczbowych
for(i in c(59,77,94,111,127,143,158)){
    dane[,i] = normalizuj(dane[,i])
    dane[,i][dane[,i] > 0 & dane[,i] <= 0.05] <- 0.05
    dane[,i][dane[,i] > 0.05] <- round(dane[,i][dane[,i] > 0.05],1)
    dane[,i]
}
for(i in c(62,159)){
  dane[,i][!is.na(dane[,i])] = normalizuj(dane[,i][!is.na(dane[,i])])
  dane[,i]
}
for(i in c(3,68,85,102,119)){
  dane[,i] = normalizuj(dane[,i])
  dane[,i]
}
# Zapis danych liczbowych bez przeskalowania
liczby <- dane
write.table(liczby, "liczbowe.csv", sep=",")

dane[,2] = normalizuj(dane[,2])

# Przeskalowanie danych liczbowych na potrzeby analizy statystycznej wg warunków określonych w przeskaluj.R
dane[,7] = przeskaluj(dane[,7], 10, 5)
dane[,8] = przeskaluj(dane[,8], 10, 4)
dane[,14:51] = przeskaluj(dane[,14:51], 7, 3)
dane[,c(52:58,69:76,86:93,103:110,120:126,135:141,149:156)] = przeskaluj(dane[,c(52:58,69:76,86:93,103:110,120:126,135:141,149:156)], 11, 5)
dane[,c(12,63:67,80:84,97:101,114:118,130:134,144:148,160:164)] = przeskaluj(dane[,c(12,63:67,80:84,97:101,114:118,130:134,144:148,160:164)], 5, 3)

# Eksport bazy danych po normalizacji i przeskalowaniu do pliku
write.table(dane, "statystyczne.csv", sep=",")
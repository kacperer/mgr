wartosci <- c("2-3 razy dziennie" = 2.5,
              "1 raz dziennie" = 1,
              "1 w miesiącu" = 1/30,
              "1 w tygodniu" = 4/30,
              "2-3 razy w miesiącu" = 2.5/30,
              "2-3 razy w tygodniu" = 4*2.5/30,
              "4-5 razy w tygodniu" = 4*4.5/30,
              "6 razy w tygodniu" = 4*6/30,
              "Nigdy" = 0,
              "Rzadziej niż 1 raz w miesiącu" = 1/90,
              "Jedna kulka puree" = 90,
              "Jeden cały ziemniak" = 85,
              "1 cały" = 170,
              "1 ćwiartka" = 170/4,
              "1 plaster" = 20,
              "1 połowa" = 170/2,
              "1 pomidor koktajlowy" = 20,
              "świderki" = 100,
              "muszelki" = 120,
              "spaghetti" = 120,
              "kokardki" = 125,
              "rurki" = 110,
              "wstążki" = 120,
              "nitki" = 140,
              "Kukurydziane" = 30,
              "Miodowe" = 30,
              "Czekoladowe" = 40,
              "Cynamonowe" = 30,
              "Owsiane" = 50,
              "Muesli" = 50,
              "Granola" = 60,
              "1 chleb chrupki" = 12,
              "1 kajzerka" = 60,
              "1 kromka  chleba razowego" = 35,
              "1 kromka chleba litewskiego" = 40,
              "1 grahamka" = 90,
              "1 kromka chleba baltonowskiego" = 35,
              "1 kromka pumpernikla" = 40,
              "Nie spożywam" = 0,
              "Nie kupuję" = NA,
              "Mężczyzna" = 0,
              "Kobieta" = 1,
              "Wieś" = 354.730635,
              "Miasto do 20 tys. mieszkańców" = 7121.834049,
              "Miasto pow. 20 tys. mieszkańców do 100 tys. mieszkańców" = 40611.441989,
              "Miasto pow. 100 tys. mieszkańców do 500 tys. mieszkańców" = 191847.878788,
              "Miasto pow. 500 tys. mieszkańców" = 877737.6,
              "Uczę się/studiuję" = 1,
              "Ukończona szkoła podstawowa" = 0,
              "Ukończone gimnazjum" = 0.25,
              "Ukończona zasadnicza szkoła zawodowa" = 0.5,
              "Ukończone liceum/technikum" = 0.5,
              "Ukończone studia licencjackie/inżynierskie" = 0.75,
              "Ukończone studia magisterskie" = 1,
              "Pieniędzy nie wystarcza nawet na najtańsze jedzenie i ubranie" = 0,
              "Pieniędzy wystarcza tylko na najtańsze jedzenie, nie wystarcza na ubrania" = 0,
              "Pieniędzy wystarcza tylko na najtańsze jedzenie i ubrania" = 0.5,
              "Żyjemy bardzo oszczędnie, aby odłożyć na poważniejsze zakupy" = 0.5,
              "Żyjemy oszczędnie i wystarcza nam na wszystko" = 1,
              "Wystarcza mi na wszystko bez szczególnego oszczędzania" = 1,
              "Nie wiem/trudno powiedzieć" = NA,
              "nie stosuję diety" = 0,
              "bezglutenową" = 1,
              "wegetariańską" = 1,
              "wegańską" = 1,
              "redukcję masy ciała" = 1,
              "redukcję masy ciała (odchudzającą)" = 1,
              "Kupuję je przez cały rok, nawet poza sezonem" = 0,
              "Kupuję je przez cały rok" = 0,
              "Kupuję je głównie w okresie, kiedy są dostępne w odpowiedniej cenie" = 1,
              "Kupuję je wtedy, kiedy mam na nie ochotę i apetyt" = 0,
              "Kupuję je przede wszystkim wtedy, kiedy mam na nie ochotę i apetyt" = 0,
              "Kupuję je dopiero wtedy, gdy zabraknie ich w moim ogródku (poza sezonem)" = 0,
              "Nie kupuję ich" = 0,
              "Bezpośrednio u rolnika" = 0,
              "Bazarek" = 0.25,
              "Warzywniak" = 0.25,
              "Sklep osiedlowy/wiejski" = 0.5,
              "Dyskont (np. Biedronka, Lidl)" = 0.75,
              "Hipermarket (z centrum handlowym)" = 1,
              "Supermarket (wolnostojący, bez centrum handlowego)" = 1,
              "Uprawiam je w ogródku/na działce" = 0,
              "Punkty i sklepy z żywnością ekologiczną" = 0.25,
              "Sam/a robię" = 0,
              "Piekarnia" = 0.25,
              "z ziemi luzem" = 0,
              "z ziemi w worku" = 1/3,
              "myte luzem" = 2/3,
              "myte w worku" = 1,
              "w całości bez opakowania" = 0,
              "w całości w zamkniętym opakowaniu" = 0.5,
              "krojony w zamkniętym opakowaniu" = 1,
              "płatki ziemniaczane" = 1,
              "mąka/skrobia ziemniaczana" = 1,
              "chipsy ziemniaczane" = 1,
              "frytki ziemniaczane i smażone ziemniaki w innych formach" = 1,
              "pyzy ziemniaczane" = 1,
              "sok pomidorowy" = 1,
              "koncentrat pomidorowy" = 1,
              "sos spaghetti" = 1,
              "ketchup" = 1,
              "dżem pomidorowy" = 1,
              "suszone pomidory" = 1,
              "chipsy z pomidorów" = 1,
              "sok jabłkowy 100% klarowny" = 1,
              "sok jabłkowy 100% mętny" = 1,
              "chipsy jabłkowe" = 1,
              "dżem/konfitura jabłkowa" = 1,
              "sok pomarańczowy 100%" = 1,
              "dżem/konfitura pomarańczowa" = 1,
              "żadne z wymienionych" = 0)
przelicz <- function(kolumna){
    wartosci[as.character(dane[,kolumna])]
}
# dane[,68] <- przelicz(dane[,68])
# dane[,70] <- przelicz(dane[,70])
# dane[,98] <- przelicz(dane[,98])
# dane[,100] <- przelicz(dane[,100])
# dane[,125] <- przelicz(dane[,125])
# dane[,147] <- przelicz(dane[,147])
# dane[,169] <- przelicz(dane[,169])
# dane[,170] <- przelicz(dane[,170])
# dane[,171] <- przelicz(dane[,171])
# dane[,172] <- przelicz(dane[,172])
# dane[,173] <- przelicz(dane[,173])
# dane[,174] <- przelicz(dane[,174])
# dane[,175] <- przelicz(dane[,175])
# dane[,176] <- przelicz(dane[,176])
# dane[,191] <- przelicz(dane[,191])
# dane[,192] <- przelicz(dane[,192])
# dane[,193] <- przelicz(dane[,193])
# dane[,194] <- przelicz(dane[,194])
# dane[,195] <- przelicz(dane[,195])
# dane[,196] <- przelicz(dane[,196])
# dane[,197] <- przelicz(dane[,197])
# dane[,198] <- przelicz(dane[,198])
# dane[,214] <- przelicz(dane[,214])
# dane[,215] <- przelicz(dane[,215])
# dane[,216] <- przelicz(dane[,216])
# dane[,217] <- przelicz(dane[,217])
# dane[,218] <- przelicz(dane[,218])
# dane[,219] <- przelicz(dane[,219])
# dane[,220] <- przelicz(dane[,220])
# dane[,221] <- przelicz(dane[,221])
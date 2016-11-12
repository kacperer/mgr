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
              "świderki" = 170/2,
              "muszelki" = 170/2,
              "spaghetti" = 170/2,
              "kokardki" = 170/2,
              "rurki" = 170/2,
              "wstążki" = 170/2,
              "nitki" = 170/2,
              "Kukurydziane" = 30,
              "Miodowe" = 30,
              "Czekoladowe" = 40,
              "Cynamonowe" = 30,
              "Owsiane" = 50,
              "Muesli" = 50,
              "Granola" = 60,
              "Nie spożywam" = 0,
              "Nie kupuję" = 0)
przelicz <- function(kolumna){
  badanie = wartosci[as.character(dane[,kolumna])]
  if(sum(is.na(badanie)) > 1)
    dane[,kolumna]
  else
    badanie
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
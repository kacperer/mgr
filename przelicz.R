przelicz <- function(kolumna){
  as.vector(wartosci[dane[,kolumna]])
}
wartosci <- c("2-3 razy dziennie" = 2.5,
              "1 raz dziennie" = 1,
              "1 w miesiącu" = 1/30,
              "1 w tygodniu" = 4/30,
              "2-3 razy w miesiącu" = 2.5/30,
              "2-3 razy w tygodniu" = 4*2.5/30,
              "4-5 razy w tygodniu" = 4*4.5/30,
              "6 razy w tygodniu" = 4*6/30,
              "Nigdy" = 0,
              "Rzadziej niż 1 raz w miesiącu" = 1/90)

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
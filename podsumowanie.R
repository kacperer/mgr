# Wczytanie danych po wstępnej obróbce
dane <- read.csv("liczbowe.csv", sep=",", fileEncoding = "UTF-8")
podsumowanie = data.frame(n=NA,
                          Srednia = NA,
                          SD = NA,
                          Minimum = NA,
                          Maksimum = NA)
for(i in 1:dim(dane)[2]){
  podsumowanie = rbind(podsumowanie, c(length(dane[,i][!is.na(dane[,i])]),
                                       round(mean(dane[,i][!is.na(dane[,i])]),3),
                                       round(sd(dane[,i][!is.na(dane[,i])]),3),
                                       round(min(dane[,i][!is.na(dane[,i])]),3),
                                       round(max(dane[,i][!is.na(dane[,i])]),3)))
  podsumowanie
}
podsumowanie = na.omit(podsumowanie)
podsumowanie = cbind(Zmienna = names(dane), podsumowanie)
write.table(podsumowanie, "podsumowanie.csv", sep=",")
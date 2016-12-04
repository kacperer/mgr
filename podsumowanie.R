# Wczytanie danych po wstępnej obróbce
source("preprocessing.R", encoding = "UTF-8")
nazwy <- read.csv("nazwy.csv", sep=";", encoding = "windows-1250")
podsumowanie = data.frame(Min = NA,
                          Max = NA,
                          n=NA,
                          Średnia = NA,
                          SD = NA)
for(i in 1:dim(liczby)[2]){
  podsumowanie = rbind(podsumowanie, c(round(min(liczby[,i][!is.na(liczby[,i])]),3),
                                       round(max(liczby[,i][!is.na(liczby[,i])]),3),
                                       length(liczby[,i][!is.na(liczby[,i])]),
                                       round(mean(liczby[,i][!is.na(liczby[,i])]),3),
                                       round(sd(liczby[,i][!is.na(liczby[,i])]),3)))
  podsumowanie
}
podsumowanie = na.omit(podsumowanie)
row.names(podsumowanie) <- names(liczby)
podsumowanie = cbind(nazwy, podsumowanie)
write.table(podsumowanie, "podsumowanie.csv", sep=",")
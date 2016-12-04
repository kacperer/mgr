# Wczytanie danych po wstępnej obróbce
source("preprocessing.R", encoding = "UTF-8")

wyniki.max <- data.frame(Nr_A=NA,
                      Zmienna_A=NA,
                      Srednia_A=NA,
                      SD_A=NA,
                      Nr_B=NA,
                      Zmienna_B=NA,
                      Średnia_B=NA,
                      SD_B=NA,
                      X=NA,
                      df=NA,
                      p=NA,
                      r=NA)
wyniki.stat <- wyniki.max

# Testy zgodności chi-kwadrat i korelacja Pearsona między wszystkimi parametrami
# - przy p < 0.05 oraz r >= 0.5 lub r <= -0.5
for (i in 1:dim(dane)[2]){
  for (j in i:dim(dane)[2]){
    A = dane[,i][!is.na(dane[,i]) & !is.na(dane[,j])]
    B = dane[,j][!is.na(dane[,i]) & !is.na(dane[,j])]
    chi.kwadrat <- chisq.test(A, B)
    korelacja <- cor.test(A, B, method = "pearson")
    if(i!=j
       && as.numeric(chi.kwadrat$p.value) < 0.05
       && (as.numeric(korelacja$estimate) >= 0.5
           || as.numeric(korelacja$estimate) <= -0.5)){
      wyniki.max = rbind(wyniki.max, c(i,
                               names(dane[i]),
                               j,
                               names(dane[j]),
                               as.numeric(chi.kwadrat$statistic),
                               as.numeric(chi.kwadrat$parameter),
                               as.numeric(chi.kwadrat$p.value),
                               as.numeric(korelacja$estimate)))
      wyniki.max
    }
  }
}
wyniki.max=na.omit(wyniki.max)
write.table(wyniki.max, "wyniki-max.csv", sep=",")

# Testy zgodności chi-kwadrat i korelacja Pearsona między parametrami metryczkowymi a innymi
# - przy p < 0.05 oraz r >= 0.2 lub r <= -0.2
for (i in 1:51){
  for (j in 52:dim(dane)[2]){
    A = dane[,i][!is.na(dane[,i]) & !is.na(dane[,j])]
    B = dane[,j][!is.na(dane[,i]) & !is.na(dane[,j])]
    chi.kwadrat <- chisq.test(A, B)
    korelacja <- cor.test(A, B, method = "pearson")
    if(as.numeric(chi.kwadrat$p.value) < 0.05
       && (as.numeric(korelacja$estimate) >= 0.2
           || as.numeric(korelacja$estimate) <= -0.2)){
      wyniki.stat = rbind(wyniki.stat, c(i,
                                 names(dane[i]),
                                 round(mean(dane[,i]),3),
                                 round(sd(dane[,i]),3),
                                 j,
                                 names(dane[j]),
                                 round(mean(dane[,j]),3),
                                 round(sd(dane[,j]),3),
                                 round(as.numeric(chi.kwadrat$statistic),3),
                                 round(as.numeric(chi.kwadrat$parameter),3),
                                 round(as.numeric(chi.kwadrat$p.value),3),
                                 round(as.numeric(korelacja$estimate),3)))
      wyniki.stat
    }
  }
}
wyniki.stat=na.omit(wyniki.stat)
write.table(wyniki.stat, "wyniki-stat.csv", sep=",")
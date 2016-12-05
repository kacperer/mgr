source("normalizuj.R", encoding = "UTF-8")
source("przeskaluj.R", encoding = "UTF-8")
testy.wszystkie <- function(){
  print("testy rozpoczęte")
  testy.normalizuj()
  testy.przeskaluj()
  print("testy zakończone")
}
testy.wszystkie()
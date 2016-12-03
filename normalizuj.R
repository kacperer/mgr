source("error.R", encoding = "UTF-8")
normalizuj <- function(x){
  if(max(x)==min(x)){
    if(max(x)==0)
      return(x)
    return(x/max(x))
  }
  (x-min(x))/(max(x)-min(x))
}

testy.normalizuj <- function(){
  testy.normalizuj_przypadek(c(5,5,5,5,5), c(1,1,1,1,1))
  testy.normalizuj_przypadek(c(0,0,0,0,0), c(0,0,0,0,0))
  testy.normalizuj_przypadek(c(0,1,2,3,4), c(0,0.25,0.5,0.75,1))
}

testy.normalizuj_przypadek <- function(przypadek, oczekiwana){
  rzeczywista = normalizuj(przypadek)
  if(!(isTRUE(all(oczekiwana == rzeczywista)))){
    error("Normalizowanie nieudane", expected, actual)
  }
}
testy.normalizuj()
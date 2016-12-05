source("error.R", encoding = "UTF-8")
przeskaluj <- function(x, stara, nowa){
  if(stara == 11 & nowa == 5)
  {
    x = replace(x, x<2, 0)
    x = replace(x, x>=2 & x<4, 0.25)
    x = replace(x, x>=4 & x<=6, 0.5)
    x = replace(x, x>=6 & x<=8, 0.75)
    x = replace(x, x>8, 1)
  }
  else if(stara == 10 & nowa == 5)
  {
    x = replace(x, x>1 & x<=3, 0.25)
    x = replace(x, x>3 & x<=6, 0.5)
    x = replace(x, x>6 & x<=10, 0.75)
    x = replace(x, x>10, 1)
  }
  else if(stara == 10 & nowa == 4)
  {
    x = replace(x, x>0 & x<=2, 1/3)
    x = replace(x, x>2 & x<=6, 2/3)
    x = replace(x, x>6, 1)
  }
  else if(stara == 7 & nowa == 3)  
  {
    x = replace(x, x<3, 0)
    x = replace(x, x>=3 & x<=5, 0.5)
    x = replace(x, x>5, 1)
  }
  else if(stara == 5 & nowa == 3)
  {
    x = replace(x, x<3, 0)
    x = replace(x, x==3, 0.5)
    x = replace(x, x>3, 1)
  }
  else
    stop("Niepoprawne argumenty dla funkcji przeskalowania!")
}
testy.przeskaluj <- function(){
  testy.przeskaluj_przypadek(c(1,3,4,7), 7, 3, c(0,0.5,0.5,1))
  testy.przeskaluj_przypadek(c(0,1,2,3,4,5,6,7,8,9,10), 11, 5, c(0,0,0.25,0.25,0.5,0.5,0.5,0.75,0.75,1,1))
  testy.przeskaluj_przypadek(c(1,2,3,4,5,6,7), 7, 3, c(0,0,0.5,0.5,0.5,1,1))
  testy.przeskaluj_przypadek(c(1,2,3,4,5), 5, 3, c(0,0,0.5,1,1))
  testy.przeskaluj_przypadek(c(5,3,4,1,2), 5, 3, c(1,0.5,1,0,0))
}
testy.przeskaluj_przypadek <- function(przypadek, stara, nowa, oczekiwana){
  rzeczywista = przeskaluj(przypadek, stara, nowa)
  if(!(isTRUE(all(oczekiwana == rzeczywista)))){
    error("Przeskalowanie nieudane", oczekiwana, rzeczywista)
  }
}
testy.przeskaluj()
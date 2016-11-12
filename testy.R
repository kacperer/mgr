# source("norm.r")
normalize <- function(x){
  if(max(x)==min(x)){
    if(max(x)==0)
      return(x)
    return(x/max(x))
  }
  (x-min(x))/(max(x)-min(x))
}

tests.Normalization <- function(){
  tests.NormalizationCase(c(5,5,5,5,5), c(1,1,1,1,1))
  tests.NormalizationCase(c(0,0,0,0,0), c(0,0,0,0,0))
  tests.NormalizationCase(c(0,1,2,3,4), c(0,0.25,0.5,0.75,1))
  
}

tests.NormalizationCase <- function(input, expected){
  actual = normalize(input)
  if(!(isTRUE(all(expected == actual)))){
    reportError("Norm failed", expected, actual)
  }
}

reportError <- function(message, expected, actual){
  print(message)
  print("expected: ")
  print(expected)
  print("actual: ")
  print(actual)
}
tests.All <- function(){
  print("tests started")
  tests.Normalization()
  tests.rescale()
  print("tests completed")
}

tests.All()
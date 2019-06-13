vector_help  <- function(a, s2, s3, s4, s44, s5, i) {  #return discount cash from history of life (vector - argument a)
  
  result2 = 0
  result3 = 0
  result4 = 0
  result5 = 0
  v = 1/(1+i)
  
  for (i in (1:length(a))) {
    
    if (a[i] == 2) {
      result2 = result2 + v^(i)*s2 
    }
    else if (a[i] == 3) {
      result3 = result3 + v^(i)*s3 
    }
    else if (a[i] == 6) {
      result4 = result4 + v^(i-1)*s4 + v^(i)*s44 
    }
    else if (a[i] == 4) {
      result4 = result4 + v^(i)*s44
    }
    else if(a[i] == 5){
      result5 = result5 + v^(i-1)*s5
    }
    else {0}
  }
  return(lista <- list('result2' = result2, 'result3' = result3, 'result4' = result4, 'result5' = result5 ) )
}


annuity_help <- function(w, i){ # w - vector of symulated entrie life
  
  v = 1/(1+i)
  w <- replace(w, w != 1, 0)
  return(sum(v^seq(0,length(w)-1,1)*w)) #sk³adka p³atna z góru
}  




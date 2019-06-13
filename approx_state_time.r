
library(sqldf)
library(data.table)

approx_state_time <- function(amount) {
  
  m <- numeric(amount * 75) # hope that is enought, however it is
  w <- numeric(amount * 85)
  a_m = 1
  a_w = 1
  for(i in 1:amount){
    
    k1 <- random_symulation('man', 420)
    k2 <- random_symulation('woman', 420)
    m[a_m:(a_m+length(k1)-1)] <- k1
    w[a_w:(a_w+length(k2)-1)] <- k2
   
    a_m = a_m + length(k1) 
    a_w = a_w + length(k2) 
  }
  m <- m[1:(min(which(m==0))-1)]
  w <- w[1:(min(which(w==0))-1)]
  
  m <- data.table(m)
  w <- data.table(w)
  setnames(m, names(m), c("v")) 
  setnames(w, names(w), c("v")) 
  
  vect <- data.table('f' = c(1,2,3,4,6,5))
  
  result_m <- sqldf("select v, count(*) AS 'how_long_m'  
                    from m
                    group by v")
  
  result_m <- sqldf('SELECT f AS "v",
                    how_long_m 
                    FROM vect
                    left join result_m on v = f')
  result_m[,2] <-  ifelse(is.na(result_m[,2]), 0, result_m[,2])
  
  #___________________________
  
  result_w <- sqldf("select v, count(*) AS 'how_long_w'  
                    from w
                    group by v")
  
  result_w <- sqldf('SELECT f AS "v",
                    how_long_w 
                    FROM vect
                    left join result_w on v = f')
  result_w[,2] <-  ifelse(is.na(result_w[,2]), 0, result_w[,2])
  
  
  result <- cbind(result_m, result_w[,2])
  result <- cbind(result, result[,2]/amount, result[,3]/amount )
  result[,2] <- round(100*result[,2]/sum( result[,2]),2) 
  result[,3] <- round(100*result[,3]/sum( result[,3]),2)
  names(result) <- c('state', 'percentage_stay_m', 'percentage_stay_w', 'amount_stay_m',  'amount_stay_w' )
  
  return(result) 
}

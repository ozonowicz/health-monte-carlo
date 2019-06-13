library(data.table)
library(sqldf)
source("random_ppl_generator.R")


company_symulation <- function(initial_cash, initial_age, count_ppl, term_policy, r, s2, s3, s4, s44, s5, p_m, p_w, i){ #pamiêtaj, ¿e hajsy co mamy tez pracujooooo na stopie
  
  p_w = p_w * (1 + r)
  p_m = p_m * (1 + r)
  mtx <- random_ppl_generator(initial_age, count_ppl, term_policy, p_m, p_w) 
  comp_cash_hist <- c( rep(0,term_policy+1))
  comp_cash_hist[1] = initial_cash
  
  h <- data.frame('f' = c(p_w,p_m,2,3,4,6,5)) 
  
  for (y in 1 : term_policy) {
    
    if(y != 1 ){
      comp_cash_hist[y] = comp_cash_hist[y-1]*(1+i) + sum(assist[c(3:6),2] * c(-s2, -s3, -s44, -s44) )  # celowo jest tutaj dwa razy s44!
    }
    
    vect <- data.frame('r' = mtx[,y])
    assist <- sqldf('SELECT r,
                    count(*) AS count
                    FROM vect
                    GROUP BY r')
    
    assist <- sqldf('SELECT f AS "r",
                    count 
                    FROM h
                    left join assist on f = r')
    
    assist[,2] <-  ifelse(is.na(assist[,2]), 0, assist[,2])
    
    if(y == 1) {comp_cash_hist[y] = comp_cash_hist[y] + sum(assist[c(1:2),2]     * c(p_w , p_m) )}
          else {comp_cash_hist[y] = comp_cash_hist[y] + sum(assist[c(1:2,6,7),2] * c(p_w , p_m, -s4, -s5) ) }
  }
  
  comp_cash_hist[term_policy + 1] = comp_cash_hist[term_policy]*(1+i) + sum(assist[c(3:6),2] * c(-s2, -s3, -s44, -s44) )
  
  #-------------------
  
  vect <- data.frame('r' = mtx[,term_policy + 1])
  assist <- sqldf('SELECT r,
                  count(*) AS count
                  FROM vect
                  GROUP BY r')
  
  assist <- sqldf('SELECT f AS "r",
                  count 
                  FROM h
                  left join assist on f = r')    
  assist[,2] <-  ifelse(is.na(assist[,2]), 0, assist[,2])

  comp_cash_hist[term_policy + 1] = comp_cash_hist[term_policy]*(1+i) + sum(assist[c(6:7),2] * c(-s4, -s5) )
  

  return(comp_cash_hist)
}







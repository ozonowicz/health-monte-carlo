
random_ppl_generator <- function(initial_age, count_ppl, term_policy, p_m, p_w){ #initial_age meaning age of ppl when we come to insure company (25 in project) 
  
  result <- matrix(NA, nrow = count_ppl, ncol = term_policy + 1)
  
  for (q in 1:count_ppl) { # random ppl generator
    
    if(runif(1) >= 0.5){
      gender = 'man'
    } else { 
      gender = 'woman'}
    o <- random_symulation(gender, initial_age + term_policy)
    while( o[initial_age + 1] == 5  | o[initial_age + 1] == 0 ) {o <- random_symulation(gender, initial_age + term_policy) } #we only want symulate alive ppl at time 'initial_age' - the point when we come to the company
    #we dont want ppl at initial_age in state 0 or 5, '0' - obvious, 5- bcs I assume we come at start of year, ppl at state '5' got money immediately "at the end of each year which person entered this state" 
    if (gender == 'man') {
      o[which(o == 1)] <- p_m
    } else {
      o[which(o == 1)] <- p_w
    }
    
    result[q,] <- o[(initial_age + 1) : (initial_age + term_policy + 1)]
  }  
  return(result)
} 



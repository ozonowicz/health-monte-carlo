#to compute entire  history of life put condition_age bigger than 200, for example condition_age 30 mean function will return vector of 31 length (30 years old)

random_symulation <- function(gender, condition_age) { #condition age - help to make fasters compute
  
  dead <- 'false'
  history <- c(rep(0,102))
  age = 0
  history[age + 1] = 1
  random <- runif(101)
  
  while (dead =='false') {

    if(gender == 'man'){
      cumulative <- lista_m[[age+1]][ifelse(history[age + 1] == 6, 4,history[age + 1]),]}
    else{
      cumulative <- lista_w[[age+1]][ifelse(history[age + 1] == 6, 4,history[age + 1]),]}
    
    if (random[age + 1] <= cumulative[1]) {
      history[age + 2] = 1
    } else if (random[age + 1] > cumulative[1] & random[age + 1] <= cumulative[2]) {
      history[age + 2] = 2
    } else if (random[age + 1] > cumulative[2] & random[age + 1] <= cumulative[3]) {
      history[age + 2] = 3
    } else if (random[age + 1] > cumulative[3] & random[age + 1] <= cumulative[4]) {
      if(history[age + 1] > 3) {history[age + 2] = 4} 
      else {history[age + 2] = 6}
    } else {
      history[age + 2] = 5
      dead = "true"
    }
    age = age + 1
    if(age == condition_age) break
  }
  if(condition_age < 101) {return (history[1:(condition_age+1)])} 
  else {return(history[1:which(history==5)])}
}



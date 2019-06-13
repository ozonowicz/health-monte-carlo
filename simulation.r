library(sqldf)
library(data.table)
library(matrixStats)
library(doParallel)

source("approx_state_time.r")
source("approx_money.r")
source("company_symulation.r")
source("random_symulation.r")

data <- read.csv("ttz.csv", head=TRUE, dec = ",", sep=";")
x <- data$ ï.¿x
data <- data[-c(3,5)]
head(data)
data <- cbind(data, 'Prob_dead_man'     =                1 - c(data$mlx[seq(2,length(data$mlx),1)],0)/data$mlx  )
data <- cbind(data, 'Prob_dead_man*10'  = pmin(1, 10  * (1 - c(data$mlx[seq(2,length(data$mlx),1)],0)/data$mlx  )))
data <- cbind(data, 'Prob_dead_man*50'  = pmin(1, 50  * (1 - c(data$mlx[seq(2,length(data$mlx),1)],0)/data$mlx  )))
data <- cbind(data, 'Prob_dead_man*250' = pmin(1, 250 * (1 - c(data$mlx[seq(2,length(data$mlx),1)],0)/data$mlx  )))

data <- cbind(data, 'Prob_dead_woman'     =                1 - c(data$klx[seq(2,length(data$klx),1)],0)/data$klx  )
data <- cbind(data, 'Prob_dead_woman*10'  = pmin(1, 10  * (1 - c(data$klx[seq(2,length(data$klx),1)],0)/data$klx  )))
data <- cbind(data, 'Prob_dead_woman*50'  = pmin(1, 50  * (1 - c(data$klx[seq(2,length(data$klx),1)],0)/data$klx  )))
data <- cbind(data, 'Prob_dead_woman*250' = pmin(1, 250 * (1 - c(data$klx[seq(2,length(data$klx),1)],0)/data$klx  )))
tail(data)
head(data)


#--------------------------------------------- create matrixes of transition 

lista_m <- rep(list(matrix(NA,5,5)),101)
gender = 'man'
for (j in 1:101) {
  lista_m[[j]] <- t(matrix(c(1/10 * max(9 - 9 * data[j, ifelse(gender == 'man', 4,8)], 9.9 - 10*  data[j, ifelse(gender == 'man', 4,8)]),
                             1/10 * min(1- data[j, ifelse(gender == 'man', 4,8)], 0.1),
                             0,
                             0,
                             data[j, ifelse(gender == 'man', 4,8)],
                             #_______________________________
                             0.3 - 3/7 * max(data[j, ifelse(gender == 'man', 5,9)] - 0.3, 0),
                             5/7 * max( data[j, ifelse(gender == 'man', 5,9)] + 0.4, 0.7) - data[j, ifelse(gender == 'man', 5,9)],
                             0.2 - 2/7 * max(data[j, ifelse(gender == 'man', 5,9)] - 0.3, 0),
                             0,
                             data[j, ifelse(gender == 'man', 5,9)],
                             #_______________________________
                             0.2 - 2/5 * max(data[j, ifelse(gender == 'man', 6,10)] - 0.5, 0),
                             0,
                             3/5* max(data[j, ifelse(gender == 'man', 6,10)] + 2/3, 7/6) - data[j, ifelse(gender == 'man', 6,10)],
                             0.1 - 1/5* max( data[j, ifelse(gender == 'man', 6,10)] - 0.5, 0),
                             data[j, ifelse(gender == 'man', 6,10)],
                             #_______________________________
                             0,
                             0,
                             0,
                             1 - data[j, ifelse(gender == 'man', 7,11)] ,
                             data[j, ifelse(gender == 'man', 7,11)],
                             #_______________________________
                             0,
                             0,
                             0,
                             0,
                             1), ncol = 5))
}

#---------------------------------------------

lista_w <- rep(list(matrix(NA,5,5)),101)
gender = 'woman'
for (j in 1:101) {
  lista_w[[j]] <- t(matrix(c(1/10 * max(9 - 9 * data[j, ifelse(gender == 'man', 4,8)], 9.9 - 10*  data[j, ifelse(gender == 'man', 4,8)]),
                             1/10 * min(1- data[j, ifelse(gender == 'man', 4,8)], 0.1),
                             0,
                             0,
                             data[j, ifelse(gender == 'man', 4,8)],
                             #_______________________________
                             0.3 - 3/7 * max(data[j, ifelse(gender == 'man', 5,9)] - 0.3, 0),
                             5/7 * max( data[j, ifelse(gender == 'man', 5,9)] + 0.4, 0.7) - data[j, ifelse(gender == 'man', 5,9)],
                             0.2 - 2/7 * max(data[j, ifelse(gender == 'man', 5,9)] - 0.3, 0),
                             0,
                             data[j, ifelse(gender == 'man', 5,9)],
                             #_______________________________
                             0.2 - 2/5 * max(data[j, ifelse(gender == 'man', 6,10)] - 0.5, 0),
                             0,
                             3/5* max(data[j, ifelse(gender == 'man', 6,10)] + 2/3, 7/6) - data[j, ifelse(gender == 'man', 6,10)],
                             0.1 - 1/5* max( data[j, ifelse(gender == 'man', 6,10)] - 0.5, 0),
                             data[j, ifelse(gender == 'man', 6,10)],
                             #_______________________________
                             0,
                             0,
                             0,
                             1 - data[j, ifelse(gender == 'man', 7,11)] ,
                             data[j, ifelse(gender == 'man', 7,11)],
                             #_______________________________
                             0,
                             0,
                             0,
                             0,
                             1), ncol = 5))
}

#--------------------------------------------

for (j in 1:101) {  # sum by rows using to obtian segment (to use MC)
  lista_m[[j]] <-  rowCumsums(lista_m[[j]])
  lista_w[[j]] <-  rowCumsums(lista_w[[j]])
}

#---------------------------------------------

# Computation of transition probs
n.cores <- detectCores()

system.time({
  registerDoParallel(n.cores)
  a <- foreach(k = 1:500, .combine = rbind, .packages = c('sqldf','data.table')) %dopar% approx_state_time(amount = 1000)
})
stopImplicitCluster()

###########################

# computation is long
# load that file to omit it and get its results outright
load(file = "approx_state_time_500x100000.dat")



typeof(a)
a_bup = data.table(a)
a <- data.table(a)
a <- a[,c(1,4:5)]

hist(unlist(a[which(state==1),2], use.names=FALSE), main="", col="green4", xlab = "years in state 1 - male", ylab = "Number of simulations", freq = T) #hist of state 1 for man
hist(unlist(a[which(state==1),3], use.names=FALSE), main="", col="green4", xlab = "years in state 1 - female", ylab = "Number of simulations", freq = T) #hist of state 1 for woman

hist(unlist(a[which(state==2),2], use.names=FALSE), main="", col="green4", xlab = "years in state 2 - male", ylab = "Number of simulations", freq = T) #hist of state 2 for man
hist(unlist(a[which(state==2),3], use.names=FALSE), main="", col="green4", xlab = "years in state 2 - female", ylab = "Number of simulations", freq = T) #hist of state 2 for woman

hist(unlist(a[which(state==3),2], use.names=FALSE), main="", col="green4", xlab = "years in state 2' - male", ylab = "Number of simulations", freq = T) #hist of state 3 for man
hist(unlist(a[which(state==3),3], use.names=FALSE), main="", col="green4", xlab = "years in state 2' - female", ylab = "Number of simulations", freq = T) #hist of state 3 for woman

hist(unlist(a[which(state==4),2], use.names=FALSE), main="", col="green4", xlab = "years in state 2'' - male", ylab = "Number of simulations", freq = T) #hist of state 4 for man
hist(unlist(a[which(state==4),3], use.names=FALSE), main="", col="green4", xlab = "years in state 2'' - female", ylab = "Number of simulations", freq = T) #hist of state 4 for woman

hist(unlist(a[which(state==6),2], use.names=FALSE), main="", col="green4", xlab = "years in state 2''' - male", ylab = "Number of simulations", freq = T, breaks = 20) #hist of state 6 for man
hist(unlist(a[which(state==6),3], use.names=FALSE), main="", col="green4", xlab = "years in state 2''' - female", ylab = "Number of simulations", freq = T) #hist of state 6 for woman

#-----------------------------------------------------------------------------------

########## computation of benefits
system.time({
  registerDoParallel(n.cores)
  b <- foreach(k = 1:500, .combine = rbind, .packages = c('sqldf','data.table')) %dopar% approx_money(count = 1000, s2 = 15000, s3 = 25000, s4 =  100000, s44 = 50000, s5 = 100000, i = 0.02)  
})
stopImplicitCluster()

#################


# computation is long
# load that file to omit it and get its results outright
load(file = "approx_money_500x300000.dat")
b = money_app

b_bup = data.table(b)

head(b)

hist(unlist(b[,1], use.names=FALSE), main="", col="green4", xlab = "Total benefits - male", ylab = "Number of simulations", freq = T) #hist of total expected benefits for man
hist(unlist(b[,7], use.names=FALSE), main="", col="green4", xlab = "Total benefits - female", ylab = "Number of simulations", freq = T) #hist of total expected benefits for woman

hist(unlist(b[,2], use.names=FALSE), main="", col="green4", xlab = "Benefits at state 2 - male", ylab = "Number of simulations", freq = T) #hist of state 2 expected benefits for man
hist(unlist(b[,8], use.names=FALSE), main="", col="green4", xlab = "Benefits at state 2 - female", ylab = "Number of simulations", freq = T) #hist of state 2 expected benefits for woman

hist(unlist(b[,3], use.names=FALSE),  main="", col="green4", xlab = "Benefits at state 2' - male", ylab = "Number of simulations", freq = T)  #hist of state 3 expected benefits for man
hist(unlist(b[,9], use.names=FALSE), main="", col="green4", xlab = "Benefits at state 2' - female", ylab = "Number of simulations", freq = T)   #hist of state 3 expected benefits for woman

hist(unlist(b[,4], use.names=FALSE), main="", col="green4", xlab = "Benefits at state 2'' - male", ylab = "Number of simulations", freq = T) #hist of state 4 expected benefits for man
hist(unlist(b[,10], use.names=FALSE), main="", col="green4", xlab = "Benefits at state 2'' - female", ylab = "Number of simulations", freq = T)  #hist of state 4 expected benefits for woman

hist(unlist(b[,5], use.names=FALSE), main="", col="green4", xlab = "Benefits at state 3 - male", ylab = "Number of simulations", freq = T)  #hist of state 5 expected benefits for man
hist(unlist(b[,11], use.names=FALSE), main="", col="green4", xlab = "Benefits at state 3 - female", ylab = "Number of simulations", freq = T)  #hist of state 5 expected benefits for woman

hist(unlist(b[,6], use.names=FALSE),  main="", col="green4", xlab = "Yearly net premium - male", ylab = "Number of simulations", freq = T)  #hist of expected premiums for man
hist(unlist(b[,12], use.names=FALSE), main="", col="green4", xlab = "Yearly net premium - female", ylab = "Number of simulations", freq = T)  #hist of expected premiums for woman

var(unlist(b[,6], use.names=FALSE))
var(unlist(b[,12], use.names=FALSE))

###########compute probability of default
  system.time({
    registerDoParallel(n.cores)
    c <- foreach(k = 1:100, .combine = rbind, .packages = c('sqldf','data.table')) %dopar% company_symulation(initial_cash = 10000000, initial_age = 25, count_ppl = 5000, term_policy = 50, r = 0, s2 = 15000, s3 = 25000, s4 =  100000, s44 = 50000, s5 = 100000, p_w = 1610.710 , p_m = 1535.262 , i = .02)  
  })
  stopImplicitCluster()
  
##################
  
# computation is long
# load that file to omit it and get its results outright
load("matrix_of_combination.dat")
c = matrix_of_result
  
c_bup = data.table(c)


ile = 100
#100 wektorów 
resulty <-matrix(NA, ile,51)
for (a in 1:ile) {
  resulty[a,] <- unlist(c[a,])
}

matplot(t(resulty), type = 'l', xlab = "Years since company acquisition", ylab = "Surplus on insurance")
abline(h = 0)



d = NULL
system.time({
  registerDoParallel(n.cores)
  d <- foreach(rx = seq(0, 0.16, 0.02), .combine = rbind, .packages = c('sqldf','data.table')) %dopar% default_prob(50, rx)
})
stopImplicitCluster()




#-----------------------------------
#-----------------------------------
#-----------------------------------


r <- numeric(20000)
for (i in 1:20000) {

  y <- Sys.time()
  a<-random_symulation('man', 420)
  r[i] <-  Sys.time() - y
}

hist(r, breaks = 90)
mean(r)


prob_of_default = 0
system.time({ prob_of_default = 
company_symulation(initial_cash = 10000000, initial_age = 25, count_ppl = 5000, term_policy = 50, r = 0, s2 = 15000, s3 = 25000, s4 =  100000, s44 = 50000, s5 = 100000, p_w = 1419.761, p_m = 1375.149, i = .02)
})

# u¿ytkownik     system   up³ynê³o 
# 981.54       4.34    1010.08           -> 30 sek -> 13.5 sek -> 4sek ->1,5sek (without parallel computing)



system.time({
random_ppl_generator(initial_age = 85, count_ppl = 10,  term_policy = 5, p_m = 1300, p_w = 1400)
})

plot(seq(0,0.15, 0.005), matrix_of_result[,11], type="l", col = "red", xlab = "r coefficient", ylab = "prob of default")


system.time({ approx_state_time(amount = 10000) })
# u¿ytkownik     system   up³ynê³o 
# 198.02       0.90     207.51 -> 5 sek

system.time({ 
  approx_money(count = 100000, s2 = 15000, s3 = 25000, s4 =  100000, s44 = 50000, s5 = 100000, i = 0.02)  
  })


system.time({random_symulation('woman',0) })


tax_vector  = seq(0,0.15, 0.005)
initial_cap = seq(5000000, 20000000, 500000)
scene = list(xaxis = list(title = "r coefficient" ), yaxis = list(title = "initial capital", titleangle = 45), zaxis = list(title = "prob of default", titleangle = 90))
p = plotly::plot_ly(x = ~tax_vector,  y = ~initial_cap, z = ~matrix_of_result) %>% plotly::add_surface() %>% 
  plotly::layout(scene = scene)
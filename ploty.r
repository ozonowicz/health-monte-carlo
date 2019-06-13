library(ggplot2)
library(plyr)
library(tidyverse)



l <- 20000
confidence <- 0.8
a <- rnorm(l)
a <- sort(a)
left  <- round(l*((1-confidence)/2))
right <- round(l*(1-(1-confidence)/2))



dens <- density(a)

data <- tibble(x = dens$x, y = dens$y) %>% 
  mutate(variable = case_when(
    (x >= a[left] & x <= a[right]) ~ "confidence",
    TRUE ~ NA_character_))

p <- ggplot(data, aes(x, y)) + geom_line() +
  geom_area(data = filter(data, variable == 'confidence'), fill = 'red') 

p + scale_x_continuous(breaks=round(c(a[1],a[left],mean(a),a[right],a[length(a)]),2))

#_____________________________________________________________________________________________________________________________________________                       















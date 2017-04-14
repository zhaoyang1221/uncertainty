cars <- read.csv("cars.csv")



library(subspace)
carsCluters <- CLIQUE(cars1,xi=40,tau = 0.08)

library(cowplot)

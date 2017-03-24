cars <- read.csv("cars.csv")
row.names(cars) <- cars[,1]
cars <- cars[,-1]


library(subspace)
carsCluters <- CLIQUE(cars1,xi=40,tau = 0.08)

library(cowplot)

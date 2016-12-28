cars <- read.csv("cars.csv")

cars1<- subset(cars, select = -name)
cars1<- sapply(cars1, function(s){
  as.numeric(s)
})
cars1<-as.data.frame(cars1)

library(subspace)
carsCluters <- CLIQUE(cars1,xi=40,tau = 0.08)
plot(carsCluters,cars1)

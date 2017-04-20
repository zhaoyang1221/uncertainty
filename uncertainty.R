cars <- read.csv("cars.csv")
cars <- cars[,-1]
cars.noNA <-na.omit(cars)

library(fpc)
library(cluster)
cars.diss <- daisy(cars.noNA)
cars.pamk <- pamk(cars.noNA)
windows()
clusplot(pam(cars.diss, cars.pamk$nc, diss=TRUE), labels = 1)

sil <- silhouette(pam(cars.diss, cars.pamk$nc, diss=TRUE))
windows()
plot(sil)


#计算每一维度上的不确定性
uncertaintyFunc1 <- function(data){
  apply(data, 2, sd)
}

#通过聚类结果构建不确定性数据集
temp <- NULL
temp[[1]] <- rownames(sil)
temp[[2]] <- rownames(subset.matrix(sil, sil[,1] == 1))
temp[[3]] <- rownames(subset.matrix(sil, sil[,1] == 2))
temp[[4]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.71))
temp[[5]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.70 & sil[,3] <= 0.71))
temp[[6]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.68 & sil[,3] <= 0.70))
temp[[7]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.66 & sil[,3] <= 0.68))
temp[[8]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.62 & sil[,3] <= 0.66))
temp[[9]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.53 & sil[,3] <= 0.62))
temp[[10]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.45 & sil[,3] <= 0.53))
temp[[11]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.2 & sil[,3] <= 0.45))
temp[[12]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] <= 0.2))

temp[[13]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.805))
temp[[14]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.8 & sil[,3] <= 0.805))
temp[[15]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.79 & sil[,3] <= 0.8))
temp[[16]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.77 & sil[,3] <= 0.79))
temp[[17]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.75 & sil[,3] <= 0.77))
temp[[18]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.67 & sil[,3] <= 0.75))
temp[[19]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.6 & sil[,3] <= 0.67))
temp[[20]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.4 & sil[,3] <= 0.6))
temp[[21]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] <= 0.4))


sample(subset.matrix(sil, sil[,1] == 1 & sil[,3] <= 0.2))

uncertaintylist <- lapply(temp, function(tp){
  cars.temp <- cars.noNA[tp,]
  uncertaintyFunc1(cars.temp)
})

#将不确定性的list转成dataframe，便于作图
cars.uncertainty.dataframe <- as.data.frame(do.call(rbind, uncertaintylist))

#使用pcalg包
library("pcalg")
stopifnot(require(Rgraphviz))# needed for all our graph plots
suffStatOfCars <- list(C = cor(cars.uncertainty.dataframe), n = nrow(cars))
pc.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, p = ncol(cars.uncertainty.dataframe),alpha = 0.01)
pc.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, labels = colnames(cars.uncertainty.dataframe), alpha = 0.01)
plot(pc.uncertainty, main = "")

pc.uncertainty.allDags <- pdag2allDags(as(pc.uncertainty, "amat"))
plotAllDags(pc.uncertainty.allDags)


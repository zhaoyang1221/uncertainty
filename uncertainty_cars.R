cars <- read.csv("cars.csv")
cars <- cars[,-1]
cars.noNA <-na.omit(cars)

library(fpc)
library(cluster)
cars.diss <- daisy(cars.noNA)
cars.pamk <- pamk(cars.noNA)
cars.pam <- pam(cars.diss, cars.pamk$nc, diss=TRUE)
clusplot(cars.pam)
sil <- silhouette(cars.pam)
windows()
plot(sil)


#计算每一维度上的不确定性(以标准差来衡量)
uncertaintyFunc1 <- function(data){
  apply(data, 2, sd)
}


library(entropy)
uncertaintyFunc2 <- function(data){
  apply(data, 2, function(x) entropy.ChaoShen(x, unit = "log2"))
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

uncertaintylist <- lapply(temp, function(tp){
  cars.temp <- cars.noNA[tp,]
  uncertaintyFunc1(cars.temp)
})

#将不确定性的list转成dataframe，便于作图
cars.uncertainty.dataframe <- as.data.frame(do.call(rbind, uncertaintylist))


library("pcalg")
stopifnot(require(Rgraphviz))# needed for all our graph plots
suffStatOfCars <- list(C = cor(cars.uncertainty.dataframe), n = nrow(cars.uncertainty.dataframe))
pc.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, p = ncol(cars.uncertainty.dataframe),alpha = 0.01)
pc.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, labels = colnames(cars.uncertainty.dataframe), alpha = 0.01)
plot(pc.uncertainty, main = "")

pc.uncertainty.allDags <- pdag2allDags(as(pc.uncertainty, "amat"))
plotAllDags(pc.uncertainty.allDags)

#数据归一化
cars.scale <- scale(cars.noNA, center = TRUE, scale = TRUE)
cars.scale.diss <- daisy(cars.scale)
cars.scale.pamk <- pamk(cars.scale)
cars.scale.pam <- pam(cars.scale, cars.scale.pamk$nc, diss = FALSE)
clusplot(cars.scale.pamk$pamobject)
sil.scale <- silhouette(cars.scale.pam)
plot(sil.scale)

tempScale <- NULL
tempScale[[1]] <- rownames(sil.scale)
tempScale[[2]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1))
tempScale[[3]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2))
tempScale[[4]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1 & sil.scale[,3] > 0.66))
tempScale[[5]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1 & sil.scale[,3] > 0.64 & sil.scale[,3] <= 0.66))
tempScale[[6]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1 & sil.scale[,3] > 0.61 & sil.scale[,3] <= 0.64))
tempScale[[7]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1 & sil.scale[,3] > 0.57 & sil.scale[,3] <= 0.61))
tempScale[[8]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1 & sil.scale[,3] > 0.52 & sil.scale[,3] <= 0.57))
tempScale[[9]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1 & sil.scale[,3] > 0.40 & sil.scale[,3] <= 0.52))
tempScale[[10]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1 & sil.scale[,3] > 0 & sil.scale[,3] <= 0.40))

tempScale[[11]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.61))
tempScale[[12]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.59 & sil.scale[,3] <= 0.61))
tempScale[[13]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.57 & sil.scale[,3] <= 0.59))
tempScale[[14]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.55 & sil.scale[,3] <= 0.57))
tempScale[[15]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.52 & sil.scale[,3] <= 0.55))
tempScale[[16]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.49 & sil.scale[,3] <= 0.52))
tempScale[[17]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.45 & sil.scale[,3] <= 0.49))
tempScale[[18]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.40 & sil.scale[,3] <= 0.45))
tempScale[[19]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.29 & sil.scale[,3] <= 0.40))
tempScale[[20]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.20 & sil.scale[,3] <= 0.29))
tempScale[[21]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] > 0.1 & sil.scale[,3] <= 0.20))
tempScale[[22]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2 & sil.scale[,3] <= 0.1))

scale.uncertaintylist <- lapply(tempScale, function(tp){
  cars.temp <- cars.scale[tp,]
  uncertaintyFunc1(cars.temp)
})

#将不确定性的list转成dataframe，便于作图
cars.scale.uncertainty.dataframe <- as.data.frame(do.call(rbind, scale.uncertaintylist))

library("pcalg")
stopifnot(require(Rgraphviz))# needed for all our graph plots
suffStatOfCars <- list(C = cor(cars.scale.uncertainty.dataframe), n = nrow(cars.scale.uncertainty.dataframe))
pc.scale.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, p = ncol(cars.scale.uncertainty.dataframe),alpha = 0.01)
pc.scale.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, labels = colnames(cars.scale.uncertainty.dataframe), alpha = 0.2)
plot(pc.scale.uncertainty, main = "")

pc.uncertainty.allDags <- pdag2allDags(as(pc.scale.uncertainty, "amat"))
plotAllDags(pc.uncertainty.allDags)
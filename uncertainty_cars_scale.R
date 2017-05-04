cars <- read.csv("cars.csv")
cars <- cars[,-1]
cars.noNA <-na.omit(cars)

library(fpc)
library(cluster)
library(ggfortify)

#数据归一化
cars.scale <- scale(cars.noNA, center = TRUE, scale = TRUE)
cars.scale.diss <- daisy(cars.scale)
cars.scale.pamk <- pamk(cars.scale)
cars.scale.pam <- pam(cars.scale.diss, cars.scale.pamk$nc, diss = TRUE)
autoplot(cars.scale.pamk$pamobject, frame = TRUE, frame.type = "norm")
clusplot(cars.scale.pam)
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

#更改抽样方法,每7行一组
tempScale1 <- NULL
tempScale1[[1]] <- rownames(sil.scale)
tempScale1[[2]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1))
tempScale1[[3]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2))
rownamesOfSil <- rownames(sil.scale)
matrixOfSil <- matrix(rownamesOfSil, ncol = 7, byrow = TRUE)
tempScale2 <- split(matrixOfSil, 1:nrow(matrixOfSil))
tempScale <- append(tempScale1, tempScale2)

scale.uncertaintylist <- lapply(tempScale, function(tp){
  cars.temp <- cars.noNA[tp,]
  uncertaintyFunc2(cars.temp)
})

#将不确定性的list转成dataframe，便于作图
cars.scale.uncertainty.dataframe <- as.data.frame(do.call(rbind, scale.uncertaintylist))




library("pcalg")
stopifnot(require(Rgraphviz))# needed for all our graph plots
suffStatOfCars <- list(C = cor(cars.scale.uncertainty.dataframe), n = nrow(cars.scale.uncertainty.dataframe))
#pc.scale.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, p = ncol(cars.scale.uncertainty.dataframe),alpha = 0.01)
pc.scale.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, labels = colnames(cars.scale.uncertainty.dataframe), alpha = 0.05)
plot(pc.scale.uncertainty, main = "")

pc.uncertainty.allDags <- pdag2allDags(as(pc.scale.uncertainty, "amat"))
plotAllDags(pc.uncertainty.allDags)
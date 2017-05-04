require(MASS)
boston <- Boston
library(fpc)
library(cluster)

boston.scale <- scale(boston, center = TRUE, scale = TRUE)

boston.diss <- daisy(boston)
boston.pamk <- pamk(boston)
boston.pam <- pam(boston.diss, boston.pamk$nc, diss=TRUE)
windows()
clusplot(boston.pam)
sil <- silhouette(boston.pam)

plot(sil)

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
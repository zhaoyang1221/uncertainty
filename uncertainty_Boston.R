require(MASS)
data(Boston)
library(fpc)
library(cluster)
library(ggfortify)

Boston.scale <- scale(Boston, center = TRUE, scale = TRUE)

Boston.diss <- daisy(Boston)
Boston.pamk <- pamk(Boston.scale)
Boston.pam <- pam(Boston.diss, Boston.pamk$nc, diss=TRUE)
windows()
autoplot(Boston.pamk$pamobject, frame = TRUE, frame.type = "norm")
clusplot(Boston.pam)
sil <- silhouette(Boston.pamk$pamobject)

plot(sil)

#通过聚类结果构建不确定性数据集
temp <- NULL
temp[[1]] <- rownames(sil)
temp[[2]] <- rownames(subset.matrix(sil, sil[,1] == 1))
temp[[3]] <- rownames(subset.matrix(sil, sil[,1] == 2))
temp[[4]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.5))
temp[[5]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.489 & sil[,3] <= 0.5))
temp[[6]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.48 & sil[,3] <= 0.489))
temp[[7]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.473 & sil[,3] <= 0.48))
temp[[8]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.467 & sil[,3] <= 0.473))
temp[[9]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.45 & sil[,3] <= 0.467))
temp[[10]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.43 & sil[,3] <= 0.45))
temp[[11]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.4 & sil[,3] <= 0.43))
temp[[12]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.35 & sil[,3] <= 0.4))
temp[[13]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.3 & sil[,3] <= 0.35))
temp[[14]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.25 & sil[,3] <= 0.3))
temp[[15]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.2 & sil[,3] <= 0.25))
temp[[16]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.1 & sil[,3] <= 0.2))
temp[[17]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] <= 0.1))
temp[[18]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.43))
temp[[19]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.4 & sil[,3] <= 0.43))
temp[[20]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.36 & sil[,3] <= 0.4))
temp[[21]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.3 & sil[,3] <= 0.36))
temp[[22]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.2 & sil[,3] <= 0.3))
temp[[23]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.15 & sil[,3] <= 0.2))
temp[[24]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.1 & sil[,3] <= 0.2))
temp[[25]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] <= 0.1))

#更改分组方式
temp1 <- NULL
temp1[[1]] <- rownames(sil)
temp1[[2]] <- rownames(subset.matrix(sil, sil[,1] == 1))
temp1[[3]] <- rownames(subset.matrix(sil, sil[,1] == 2))
temp1[[4]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.13))
temp1[[5]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.2))
temp1[[6]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.26))
temp1[[7]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.31))
temp1[[8]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.35))
temp1[[9]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.4))
temp1[[10]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.42))
temp1[[11]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.43))
temp1[[12]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.45))
temp1[[13]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.467))
temp1[[14]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.473))
temp1[[15]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.48))
temp1[[16]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.489))
temp1[[17]] <- rownames(subset.matrix(sil, sil[,1] == 1 & sil[,3] > 0.5))

temp1[[18]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.13))
temp1[[19]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.2))
temp1[[20]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.3))
temp1[[21]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.36))
temp1[[22]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.4))
temp1[[23]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.43))



temp[[18]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.43))
temp[[19]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.4 & sil[,3] <= 0.43))
temp[[20]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.36 & sil[,3] <= 0.4))
temp[[21]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.3 & sil[,3] <= 0.36))
temp[[22]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.2 & sil[,3] <= 0.3))
temp[[23]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.15 & sil[,3] <= 0.2))
temp[[24]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] > 0.1 & sil[,3] <= 0.2))
temp[[25]] <- rownames(subset.matrix(sil, sil[,1] == 2 & sil[,3] <= 0.1))

#第二种不确定性计算方式（以信息熵来衡量）
library(entropy)
uncertaintyFunc2 <- function(data){
  apply(data, 2, function(x){
    tmp <- table(x)
    entropy.empirical(tmp, unit = "log2")
  })
}

#计算不确定性
Boston.uncertaintylist <- lapply(temp1, function(tp){
  Boston.temp <- Boston[tp,]
  uncertaintyFunc2(Boston.temp)
})

#将不确定性的list转成dataframe，便于作图
Boston.uncertainty.dataframe <- as.data.frame(do.call(rbind, Boston.uncertaintylist))

#正态性检验
Boston.SWtest <- apply(Boston.uncertainty.dataframe, 2, shapiro.test)

shapiro.test(Boston.uncertainty.dataframe$dis)

cars <- read.csv("cars.csv")
cars <- cars[,-1]
cars.noNA <-na.omit(cars)

library(fpc)
library(cluster)

library(ggfortify)
library(ggplot2)
windowsFonts(myFont = windowsFont("方正书宋简体"))
windowsFonts(Times = windowsFont("Times New Roman"))

#数据归一化
cars.scale <- scale(cars.noNA, center = TRUE, scale = TRUE)
cars.scale.diss <- daisy(cars.scale)

#依据轮廓系数来选取最佳的聚类个数
K <-2:8
rst <- sapply(K, function(i){
  print(paste("K=", i))
  clusters <- pam(cars.scale, i)
  result <- silhouette(clusters)
  sumryOfresult <- summary(result)
  sumryOfresult$avg.width
})

ggplot(NULL, aes(x= K, y = rst)) + geom_point(shape = 21, size = 4, fill = "black", colour = "black") + geom_line() + ylab("轮廓系数")  + theme(axis.title.x = element_text(family = "Times",size=18, face="italic")
                                                                                                                                            ,axis.text.x = element_text(family = "Times",size = 18,margin=unit(c(0.2,0.2,0.1,0.1), "cm"))
                                                                                                                                            ,axis.text.y = element_text(family = "Times",size = 18,margin=unit(c(0.2,0.2,0.2,0.1), "cm"))
                                                                                                                                            , axis.title.y = element_text(family = "myFont",size=18)
                                                                                                                                            ,axis.ticks.length=unit(-0.1, "cm"))
#聚类
library(fpc)
library(factoextra)
library(ggfortify)
library(ggplot2)
cars.scale.pamk <- pamk(cars.scale)
cars.scale.pam <- pam(cars.scale, cars.scale.pamk$nc)

autoplot(cars.scale.pamk$pamobject, frame = TRUE, frame.type = "norm")+theme(axis.title = element_text(family = "myFont",size=18),axis.text.x = element_text(family = "Times",size = 18,margin=unit(c(0.2,0.2,0.1,0.1), "cm")),axis.text.y = element_text(family = "Times",size = 18,margin=unit(c(0.2,0.2,0.2,0.1), "cm")),legend.position='none',axis.ticks.length=unit(-0.1, "cm"), plot.margin = margin(0.2,0.5,0.2,0.2,"cm"))


library(factoextra)
clusplot(cars.scale.pamk$pamobject)
sil.scale <- silhouette(cars.scale.pamk$pamobject)

fviz_silhouette(sil.scale, label = F) +ylab("轮廓系数") + ggtitle("轮廓系数分布图\n  平均轮廓系数:0.47") + theme(plot.title=element_text(family = "myFont",size=18),axis.title = element_text(family = "myFont",size=18),axis.text.y = element_text(family = "Times",size = 18,margin=unit(c(0.2,0.2,0.2,0.1), "cm")),legend.position='none',axis.ticks.length=unit(-0.15, "cm"))
plot(sil.scale,main = "Silhouette plot")


#计算每一维度上的不确定性(以标准差来衡量)
uncertaintyFunc1 <- function(data){
  apply(data, 2, sd)
}

#第二种不确定性计算方式（以信息熵来衡量）
library(entropy)
uncertaintyFunc2 <- function(data){
  apply(data, 2, function(x){
    tmp <- table(x)
    entropy.empirical(tmp, unit = "log2")
  })
}

#按值抽样 
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

#更改抽样方法,每14行一组
tempScale1 <- NULL
tempScale1[[1]] <- rownames(sil.scale)
tempScale1[[2]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 1))
tempScale1[[3]] <- rownames(subset.matrix(sil.scale, sil.scale[,1] == 2))
rownamesOfSil <- rownames(sil.scale)
matrixOfSil <- matrix(rownamesOfSil, ncol = 14, byrow = TRUE)
tempScale2 <- split(matrixOfSil, 1:nrow(matrixOfSil))
tempScale <- append(tempScale1, tempScale2)

#直接对数据中的某一列进行排序然后分组
cars.order <- cars.noNA[order(cars.noNA[,4], decreasing = TRUE),]
tempScale3<- NULL
rownamesOfOrdered <- rownames(cars.order)
matrixOfOrdered <- matrix(rownamesOfOrdered, ncol = 14, byrow = TRUE)
tempScale3 <- split(matrixOfOrdered, 1:nrow(matrixOfOrdered))

#计算不确定性
scale.uncertaintylist <- lapply(tempScale3, function(tp){
  cars.temp <- cars.noNA[tp,]
  uncertaintyFunc2(cars.temp)
})

#将不确定性的list转成dataframe，便于作图
cars.scale.uncertainty.dataframe <- as.data.frame(do.call(rbind, scale.uncertaintylist))

#正态检验
SWtest <- apply(cars.scale.uncertainty.dataframe, 2, shapiro.test)

shapiro.test(cars.scale.uncertainty.dataframe$economy)

qqnorm(cars.scale.uncertainty.dataframe$economy, main = "", xlab = "理论分位数",ylab="样本分位数") 

ggplot(cars.scale.uncertainty.dataframe, aes(sample=economy))+stat_qq()+xlab("理论分位数")+ylab("样本分位数")+theme(axis.title = element_text(family = "myFont",size=18),axis.text.x = element_text(family = "Times",size = 18,margin=unit(c(0.2,0.2,0.1,0.1), "cm")),axis.text.y = element_text(family = "Times",size = 18,margin=unit(c(0.2,0.2,0.2,0.1), "cm")),axis.ticks.length=unit(-0.15, "cm"))

qqline(cars.scale.uncertainty.dataframe$cylinders)
hist(cars.scale.uncertainty.dataframe$displacement)

#相关分析
library("Hmisc")
corelationResult <- rcorr(as.matrix(cars.scale.uncertainty.dataframe))

#数据重组
flattenCorrMatrixWithUpper <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    r  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrixWithAll <- function(cormat, pmat) {
  ut <- xor(upper.tri(cormat, diag = T), lower.tri(cormat))
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    r  =(cormat)[ut],
    p = pmat[ut]
  )
}
corrMatrix <- flattenCorrMatrixWithAll(corelationResult$r,corelationResult$P)
ggplot(corrMatrix) + geom_point(aes(x = r, y = p))


library(corrplot)
par(mfrow=c(1,2))
cars_corrplot <- corrplot(corelationResult$r, method = "circle", type = "upper", order = "FPC", tl.cex = 2,cl.cex = 2,cl.offset = 0.5, p.mat = corelationResult$P, sig.level = 0.05, insig = "blank")

#原数据的相关分析
library("Hmisc")
library("corrplot")
corrResultOfOriginalDATA <- rcorr(as.matrix(cars.noNA))
corrMatrixOfOriginalDATA <- flattenCorrMatrixWithAll(corrResultOfOriginalDATA$r, corrResultOfOriginalDATA$P)
corrplot(corrResultOfOriginalDATA$r, method = "circle", type = "upper", order = "FPC",tl.cex = 2.5,cl.cex = 2.5,cl.offset = 0,
         p.mat = corrResultOfOriginalDATA$P, sig.level = 0.05, insig = "blank")

flattenCorrMatrixWithAll <- function(cormat, pmat) {
  ut <- xor(upper.tri(cormat, diag = T), lower.tri(cormat))
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    r  =(cormat)[ut],
    p = pmat[ut]
  )
}

#因果分析
library("pcalg")
stopifnot(require(Rgraphviz))# needed for all our graph plots
suffStatOfCars <- list(C = cor(cars.scale.uncertainty.dataframe), n = nrow(cars.scale.uncertainty.dataframe))
#pc.scale.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, p = ncol(cars.scale.uncertainty.dataframe),alpha = 0.01)
pc.scale.uncertainty <- pc(suffStatOfCars, indepTest = gaussCItest, labels = colnames(cars.scale.uncertainty.dataframe), alpha = 0.05)
plot(pc.scale.uncertainty, main = "")

pc.uncertainty.allDags <- pdag2allDags(as(pc.scale.uncertainty, "amat"))
plotAllDags(pc.uncertainty.allDags)
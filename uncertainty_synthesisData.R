#合成数据集
n <- 1000#数据量
x <- rnorm(n, mean = 5, sd = 10)
e <- rnorm(n, mean = 0, sd = 5)#随机扰动
y1 <- 2*x + 1 + e
y2 <- x^2 + e
y3 <- rnorm(n, mean = 5, sd = 10)

SynData <- data.frame(x, y1, y2, y3)
library(ggplot2)
ggplot(SynData, aes(x, y1)) + geom_point()


#聚类
library(fpc)
library(cluster)
library(ggfortify)
SynData.pamk <- pamk(SynData)
autoplot(SynData.pamk$pamobject, frame = TRUE, frame.type = "norm")
plot(SynData.pamk$pamobject)
SynData.sil <- silhouette(SynData.pamk$pamobject)
plot(SynData.sil)

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

#分组
SynData.rownamesOfSil <- rownames(SynData.sil)
SynData.matrixOfSil <- matrix(SynData.rownamesOfSil, ncol = 20, byrow = TRUE)
SynData.tempScale <- split(SynData.matrixOfSil, 1:nrow(SynData.matrixOfSil))

#计算
SynData.uncertaintylist <- lapply(SynData.tempScale, function(tp){
  SynData.temp <- SynData[tp,]
  uncertaintyFunc1(SynData.temp)
})

#将不确定性的list转成dataframe，便于作图
SynData.uncertainty.dataframe <- as.data.frame(do.call(rbind, SynData.uncertaintylist))

#正态检验
shapiro.test(SynData.uncertainty.dataframe$y2)
qqnorm(SynData.uncertainty.dataframe$y2)
qqline(SynData.uncertainty.dataframe$y2)
hist(SynData.uncertainty.dataframe$y1)

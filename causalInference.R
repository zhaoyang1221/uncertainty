
cars <- read.csv("cars.csv")
cars <- cars[,-1]
cars.noNA <-na.omit(cars)

#使用pcalg包
library("pcalg")
stopifnot(require(Rgraphviz))# needed for all our graph plots
suffStatOfCars <- list(C = cor(cars.noNA), n = nrow(cars.noNA))
pc.cars <- pc(suffStatOfCars, indepTest = gaussCItest, p = ncol(cars.noNA),alpha = 0.01)
pc.cars <- pc(suffStatOfCars, indepTest = gaussCItest, labels = colnames(cars.noNA), alpha = 0.01)
plot(pc.cars, main = "")
ida(2,3, cov(cars.noNA), pc.cars@graph, method = "global")
ida(6,4, cov(cars.noNA), pc.cars@graph)

pc.cars.allDags <- pdag2allDags(as(pc.cars, "amat"))
plotAllDags(pc.cars.allDags)

#plot出所有dag图，参数是amatType的邻接矩阵
plotAllDags <- function(res) {
  require(graph)
  p <- sqrt(ncol(res$dags))
  nDags <- ceiling(sqrt(nrow(res$dags)))
  par(mfrow = c(nDags, nDags))
  for (i in 1:nrow(res$dags)) {
    tmp <- matrix(res$dags[i,],p,p)
    colnames(tmp) <- rownames(tmp) <- res$nodeNms
    plot(as(tmp, "graphNEL"))
  }
}


#第五个dag最符合实际
trueDAG <- trueDAGFunc(pc.cars.allDags, 5)
plot(trueDAG)

#取出指定的dag
trueDAGFunc <- function(res, index) {
  require(graph)
  p <- sqrt(ncol(res$dags))
  tmp <- matrix(res$dags[index,],p,p)
  colnames(tmp) <- rownames(tmp) <- res$nodeNms
  return(as(tmp, "graphNEL"))
}

#计算影响因子
causalEffect(trueDAG, 4,2)
ida(2,3, cov(cars.noNA), trueDAG)
ida(2,5, cov(cars.noNA), trueDAG)
idaFast(2, c(3,4,5),cov(cars.noNA),trueDAG)

#维度之间的散点图，查看相关关系
require("ggplot2")
ggplot(cars.noNA, aes(x = cylinders, y = displacement))+ geom_point() + geom_smooth()
ggplot(cars.noNA, aes(x = displacement, y = power)) + geom_point() + geom_smooth()
ggplot(cars.noNA, aes(x = power, y = timeTo60mph)) + geom_point() + geom_smooth()
ggplot(cars.noNA, aes(x = displacement, y = weight)) + geom_point() + geom_smooth()
ggplot(cars.noNA, aes(x = weight, y = economy)) + geom_point() + geom_smooth()
ggplot(cars.noNA, aes(x = year, y = economy)) + geom_point() + geom_smooth()


  
  

#学习pcalg包
library("pcalg")
data("gmG")

suffStat <- list(C = cor(gmG8$x), n = nrow(gmG8$x))
pc.gmG <- pc(suffStat, indepTest = gaussCItest, p = ncol(gmG8$x), alpha = 0.01)
stopifnot(require(Rgraphviz))# needed for all our graph plots
par(mfrow = c(1,2))
plot(gmG8$g, main = "") ; plot(pc.gmG, main = "")

#causal effect
ida(1, 6, cov(gmG8$x), pc.gmG@graph)
idaFast(1, c(4,5,6), cov(gmG8$x), pc.gmG@graph)

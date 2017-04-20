
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


#按行进行分组，分别对每组数据计算均值来构建新的数据集（chunk是分组行数）
constructNewCarsFunc <- function(data, chunk){
  #对数据集按行数进行分组
  n <- nrow(data)
  r <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
  d <- split(data, r)
  
  #对每一个分组进行计算(这里是均值)
  meanOfDatalist <- lapply(d, function(data){
    apply(data, 2, mean)
  })
  result <- as.data.frame(do.call(rbind, meanOfDatalist))
  return(result)
}

#计算每一维度上的不确定性
uncertaintyFunc1 <- function(data){
  apply(data, 2, sd)
}

cars.uncertainty <- NULL
for (i in 1:10) {
  temp <- constructNewCarsFunc(cars.noNA, i)
  cars.uncertainty[[i]] <- uncertaintyFunc1(temp)
}

#将不确定性的list转成dataframe，便于作图
cars.uncertainty.dataframe <- as.data.frame(do.call(rbind, cars.uncertainty))

#根据具有因果关系的维度画散点图
require("ggplot2")
ggplot(cars.uncertainty.dataframe, aes(x = cylinders, y = displacement))+ geom_point() + geom_smooth()
ggplot(cars.uncertainty.dataframe, aes(x = displacement, y = power)) + geom_point() + geom_smooth()
ggplot(cars.uncertainty.dataframe, aes(x = power, y = timeTo60mph)) + geom_point() + geom_smooth()
ggplot(cars.uncertainty.dataframe, aes(x = displacement, y = weight)) + geom_point() + geom_smooth()
ggplot(cars.uncertainty.dataframe, aes(x = weight, y = economy)) + geom_point() + geom_smooth()
ggplot(cars.uncertainty.dataframe, aes(x = year, y = economy)) + geom_point() + geom_smooth()

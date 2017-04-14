food <- read.csv("food.csv")
row.names(food) <- food[,1]
food <- food[,-1]

library(subspace)
foodClusters <- CLIQUE(food,xi=40,tau=0.15)

foodClusters[[1]]$subspace
foodClusters[[1]]$objects

#子空间mds图
library(ggplot2)
require(cowplot)
plots <- NULL
for (i in 1:20) {
  subspaceOfFood <- food[foodClusters[[i]]$objects,]
  d <- dist(subspaceOfFood)
  fit <- cmdscale(d, k=2, eig = TRUE)
  x <- fit$points[,1]
  y <- fit$points[,2]
  
  plots[[i]] <- ggplot(data.frame(x,y), aes(x,y)) + geom_point(size=1 ,color = "blue") + xlab("") + ylab("");
}
plot_grid(plotlist = plots)

#子空间不确定性
uncertaintyList <- lapply(foodClusters, function(cluster){
  subOfFood <- food[, cluster$subspace]
  if (typeof(subOfFood) == "list") {
    multiDimensionUncertaintyFun(subOfFood)
  } else {
    OneDimensionUncertaintyFun(subOfFood)
  }
})

#只有一个维度的子空间，利用标准差作为不确定性
OneDimensionUncertaintyFun <- function(x) {
  sd(x)
}

#多维子空间不确定性计算
multiDimensionUncertaintyFun <- function(x) {
  covOfFood <- cov(x)
  eigenValues <- eigen(covOfFood, only.values = TRUE)$values
  d <- ncol(x) / 2
  result <- pi^d / gamma(d+1) * prod(sqrt(eigenValues), na.rm = TRUE)
  return(result)
}

uncertaintyFrame <- data.frame(index = c(1:length(uncertaintyList)), uncertainty = as.double(uncertaintyList))
ggplot(uncertaintyFrame) + geom_col()



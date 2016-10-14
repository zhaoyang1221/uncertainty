#读取数据全部的csv文件名
filenames <- list.files("datasets", pattern = ".csv")

#得到球员数据集,并保留只需要的维度
myvars <- c("G", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")
datalist <- lapply(filenames, function(name){
  out <- read.csv(paste("datasets/", name, sep = ""))
  out <- out[myvars]
})

#球员姓名列表
playernames <- lapply(filenames, function(name){
  strsplit(name, "\\.")[[1]][1]
})
#为球员数据集命名
names(datalist) = playernames

#计算每一维度上的标准差
sdOfDatalist <- lapply(datalist, function(data){
  apply(data, 2, sd)
})

#第一种不确定性计算公式（每一维度的标准差之和），
#参数为每个维度的标准差
uncertaintyFunc1 <- function(sdOfDatalist){
  result <- lapply(sdOfDatalist, function(sdOfPlayer){
    sum(sdOfPlayer)
  })
}

#计算第一种不确定性
uncertaintyList1<- uncertaintyFunc1(sdOfDatalist)

#计算协方差矩阵
covMatrixList <- lapply(datalist, function(data){
  cov(data)
})

#计算协方差矩阵的特征值
evList <- lapply(covMatrixList, function(covMatrix){
  eigen(covMatrix, only.values = TRUE)$values
})

#第二种不确定性计算方程，
#参数为单个球员协方差矩阵的特征值
uncertaintyFunc2 <- function(x){
  d <- length(myvars) / 2
  result <- pi^d / gamma(d+1) * prod(sqrt(x), na.rm = TRUE)
  return(result)
}

uncertaintyList2 <- lapply(evList, function(ev){
  uncertaintyFunc2(ev)
})



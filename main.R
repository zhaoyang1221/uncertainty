#读取数据
Kobe <- read.csv("data/Kobe Bryant.csv")
LeBron <- read.csv("data/LeBron James.csv")
Curry <- read.csv("data/Stephen Curry.csv")

#数据预处理,保留需要的维度
myvars <- c("G", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")
Kobe <- Kobe[myvars]
LeBron <- LeBron[myvars]

#计算每一维度上的标准差
sdOfKobe <- apply(Kobe, 2, sd)
sdOfLeBron <- apply(LeBron, 2, sd)

#第一种不确定性计算公式（每一维度的标准差之和），
#参数为每个维度的标准差
uncertaintyFunc1 <- function(x){
  return(sum(x))
}

#计算第一种不确定性
uncertaintyOfKobe <- uncertaintyFunc1(sdOfKobe)
uncertaintyOfLeBron <- uncertaintyFunc1(sdOfLeBron)

#计算协方差矩阵
covOfKobe <- cov(Kobe)
covOfLeBron <- cov(LeBron)

#计算协方差矩阵的特征值
evOfKobe <- eigen(covOfKobe, only.values = TRUE)$values
evOfLeBron <- eigen(covOfLeBron, only.values = TRUE)$values

#第二种不确定性计算方程，
#参数为协方差矩阵的特征值
uncertaintyFunc2 <- function(x){
  d <- length(myvars) / 2
  result <- pi^d / gamma(d+1) * prod(sqrt(x), na.rm = TRUE)
  return(result)
}

uncertaintyOfKobe2 <- uncertaintyFunc2(evOfKobe)
uncertaintyOfLeBron2 <- uncertaintyFunc2(evOfLeBron)

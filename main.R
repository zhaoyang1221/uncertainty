#设置当前目录为工作目录
setwd("E:/visualization/workspace/test")
#读取数据
Kobe <- read.csv("Kobe Bryant.csv")
LeBron <- read.csv("LeBron James.csv")

#数据预处理
myvars <- c("G", "MP", "FG", "FGA", "X3P", "X3PA", "FT", "FTA", "ORB", "DRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")

Kobe <- Kobe[myvars]
LeBron <- LeBron[myvars]

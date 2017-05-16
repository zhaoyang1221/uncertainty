#合成数据集
n <- 100#数据量
x <- rnorm(n, mean = 5, sd = 5)
e <- rnorm(n, mean = 0, sd = 1)#随机扰动
y1 <- 2*x + 1 + e
y2 <- x^2 + e
y3 <- rnorm(n, mean = 5, sd = 5)

SynthesisData <- data.frame(x, y1, y2, y3)
library(ggplot2)
ggplot(SynthesisData, aes(x, y2)) + geom_point()

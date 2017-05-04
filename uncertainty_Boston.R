require(MASS)

library(fpc)
library(cluster)
data("Boston")
Boston.scale <- scale(Boston)
Boston.pamk <- pamk(Boston.scale)
Boston.pam <- pam(Boston.diss, Boston.pamk$nc, diss = TRUE)
autoplot(Boston.pamk$pamobject, frame = TRUE, frame.type = "norm")
Boston.sil <- silhouette(Boston.pamk$pamobject)
plot(Boston.sil)
summary(Boston)



#将聚类数据转成json
library(jsonlite)
library(plyr)
cars.scale.pamk.plotJson <- toJSON(cars.scale.pamk.plot$data, dataframe = "rows", pretty = T, factor = "integer")
cat(cars.scale.pamk.plotJson)
writeLines(cars.scale.pamk.plotJson, "json/cars_clustering_scatter.json")

#打印sil数据

cars.sil <- cars.scale.pamk$pamobject$silinfo$widths
cars.sil <- as.data.frame(cars.sil)
cars.sil$node_id <- as.numeric(rownames(cars.sil))

clusAvg <- data.frame(cluster = c(1,2), clusSilAvg = cars.scale.pamk$pamobject$silinfo$clus.avg.widths )
cars.sil_info <- list(silInfo = cars.sil, clusAvg = clusAvg, silAvg = as.numeric(cars.scale.pamk$pamobject$silinfo$avg.width))
sil.scale.json <- toJSON(cars.sil_info, pretty = T)
writeLines(sil.scale.json, "json/cars_sil.json")
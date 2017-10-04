#计算cars.naNA的基本统计特性
Boston.mean <- sapply(Boston, mean)
Boston.sd <- sapply(Boston, sd)
Boston.min <- sapply(Boston, min)
Boston.max <- sapply(Boston, max)
Boston.median <- sapply(Boston, median)
Boston.uncertainty <- uncertaintyFunc2(Boston)
Boston.summary <- data.frame(mean=Boston.mean, sd=Boston.sd, min=Boston.min, max=Boston.max, median=Boston.median, uncertainty=Boston.uncertainty)

#打印数据
library(jsonlite)
writeLines(toJSON(Boston.summary, pretty = T), "json/Boston_summary.json")

#打印原数据和不确定性数据
writeLines(toJSON(Boston, pretty = T), "json/Boston.json")

#打印不确定性数据的相关关系数据
writeLines(toJSON(Boston.corrMatrix, pretty = T, na = "null"), "json/Boston_uncertainty_correlation.json")

#打印原数据的相关关系
writeLines(toJSON(Boston.corrMatrixOfOriginalDATA, pretty = T, na = "null"), "json/Boston_original_correlation.json")

#打印原始数据和不确定性数据的结合数据
writeLines(toJSON(list(original = Boston, uncertainty = Boston.uncertainty.dataframe), pretty = T), "json/Boston_original_with_uncertainty.json")
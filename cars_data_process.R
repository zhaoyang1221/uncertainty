#计算cars.naNA的基本统计特性
cars.noNA.mean <- sapply(cars.noNA, mean)
cars.noNA.sd <- sapply(cars.noNA, sd)
cars.noNA.min <- sapply(cars.noNA, min)
cars.noNA.max <- sapply(cars.noNA, max)
cars.noNA.median <- sapply(cars.noNA, median)
cars.noNA.uncertainty <- uncertaintyFunc2(cars.noNA)
cars.noNA.summary <- data.frame(mean=cars.noNA.mean, sd=cars.noNA.sd, min=cars.noNA.min, max=cars.noNA.max, median=cars.noNA.median, uncertainty=cars.noNA.uncertainty)

#打印数据
library(jsonlite)
writeLines(toJSON(cars.noNA.summary, pretty = T), "json/cars_summary.json")

#打印原数据和不确定性数据
writeLines(toJSON(cars.noNA, pretty = T), "json/cars.json")

#打印不确定性数据的相关关系数据
writeLines(toJSON(corrMatrix, pretty = T, na = "null"), "json/cars_uncertainty_correlation.json")

#打印原数据的相关关系
writeLines(toJSON(corrMatrixOfOriginalDATA, pretty = T, na = "null"), "json/cars_original_correlation.json")

#打印原始数据和不确定性数据的结合数据
writeLines(toJSON(list(original = cars.noNA, uncertainty = cars.scale.uncertainty.dataframe), pretty = T), "json/cars_original_with_uncertainty.json")

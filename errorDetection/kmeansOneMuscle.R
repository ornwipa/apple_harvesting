# require x (principal components); return anomaly

library(stats)
clusters <- kmeans(x[,1:3], centers = 4, iter.max = 10, nstart = 25)
clusters$size
k <- clusters$cluster
anomaly_kmeans <- 2-k
anomaly_kmeans[anomaly_kmeans == 0] <- NA

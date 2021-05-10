# require x (principal components); return anomaly

pc1 <- x[,1]
iqr1 <- as.numeric(quantile(pc1, probs = 0.75) - quantile(pc1, probs = 0.25))
lower_pc1 <- as.numeric(quantile(pc1, probs = 0.25)) - 1.5*iqr1
upper_pc1 <- as.numeric(quantile(pc1, probs = 0.75)) + 1.5*iqr1

pc2 <- x[,2]
iqr2 <- as.numeric(quantile(pc2, probs = 0.75) - quantile(pc2, probs = 0.25))
lower_pc2 <- as.numeric(quantile(pc2, probs = 0.25)) - 1.5*iqr2
upper_pc2 <- as.numeric(quantile(pc2, probs = 0.75)) + 1.5*iqr2

pc3 <- x[,3]
iqr3 <- as.numeric(quantile(pc3, probs = 0.75) - quantile(pc3, probs = 0.25))
lower_pc3 <- as.numeric(quantile(pc3, probs = 0.25)) - 1.5*iqr3
upper_pc3 <- as.numeric(quantile(pc3, probs = 0.75)) + 1.5*iqr3

anomaly_iqr <- ((x[,1]<lower_pc1) | (x[,1]>upper_pc1)) &
  ((x[,2]<lower_pc2) | (x[,2]>upper_pc2)) & ((x[,3]<lower_pc3) | (x[,3]>upper_pc3))
summary(anomaly_iqr)

anomaly_iqr[anomaly_iqr == 0] <- NA

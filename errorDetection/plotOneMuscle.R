# required: anomaly, tidy_data

par(mfrow=c(2,2))

plot(tidy_data$Mean, type = "l",
     xlab = "Time (s)", ylab = "Amplitude (µV)",
     main = "EMG Root-Mean-Square Amplitude")
anomaly_mean <- anomaly_iqr*tidy_data$Mean
points(anomaly_mean, col = "deepskyblue")
legend("topleft", col = "deepskyblue", pch = 1,
       legend = "anomalies as outliers of principal components")

plot(tidy_data$Mean, type = "l",
     xlab = "Time (s)", ylab = "Amplitude (µV)",
     main = "EMG Root-Mean-Square Amplitude")
anomaly_mean <- anomaly_kmeans*tidy_data$Mean
points(anomaly_mean, col = "darkorange")
legend("topleft", col = "darkorange", pch = 1,
       legend = "anomalies detected through k-means clustering")

plot(tidy_data$MeanPF, type = "l", 
     xlab = "Time (s)", ylab = "Frequency (Hz)",
     main = "EMG Mean Power Frequency By Second")
anomaly_meanPF <- anomaly_iqr*tidy_data$MeanPF
points(anomaly_meanPF, col = "deepskyblue")
legend("topleft", col = "deepskyblue", pch = 1,
       legend = "anomalies as outliers of principal components")

plot(tidy_data$MeanPF, type = "l", 
     xlab = "Time (s)", ylab = "Frequency (Hz)",
     main = "EMG Mean Power Frequency By Second")
anomaly_meanPF <- anomaly_kmeans*tidy_data$MeanPF
points(anomaly_meanPF, col = "darkorange")
legend("topleft", col = "darkorange", pch = 1,
       legend = "anomalies detected through k-means clustering")

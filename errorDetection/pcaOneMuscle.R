# require tidy_data; return x (PC1,...,PC5)

library(FactoMineR)
pca <- PCA(tidy_data[9:28], scale.unit = TRUE, graph = FALSE)

get_eigenvalue(pca)
pca$var$contrib

x <- pca$ind$coord

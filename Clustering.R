library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(fs) 

# Read the data
data_normalized_zscore <- read_csv("data_normalized_zscore.csv")

# Filter the data for labels 0 and 1
data <- subset(data, labels %in% c(0, 1))
label_colors <- c("blue", "red")
cols_of_interest <- colnames(data)[-c(1, 2, ncol(data))]
print(cols_of_interest)

###############################################################################
######################### Z-Score Normalization ###############################
###############################################################################

# Calculate Mahalanobis distance
data_normalized_zscore_mean <- colMeans(data_normalized_zscore)
data_normalized_zscore_cov <- cov(data_normalized_zscore)
data_normalized_zscore_mahalanobis_dist <- mahalanobis(data_normalized_zscore, data_normalized_zscore_mean, data_normalized_zscore_cov)

# Methods for distance calculation
distance_methods <- c("euclidean", "manhattan", "mahalanobis")

# Create a plot to display dendrograms for each distance calculation method and linkage method
par(mfrow = c(2, 4))  

for (distance_method in distance_methods) {
  if (distance_method == "mahalanobis") {
    distances <- as.dist(data_normalized_zscore_mahalanobis_dist)  # Use the correct Mahalanobis distances
  } else {
    distances <- dist(data_normalized_zscore, method = distance_method)
  }
  
  # Perform hierarchical clustering using different linkage methods
  linkage_methods <- c("single", "complete", "average", "ward.D2") # Different linkage methods
  
  for (linkage_method in linkage_methods) {
    hc <- hclust(distances, method = linkage_method)
    plot(hc, main = paste("Hierarchical Clustering - Z-Score Normalized Data", linkage_method, "(", distance_method, ")"),
         xlab = "Observations", sub = NULL)
  }
}

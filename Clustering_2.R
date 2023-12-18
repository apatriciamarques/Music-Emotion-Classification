# Load required libraries
library(caret)
library(dplyr)
library(cluster)
library(dendextend)

orig_data <- read_csv("278k_song_labelled.csv")
orig_data <- subset(data, labels %in% c(0, 1))
strata_var <- orig_data$labels
set.seed(123)
indices <- createDataPartition(strata_var, p = 0.05, list = FALSE)
data <- orig_data[indices, ]

#changing the label that is num to a 2 level factor
data$labels <- factor(data$labels, levels = c(0, 1))

cols_of_interest <- colnames(orig_data)[-c(1, length(orig_data))]

data_normalized = data[, -1]

################################
####### PRE-PROCESSING #########
################################


#NORMALIZAÇÃO 

min_data_normalized<- sapply(data[cols_of_interest], min)
max_data_normalized<- sapply(data[cols_of_interest], max)

scale_data_normalized = max_data_normalized - min_data_normalized

data_normalized[cols_of_interest] <- scale(data[cols_of_interest], center = min_data_normalized, scale = scale_data_normalized)

data_subset <- data_normalized


# Define distance methods and linkage methods
distance_methods <- c("euclidean", "manhattan")
linkage_methods <- c("single", "complete", "average", "ward.D")

# Initialize a data frame to store results
results_df <- data.frame(
  Distance_Method = character(),
  Linkage_Method = character(),
  Correct_Count = integer(),
  Incorrect_Count = integer(),
  stringsAsFactors = FALSE
)

# Perform hierarchical clustering and store results
for (distance_method in distance_methods) {
  for (linkage_method in linkage_methods) {
    # Calculate distances
    distances <- dist(data_subset[, -ncol(data_subset)], method = distance_method)
    distances[is.na(distances)] <- 0
    
    # Perform hierarchical clustering
    hc <- hclust(distances, method = linkage_method)
    
    # Cut the tree to obtain cluster labels
    cluster_labels <- cutree(hc, k = 2)
    
    # Create a confusion matrix
    cat("Distance Method:", distance_method, " | Linkage Method:", linkage_method, "\n")
    confusion_matrix <- table(cluster_labels, data_subset$labels)
    print(confusion_matrix)
    
  }
}


#########################################################################################
########################################## K-Means ######################################
#########################################################################################

data_subset_no_labels <- data_subset[, -which(names(data_subset) == "labels")]

# Perform unsupervised k-means clustering
km.res <- kmeans(data_subset_no_labels, 2, nstart = 25)

# Extract cluster labels
cluster_labels <- km.res$cluster

original_labels <- data_subset$labels

# Print the confusion matrix
confusion_matrix <- table(cluster_labels, original_labels)
print(confusion_matrix)

#########################################################################################
######################################## K-Medoids ######################################
#########################################################################################

# Remove the "labels" column for unsupervised k-medoids
data_subset_no_labels <- data_subset[, -which(names(data_subset) == "labels")]

# Perform unsupervised k-medoids clustering
medoid_res <- pam(data_subset_no_labels, k = 2)

# Extract cluster medoids
medoid_cluster_labels <- medoid_res$cluster

original_labels <- data_subset$labels

# Print the confusion matrix
confusion_matrix_medoids <- table(medoid_cluster_labels, original_labels)
print(confusion_matrix_medoids)



#########################################################################################
############## Dendogram for Ward's Method with EManhattan Distance ######################
#########################################################################################


# Calculate distances
distances <- dist(data_subset[, -1], method = "manhattan")
distances[is.na(distances)] <- 0

# Perform hierarchical clustering with Ward's Method
hc <- hclust(distances, method = "ward.D")

# Cut the tree to obtain cluster labels
cluster_labels <- cutree(hc, k = 2)

# Create a dendrogram
dend <- as.dendrogram(hc)

cluster_colors <- c("blue", "red")
names(cluster_colors) <- 1:2

dend_colored <- color_branches(dend, k = 2, col = cluster_colors)

par(mar = c(5, 4, 2, 1))  
plot(dend_colored, main = "Cluster Dendrogram")


#########################################################################################
######### Ward's Method with Manhattan Distance for other pre-processing datasets #######
#########################################################################################

#####################Z-score
# Read the data
data_normalized_zscore <- read.csv("data_normalized_zscore.csv")

# Sample a subset of your data
set.seed(123)
strata_var <- data_normalized_zscore$labels
indices <- createDataPartition(strata_var, p = 0.05, list = FALSE)
data_subset_zscore <- data_normalized_zscore[indices, ]

# Apply the trained model to the new dataset
distances_zscore <- dist(data_subset_zscore[, -1], method = "euclidean")
distances_zscore[is.na(distances_zscore)] <- 0

hc <- hclust(distances_zscore, method = "ward.D")

# Cut the tree to obtain cluster labels
cluster_labels_zscore <- cutree(hc, k = 2)

# Assuming new_data has true labels in a column named "labels"
# Create a confusion matrix for the new dataset
confusion_matrix_zscore <- table(cluster_labels, data_subset_zscore$labels)
confusion_matrix_zscore

#####################Robust Standardization
'''data_std_median <- read.csv("data_std_median.csv")

# Sample a subset of your data
set.seed(123)
strata_var <- data_std_median$labels
indices <- createDataPartition(strata_var, p = 0.005, list = FALSE)
data_subset_std <- data_std_median[indices, ]

# Apply the trained model to the new dataset
distances_std <- dist(data_subset_std[, -1], method = "manhattan")
distances_std[is.na(distances_std)] <- 0

hc <- hclust(distances_yeo, method = "ward.D")

# Cut the tree to obtain cluster labels
cluster_labels_std <- cutree(hc, k = 2)

# Assuming new_data has true labels in a column named "labels"
# Create a confusion matrix for the new dataset
confusion_matrix_std <- table(cluster_labels_std, data_subset_std$labels)
confusion_matrix_std
'''


#####################Yeo-Johnson
# Read the data
data_normalized_yeo <- read.csv("ddata_normalized_yeo_johnson.csv")

# Sample a subset of your data
set.seed(123)
strata_var <- data_normalized_yeo$labels
indices <- createDataPartition(strata_var, p = 0.05, list = FALSE)
data_subset_yeo <- data_normalized_yeo[indices, ]

# Apply the trained model to the new dataset
distances_yeo <- dist(data_subset_yeo[, -1], method = "euclidean")
distances_yeo[is.na(distances_yeo)] <- 0

hc <- hclust(distances_yeo, method = "ward.D")

# Cut the tree to obtain cluster labels
cluster_labels_yeo <- cutree(hc, k = 2)

# Assuming new_data has true labels in a column named "labels"
# Create a confusion matrix for the new dataset
confusion_matrix_yeo <- table(cluster_labels_yeo, data_subset_yeo$labels)
confusion_matrix_yeo


#####################Standardized PCA
# Read the data
data_normalized_pca <- read.csv("Stand_PCA.csv")
data_normalized_pca

# Sample a subset of your data
set.seed(123)
strata_var <- data_normalized_pca$labels
indices <- createDataPartition(strata_var, p = 0.05, list = FALSE)
data_subset_pca <- data_normalized_pca[indices, ]

# Apply the trained model to the new dataset
distances_pca <- dist(data_subset_pca[, -1], method = "euclidean")
distances_pca[is.na(distances_pca)] <- 0

hc <- hclust(distances_pca, method = "ward.D")

# Cut the tree to obtain cluster labels
cluster_labels_pca <- cutree(hc, k = 2)

# Assuming new_data has true labels in a column named "labels"
# Create a confusion matrix for the new dataset
confusion_matrix_pca <- table(cluster_labels_pca, data_subset_pca$labels)
confusion_matrix_pca




################Gráfico de clusters+Confusion matrix of min_max+PCA


# Perform PCA on the updated dataset (data without removed variables)
pca_result <- prcomp(data_normalized_min_max, scale = TRUE)

# Explained variance ratio for each principal component
pca_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Calculate the number of components that explain 90% of the variance
num_components_to_retain <- which(cumsum(pca_variance_ratio) >= 0.9)[1]

# Obtain the transformed data using the retained principal components
transformed_data <- pca_result$x[, 1:num_components_to_retain]
colnames(transformed_data) <- paste0("PC", 1:num_components_to_retain)

# Extract the last column from the original dataset
last_column <- data_normalized_min_max[, ncol(data_normalized_min_max)]

# Combine the selected columns with the last column from the original dataset
final_dataset <- data.frame(transformed_data, labels = last_column)
final_dataset



# Sample a subset of your data
set.seed(123)
strata_var <- final_dataset$labels
indices <- createDataPartition(strata_var, p = 0.05, list = FALSE)
data_subset_min_max <- final_dataset[indices, ]

# Apply the trained model to the new dataset
distances_pca_min_max <- dist(data_subset_min_max[, -1], method = "euclidean")
distances_pca_min_max[is.na(distances_pca_min_max)] <- 0

hc <- hclust(distances_pca_min_max, method = "ward.D")

# Cut the tree to obtain cluster labels
cluster_labels_pca_min_max <- cutree(hc, k = 2)

# Assuming new_data has true labels in a column named "labels"
# Create a confusion matrix for the new dataset
confusion_matrix_pca_min_max <- table(cluster_labels_pca_min_max, data_subset_min_max$labels)
confusion_matrix_pca_min_max


# Create the PCA data frame for plotting, using only the first two principal components
pca_data_for_plotting <- data_subset_min_max[, 1:2]
pca_data_for_plotting$cluster <- as.factor(cluster_labels_pca_min_maxa)

# Function to create data frame for convex hull
getHull <- function(df) {
  ch <- chull(df$PC1, df$PC2)
  ch <- c(ch, ch[1])  # Complete the loop
  df[ch, ]
}

# Get the convex hulls for each cluster
hulls <- lapply(split(pca_data_for_plotting, pca_data_for_plotting$cluster), getHull)

# Combine all hulls into one data frame
hulls_df <- do.call(rbind, hulls)

# Create the plot with ggplot2
ggplot(pca_data_for_plotting, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.5) +
  geom_polygon(data = hulls_df, aes(fill = cluster), color = "black", alpha = 0.2) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(title = "Cluster plot", x = "Dim1 (PC1)", y = "Dim2 (PC2)")

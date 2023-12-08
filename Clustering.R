library(readr)
library(dendextend)
library(cluster)

# Read the data (change)
data_input <- "data_normalized_min_max.csv"
print(cols_of_interest)

# Sample
print(sum(is.na(data_input)) )
data_normalized <- read_csv(data_input)
data <- data_normalized[sample(nrow(data_normalized), size = 1000), ]
variables_of_interest <- colnames(data)[-c(1, 2, ncol(data))]
label_colors <- c("blue", "red")

# Subset data using variables of interest
data_subset <- data[, c("labels", variables_of_interest)]

# Create a plot to display dendrograms for each distance calculation method and linkage method
par(mfrow = c(2, 4))  

#########################################################################################
############################### Hierarchical ############################################
#########################################################################################

# Methods for distance calculation and linkage
distance_methods <- c("euclidean", "manhattan")#, "mahalanobis")
linkage_methods <- c("single", "complete", "average", "ward.D2") 

# Identification of combinations and ID for each
method_combinations <- expand.grid(distance_method = distance_methods, linkage_method = linkage_methods)
method_combinations$method_id <- 1:nrow(method_combinations)  

confusion_matrices <- list()
correct_counts <- vector("numeric", length = nrow(method_combinations))
incorrect_counts <- vector("numeric", length = nrow(method_combinations))

for (i in 1:nrow(method_combinations)) {
  distance_method <- method_combinations$distance_method[i]
  linkage_method <- method_combinations$linkage_method[i]
  
  # Calculate distances using the subset of variables
  distances <- dist(data[, variables_of_interest], method = distance_method)
  # Replace NA values with 0
  distances[is.na(distances)] <- 0
  
  hc <- hclust(distances, method = linkage_method)
  
  dend <- as.dendrogram(hc)
  title <- paste("Hierarchical Clustering - Method:", linkage_method, ", Distance:", distance_method)
  dend <- color_branches(dend, k = 2)
  plot(dend, main = title)
  
  cluster_labels <- cutree(hc, k = 2)
  confusion_matrix <- table(cluster_labels, data$labels)
  method_name <- paste("Division:", distance_method, "- Linkage:", linkage_method)
  confusion_matrices[[method_name]] <- confusion_matrix
  
  incorrect_counts[i] <- sum(diag(confusion_matrix))
  correct_counts[i] <- sum(confusion_matrix) - incorrect_counts[i]
}

for (method_name in names(confusion_matrices)) {
  print(method_name)
  print(confusion_matrices[[method_name]])
}

summary_table <- data.frame(
  Combination = paste(method_combinations$distance_method, method_combinations$linkage_method),
  Correct = correct_counts,
  Incorrect = incorrect_counts
)

# Identify the method with the highest correct count and the lowest incorrect count
best_correct <- summary_table[which.max(summary_table$Correct), ]
best_incorrect <- summary_table[which.min(summary_table$Incorrect), ]

# Display the summary table
print(summary_table)

# Display the best correct and best incorrect methods
print("Best Correct Method:")
print(best_correct)

print("Best Incorrect Method:")
print(best_incorrect)


#########################################################################################
########################################## K-Means ######################################
#########################################################################################

km <- kmeans(data[, variables_of_interest], centers = 2)

cluster_labels_km <- km$cluster
method_name_km <- paste("K-means")

# Calculate confusion matrix for K-means
confusion_matrix_km <- table(cluster_labels_km, data$labels)
correct_counts <- sum(diag(confusion_matrix_km))
incorrect_counts <- sum(confusion_matrix_km) - correct_counts

print(method_name_km)
print(confusion_matrix_km)

summary_table <- data.frame(
  Method = distance_method,
  Correct = correct_counts,
  Incorrect = incorrect_counts
)

print(summary_table)

#########################################################################################
######################################## K-Medoids ######################################
#########################################################################################

# Methods for distance calculation
distance_methods <- c("euclidean", "manhattan")

confusion_matrices <- list()
correct_counts <- vector("numeric", length = length(distance_methods))
incorrect_counts <- vector("numeric", length = length(distance_methods))

for (i in seq_along(distance_methods)) {
  distance_method <- distance_methods[i]
  
  # Perform K-medoids clustering using different distance methods
  medoids_result <- pam(data, k = 2, diss = FALSE, metric = distance_method)
  
  cluster_labels_medoids <- medoids_result$clustering
  method_name_medoids <- paste("K-medoids - Method:", distance_method)
  
  # Calculate confusion matrix for K-medoids
  confusion_matrix_medoids <- table(cluster_labels_medoids, data$labels)
  confusion_matrices[[method_name_medoids]] <- confusion_matrix_medoids
  
  # Calculate correct and incorrect counts for each method
  correct_counts[i] <- sum(diag(confusion_matrix_medoids))
  incorrect_counts[i] <- sum(confusion_matrix_medoids) - correct_counts[i]
}

# Display confusion matrices for different methods
for (method_name in names(confusion_matrices)) {
  print(method_name)
  print(confusion_matrices[[method_name]])
}

# Display correct and incorrect counts for different methods
summary_table_medoids <- data.frame(
  Method = distance_methods,
  Correct = correct_counts,
  Incorrect = incorrect_counts
)

print(summary_table_medoids)

# Identify the best correct and best incorrect methods
best_correct_medoids <- summary_table_medoids[which.max(summary_table_medoids$Correct), ]
best_incorrect_medoids <- summary_table_medoids[which.min(summary_table_medoids$Incorrect), ]

print("Best Correct Method:")
print(best_correct_medoids)

print("Best Incorrect Method:")
print(best_incorrect_medoids)


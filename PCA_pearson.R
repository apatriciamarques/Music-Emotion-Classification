data <- read_csv("278k_song_labelled.csv")
data <- subset(data, labels %in% c(0, 1))

correlation_matrix <- cor(data)

# Get the number of columns (variables) in the dataset
num_vars <- ncol(data)

# Set the threshold for correlation (e.g., 0.7)
threshold <- 0.70

# Initialize an empty list to store variable pairs with high correlation
highly_correlated_pairs <- list()

# Iterate through the correlation matrix to find highly correlated variables
for (i in 1:(num_vars - 1)) {
  for (j in (i + 1):num_vars) {
    if (correlation_matrix[i, j] > threshold) {
      # Store the pair of variables with high correlation and their correlation value
      pair_name <- paste(names(data)[i], names(data)[j])
      highly_correlated_pairs[[pair_name]] <- correlation_matrix[i, j]
    }
  }
}

# Display the variable pairs with correlations above the threshold
print(highly_correlated_pairs)

# Calculate mean absolute correlation for each variable
mean_abs_cor <- colMeans(abs(correlation_matrix))

# Initialize a list to store removed variable names
removed_vars <- list()

# Identify the variables with the highest mean absolute correlation and remove one from each pair
for (pair_name in names(highly_correlated_pairs)) {
  pair <- unlist(strsplit(pair_name, " "))  # Split the pair name into individual variable names
  
  # Remove the variable with the higher mean absolute correlation
  mean_abs_cor_var1 <- mean_abs_cor[pair[1]]
  mean_abs_cor_var2 <- mean_abs_cor[pair[2]]
  
  if (mean_abs_cor_var1 > mean_abs_cor_var2) {
    removed_vars[[pair[1]]] <- pair[1]  # Store the removed variable name
    data <- data[, !names(data) %in% pair[1]]
  } else {
    removed_vars[[pair[2]]] <- pair[2]  # Store the removed variable name
    data <- data[, !names(data) %in% pair[2]]
  }
}

# Display the removed variables
print("Removed variables:")
print(removed_vars)

library(stats)

# Perform PCA on the updated dataset (data without removed variables)
pca_result <- prcomp(data, scale = TRUE)

# Explained variance ratio for each principal component
pca_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Specify a device to display the plot
windows()  # Use this if you're on a Windows machine, or change according to your environment

# Plot the variance explained by each principal component
plot(pca_variance_ratio, type = 'b', xlab = 'Principal Component', ylab = 'Proportion of Variance Explained Pearson')

# Calculate the number of components that explain 90% of the variance
num_components_to_retain <- which(cumsum(pca_variance_ratio) >= 0.9)[1]

# Obtain the transformed data using the retained principal components
transformed_data <- pca_result$x[, 1:num_components_to_retain]
colnames(transformed_data) <- paste0("PC", 1:num_components_to_retain)

# Extract the last column from the original dataset
last_column <- data[, ncol(data)]

# Combine the selected columns with the last column from the original dataset
final_dataset <- data.frame(transformed_data, labels = last_column)
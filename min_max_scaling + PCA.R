# Define a function for Min-Max normalization
min_max_normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}

# Min-Max normalization for each numeric column
data_normalized_min_max <- as.data.frame(lapply(data, min_max_normalize))

# Perform PCA on the updated dataset (data without removed variables)
pca_result <- prcomp(data_normalized_yeo_johnson, scale = TRUE)

# Explained variance ratio for each principal component
pca_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Specify a device to display the plot
windows()  

# Plot the variance explained by each principal component
plot(pca_variance_ratio, type = 'b', xlab = 'Principal Component', ylab = 'Proportion of Variance Explained')

# Select the number of principal components to retain based on explained variance
# For example, you might select components that explain a certain percentage of variance (e.g., 90%)

# Obtain the transformed data using selected principal components
num_components_to_retain <- 5  # Adjust this number based on your explained variance threshold
transformed_data <- as.data.frame(predict(pca_result, newdata = data))

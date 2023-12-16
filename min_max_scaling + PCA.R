# Define a function for Min-Max normalization
min_max_normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}

# Min-Max normalization for each numeric column
data_normalized_min_max <- as.data.frame(lapply(data, min_max_normalize))

# Perform PCA on the updated dataset (data without removed variables)
pca_result <- prcomp(data_normalized_min_max, scale = TRUE)

# Explained variance ratio for each principal component
pca_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Specify a device to display the plot
windows()  

# Plot the variance explained by each principal component
plot(pca_variance_ratio, type = 'b', xlab = 'Principal Component', ylab = 'Proportion of Variance Explained')



# Obtain the transformed data using selected principal components
num_components_to_retain <- 5  
transformed_data <- as.data.frame(predict(pca_result, newdata = data))

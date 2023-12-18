#Yeo-Johnson
library(caret)
preProcValues <- preProcess(data, method = c("YeoJohnson"))
data_normalized_yeo_johnson <- predict(preProcValues, data)

library(stats)

# Perform PCA on the updated dataset (data without removed variables)
pca_result <- prcomp(data_normalized_yeo_johnson, scale = TRUE)

# Explained variance ratio for each principal component
pca_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Specify a device to display the plot
windows()  

# Plot the variance explained by each principal component
plot(pca_variance_ratio, type = 'b', xlab = 'Principal Component', ylab = 'Proportion of Variance Explained Yeo')

# Calculate the number of components that explain 90% of the variance
num_components_to_retain <- which(cumsum(pca_variance_ratio) >= 0.9)[1]

# Obtain the transformed data using the retained principal components
transformed_data <- pca_result$x[, 1:num_components_to_retain]
colnames(transformed_data) <- paste0("PC", 1:num_components_to_retain)

# Extract the last column from the original dataset
last_column <- data[, ncol(data)]

# Combine the selected columns with the last column from the original dataset
final_dataset <- data.frame(transformed_data, labels = last_column)
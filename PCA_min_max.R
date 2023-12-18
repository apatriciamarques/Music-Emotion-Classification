data <- read_csv("278k_song_labelled.csv")

data <- subset(data, labels %in% c(0, 1))

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
plot(pca_variance_ratio, type = 'b', xlab = 'Principal Component', ylab = 'Proportion of Variance Explained Min Max')

# Calculate the number of components that explain 90% of the variance
num_components_to_retain <- which(cumsum(pca_variance_ratio) >= 0.9)[1]

# Obtain the transformed data using the retained principal components
transformed_data <- pca_result$x[, 1:num_components_to_retain]
colnames(transformed_data) <- paste0("PC", 1:num_components_to_retain)

# Extract the last column from the original dataset
last_column <- data_normalized_min_max[, ncol(data_normalized_min_max)]

# Combine the selected columns with the last column from the original dataset
final_dataset <- data.frame(transformed_data, labels = last_column)
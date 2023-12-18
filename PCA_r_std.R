library(readr)
library(ggplot2)
library(readxl)

destination <- 'C:/Users/catar/OneDrive/Documentos/PCA_Analysis_robust_stand.pdf'

# Open PDF
pdf(file = destination)
data <- read_csv("278k_song_labelled.csv")
data <- subset(data, labels %in% c(0, 1))

# Robust standardization function using median and MAD
robust_scalar<- function(x){(x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))}

# Apply robust standardization to the dataset
data.robust <- as.data.frame(lapply(data, robust_scalar))

# Robust PCA using the median and MAD
data.rpca <- prcomp(data.robust, retx = TRUE)
summary(data.rpca)

# Scores
x <- data.rpca$x
num_components <- ncol(data.rpca$x)

# Loadings
loadings <- data.rpca$rotation
round(loadings[, 2:11], 3)

# Calculate the proportion of variance explained by each PC
var_explained <- data.rpca$sdev^2 / sum(data.rpca$sdev^2) * 100

# Specify a device to display the plot
windows() 

# Plot the variance explained by each principal component
plot(var_explained, type = 'b', xlab = 'Principal Component', ylab = 'Proportion of Variance Explained Min Max')

# Calculate the x-axis position for the dots aligned with the center of the bars
x_pos_centered <- bar_positions + bar_width / 2

# Add points representing the percentage values aligned with the center of the bars
points(x_pos_centered, var_explained, col = "#8b138c", pch = 19)

# Connect the points with a line
lines(x_pos_centered, var_explained, col = "#8b138c", type = "b")

abline(h = 8, col = "#008ECC", lty = 2)

# Cumulative proportion of variance explained
cumulative_var <- cumsum(var_explained)

# Calculate the number of components that explain 90% of the variance
num_components_to_retain <- which(cumsum(var_explained) >= 0.9)[1]

# Obtain the transformed data using the retained principal components
transformed_data <- data.rpca$x[, 1:num_components_to_retain]

# Turn off PDF plotting
dev.off()

selected_columns <- data.robust$x[, 1:num_components_to_retain]

# Extract the last column from the original dataset
last_column <- data[, ncol(data)]

# Combine the selected columns with the last column from the original dataset
final_dataset <- cbind(transformed_data, LastColumn = last_column)

# Write the merged dataset to an Excel file
#write.xlsx(final_dataset, "C:/Users/catar/OneDrive/Documentos/PCA_Analysis_classic_stand_dataset.xlsx", sheetName = "MergedDataset", rowNames = FALSE)

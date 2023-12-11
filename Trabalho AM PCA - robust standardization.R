library(readr)
library(ggplot2)
library(readxl)

destination <- 'C:/Users/catar/OneDrive/Documentos/PCA_Analysis_robust_stand.pdf'

# Open PDF
pdf(file = destination)
data <- read_csv("278k_song_labelled.csv")

# Robust PCA using the median and MAD
data.robust <- scale(data[, 2:12], center = TRUE, scale = TRUE)
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

# Visualize the variance explained by each principal component
bar_width <- 0.8  # Define the width of each bar
bar_positions <- barplot(var_explained, names.arg = 1:length(var_explained), 
                         xlab = "Principal Component", ylab = "Percentage of Variance Explained",
                         main = "Percentage of Variance Explained by PCs", col = "#6495ED")  # Set color

# Calculate the x-axis position for the dots aligned with the center of the bars
x_pos_centered <- bar_positions + bar_width / 2

# Add points representing the percentage values aligned with the center of the bars
points(x_pos_centered, var_explained, col = "#8b138c", pch = 19)

# Connect the points with a line
lines(x_pos_centered, var_explained, col = "#8b138c", type = "b")

abline(h = 8, col = "#008ECC", lty = 2)

# Cumulative proportion of variance explained
cumulative_var <- cumsum(var_explained)

# Find the number of components that explain a certain threshold of variance (e.g., 90%)
num_components_90 <- which(cumulative_var >= 90)[1]

# Turn off PDF plotting
dev.off()

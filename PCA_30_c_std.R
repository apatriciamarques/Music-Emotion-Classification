library(readr)
library(ggplot2)
library(readxl)

destination <- 'C:/Users/catar/OneDrive/Documentos/PCA_Analysis_classic_stand_30.pdf'

#open PDF
pdf(file=destination, width = 8, height = 6)
data <- read_csv("278k_song_labelled.csv")

data <- subset(data, labels %in% c(0, 1))

#PCA based on the covariance matrix
data.cpca <- prcomp(data[,2:12], scale. = TRUE, retx=TRUE)
summary(data.cpca)
#screeplot(data.cpca, main="", type="lines")
#abline(h=mean(data.cpca$sdev^2),col="green")

#Scores

x <- data.cpca$x
num_components <- ncol(data.cpca$x)

#Loadings

loadings<-data.cpca$r
round(loadings[,2:11],3)

# Calculate the proportion of variance explained by each PC
var_explained <- data.cpca$sdev^2 / sum(data.cpca$sdev^2) * 100

bar_width <- 0.8  # Define the width of each bar
bar_positions <- barplot(var_explained, names.arg = 1:length(var_explained), 
                         xlab = "Principal Component", ylab = "Percentage of Variance Explained",
                         main = "Percentage of Variance Explained by PCs", col = "#6495ED"
                         )  # Set color

# Calculate the x-axis position for the dots aligned with the center of the bars
x_pos_centered <- bar_positions + bar_width / 2

# Add points representing the percentage values aligned with the center of the bars
points(x_pos_centered, var_explained, col = "#8b138c", pch = 19)

# Connect the points with a line
lines(x_pos_centered, var_explained, col = "#8b138c", type = "b")


# Cumulative proportion of variance explained
cumulative_var <- cumsum(var_explained)


# Find the number of components that explain a certain threshold of variance (e.g., 90%)
num_components_90 <- which(cumulative_var >= 90)[1]

#turn off PDF plotting
dev.off() 

selected_columns <- data.cpca$x[, 1:num_components_90]

# Extract the last column from the original dataset
last_column <- data[, ncol(data)]

# Combine the selected columns with the last column from the original dataset
final_dataset <- cbind(selected_columns, LastColumn = last_column)

# Write the merged dataset to an Excel file
#write.xlsx(final_dataset, "C:/Users/catar/OneDrive/Documentos/PCA_Analysis_classic_stand_dataset.xlsx", sheetName = "MergedDataset", rowNames = FALSE)


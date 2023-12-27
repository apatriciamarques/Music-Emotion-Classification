library(readr)
library(ggplot2)
library(corrplot)
library(psych)
library(caret)
library(class)
library(pheatmap)
library(data.table)
library(rpart)
library(dendextend)
library(cluster)




#IMPORTANTE - MUDAR O DIRETÓRIO
orig_data <- read_csv("MEFT/4º ano/Análise Multivariada/Projeto/278k_song_labelled.csv")

orig_data <- subset(orig_data, labels %in% c(0, 1))

cols_of_interest <- colnames(orig_data)[-c(1, length(orig_data))]
#print(cols_of_interest)

#Doing a sample of our data
strata_var <- orig_data$labels
set.seed(123)
indices <- createDataPartition(strata_var, p = 0.05, list = FALSE)
data <- orig_data[indices, ]

#changing the label that is num to a 2 level factor
data$labels <- factor(data$labels, levels = c(0, 1))

#Dividing in test set and train test for the SUPERVISED METHODS
set.seed(121)
train_indices <- sample(1:nrow(data), 0.9 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

#Creating the multiple data sets that are needed
train_data = train_data[, -1]
test_data = test_data[, -1]
data_normalized = data[, -1]
data_stand = data[, -1]
data_stand_median = data[, -1]



#####################################################
#####################################################
############### EXPLORATORY ANALYSIS ################
#####################################################
#####################################################

#FOR THIS FIRST PART WE USE THE ALL DATA SET

label_colors <- c("blue", "green")


ggplot(orig_data, aes(x = factor(labels, labels = c("sad", "happy")))) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black", alpha = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Emotions", x = "Emotion", y = "Frequency")


############ Variable Distribution - Histograms ###############

#Duration
ggplot(orig_data, aes(x = `duration (ms)`, fill = factor(labels))) +
  geom_histogram(bins = 20, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Duration by Emotion", x = "Duartion (1e+06 ms)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1.5e7, by = 0.5e6), labels = seq(0, 1.5e7, by = 0.5e6) / 1e6)


#Accousticness
ggplot(orig_data, aes(x = acousticness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Acousticness by Emotion", x = "Acousticness", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +
  theme_minimal()

#Danceability
ggplot(orig_data, aes(x = danceability, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Danceability by Emotion", x = "Danceability ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Energy
ggplot(orig_data, aes(x = energy, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Energy by Emotion", x = "Energy ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Loudness
ggplot(orig_data, aes(x = loudness, fill = factor(labels))) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Loudness by Emotion", x = "Loudness (dB)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Speechiness
ggplot(orig_data, aes(x = speechiness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Speechiness by Emotion", x = "Speechiness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Acousticness
ggplot(orig_data, aes(x = acousticness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Acousticness by Emotion", x = "Acousticness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Instrumentalness
ggplot(orig_data, aes(x = instrumentalness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Instrumentalness by Emotion", x = "Instrumentalness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Liveness
ggplot(orig_data, aes(x = liveness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Liveness by Emotion", x = "Liveness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Valence
ggplot(orig_data, aes(x = valence, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Valence by Emotion", x = "Valence ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Tempo
ggplot(orig_data, aes(x = tempo, fill = factor(labels))) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Tempo by Emotion", x = "Tempo (BPM)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Spec_rate
ggplot(orig_data, aes(x = spec_rate, fill = factor(labels))) +
  geom_histogram(bins = 10, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Spec_rate by Emotion", x = "Spec_rate ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()


############Box plots#############

#Duration
ggplot(orig_data, aes(x = factor(labels), y = `duration (ms)`, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Duration", x = "", y = "Duration (ms)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Danceability
ggplot(orig_data, aes(x = factor(labels), y = danceability, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Danceability", x = "", y = "Danceability", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Energy
ggplot(orig_data, aes(x = factor(labels), y = energy, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Energy", x = "", y = "Energy", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Loudness
ggplot(orig_data, aes(x = factor(labels), y = loudness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Loudness", x = "", y = "Loudness (dB)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Speechiness
ggplot(orig_data, aes(x = factor(labels), y = speechiness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Speechiness", x = "", y = "Speechiness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Acousticness
ggplot(orig_data, aes(x = factor(labels), y = acousticness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Acousticness", x = "", y = "Acousticness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Instrumentalness
ggplot(orig_data, aes(x = factor(labels), y = instrumentalness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Instrumentalness", x = "", y = "Instrumentalness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Liveness
ggplot(orig_data, aes(x = factor(labels), y = liveness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Liveness", x = "", y = "Liveness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Valence
ggplot(orig_data, aes(x = factor(labels), y = valence, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Valence", x = "", y = "Valence", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Tempo
ggplot(orig_data, aes(x = factor(labels), y = tempo, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Tempo", x = "", y = "Tempo (BPM)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Spec_rate
ggplot(orig_data, aes(x = factor(labels), y = spec_rate, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Spec_rate", x = "", y = "Spec_rate", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#Correlation matrix
corrplot(cor(orig_data[, c(2:13)]), method = "square", type = "full", order = "original",
         tl.col = "black", addCoef.col = "black", number.cex = 0.5, tl.cex = 0.6,
         title = "Correlation Matrix", mar = c(0, 0, 1, 0)
)


#####################################################
#####################################################
############## PRE-PROCESSING METHODS ###############
#####################################################
#####################################################


####################################
###### MIN-MAX NORMALIZATION #######
####################################

#Taking the min and max of each column of the data
min_data_normalized<- sapply(data[cols_of_interest], min)
max_data_normalized<- sapply(data[cols_of_interest], max)

#Calculate the difference between the max and the min of each column
scale_data_normalized = max_data_normalized - min_data_normalized

#Using the scale function with the values for the z-score
data_normalized[cols_of_interest] <- scale(data[cols_of_interest], 
                                           center = min_data_normalized, 
                                           scale = scale_data_normalized)
#describe(data_normalized)

####################################
########## YEO - JOHSON ############
####################################

preProcValues <- preProcess(data, method = c("YeoJohnson"))
data_normalized_yeo_johnson <- predict(preProcValues, data)


######################################
####### Z-SCORE TRANSFORMATION #######
######################################

#using the function scale, that does the standardization
data_stand[cols_of_interest] <- scale(data_stand[cols_of_interest])


#############################################
####### STANDARDIZATION WITH MEDIAN=0 #######
#############################################

#Taking the mean and SD of the training data
median_values <- apply(data[cols_of_interest], 2, median)
mad_values <- apply(data[cols_of_interest], 2, mad)

data_stand_median[cols_of_interest] = scale(data_stand_median[cols_of_interest], 
                                            center = median_values, 
                                            scale = mad_values)


#########################
########## PCA ##########
#########################


#Classic PCA
data.cpca <- prcomp(data[,1:11], scale. = TRUE, retx=TRUE)

var_explained <- data.cpca$sdev^2 / sum(data.cpca$sdev^2) * 100
cumulative_var <- cumsum(var_explained)
num_components_90 <- which(cumulative_var >= 90)[1]

selected_columns_data <- data.cpca$x[, 1:num_components_90]
last_column_train <- data[, ncol(train_data)]
data_pca <- cbind(selected_columns_data, LastColumn = last_column_train)




#####################################################
#####################################################
############### UNSUPERVISED METHODS ################
#####################################################
#####################################################



#Using the normalized data 
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
############## Dendogram for Ward's Method with Euclidean Distance ######################
#########################################################################################


# Calculate distances
distances <- dist(data_subset[, -1], method = "euclidean")
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

data_subset_zscore <- data_stand

# Apply the trained model to the new dataset
distances_zscore <- dist(data_subset_zscore[, -ncol(data_subset_zscore)], method = "euclidean")
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

data_subset_yeo <- data_normalized_yeo

# Apply the trained model to the new dataset
distances_yeo <- dist(data_subset_yeo[, -ncol[data_subset_yeo]], method = "euclidean")
distances_yeo[is.na(distances_yeo)] <- 0

hc <- hclust(distances_yeo, method = "ward.D")

# Cut the tree to obtain cluster labels
cluster_labels_yeo <- cutree(hc, k = 2)

# Assuming new_data has true labels in a column named "labels"
# Create a confusion matrix for the new dataset
confusion_matrix_yeo <- table(cluster_labels_yeo, data_subset_yeo$labels)
confusion_matrix_yeo


# ################Gráfico de clusters+Confusion matrix of min_max+PCA################
# 
# 
# # Perform PCA on the updated dataset (data without removed variables)
# pca_result <- prcomp(data_normalized_min_max, scale = TRUE)
# 
# # Explained variance ratio for each principal component
# pca_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)
# 
# # Calculate the number of components that explain 90% of the variance
# num_components_to_retain <- which(cumsum(pca_variance_ratio) >= 0.9)[1]
# 
# # Obtain the transformed data using the retained principal components
# transformed_data <- pca_result$x[, 1:num_components_to_retain]
# colnames(transformed_data) <- paste0("PC", 1:num_components_to_retain)
# 
# # Extract the last column from the original dataset
# last_column <- data_normalized_min_max[, ncol(data_normalized_min_max)]
# 
# # Combine the selected columns with the last column from the original dataset
# final_dataset <- data.frame(transformed_data, labels = last_column)
# final_dataset
# 
# 
# 
# # Sample a subset of your data
# set.seed(123)
# strata_var <- final_dataset$labels
# indices <- createDataPartition(strata_var, p = 0.05, list = FALSE)
# data_subset_min_max <- final_dataset[indices, ]
# 
# # Apply the trained model to the new dataset
# distances_pca_min_max <- dist(data_subset_min_max[, -1], method = "euclidean")
# distances_pca_min_max[is.na(distances_pca_min_max)] <- 0
# 
# hc <- hclust(distances_pca_min_max, method = "ward.D")
# 
# # Cut the tree to obtain cluster labels
# cluster_labels_pca_min_max <- cutree(hc, k = 2)
# 
# # Assuming new_data has true labels in a column named "labels"
# # Create a confusion matrix for the new dataset
# confusion_matrix_pca_min_max <- table(cluster_labels_pca_min_max, data_subset_min_max$labels)
# confusion_matrix_pca_min_max
# 
# 
# # Create the PCA data frame for plotting, using only the first two principal components
# pca_data_for_plotting <- data_subset_min_max[, 1:2]
# pca_data_for_plotting$cluster <- as.factor(cluster_labels_pca_min_maxa)
# 
# # Function to create data frame for convex hull
# getHull <- function(df) {
#   ch <- chull(df$PC1, df$PC2)
#   ch <- c(ch, ch[1])  # Complete the loop
#   df[ch, ]
# }
# 
# # Get the convex hulls for each cluster
# hulls <- lapply(split(pca_data_for_plotting, pca_data_for_plotting$cluster), getHull)
# 
# # Combine all hulls into one data frame
# hulls_df <- do.call(rbind, hulls)
# 
# # Create the plot with ggplot2
# ggplot(pca_data_for_plotting, aes(x = PC1, y = PC2, color = cluster)) +
#   geom_point(alpha = 0.5) +
#   geom_polygon(data = hulls_df, aes(fill = cluster), color = "black", alpha = 0.2) +
#   scale_color_manual(values = c("blue", "red")) +
#   scale_fill_manual(values = c("blue", "red")) +
#   theme_minimal() +
#   labs(title = "Cluster plot", x = "Dim1 (PC1)", y = "Dim2 (PC2)")
# 
# 




#####################################################
#####################################################
################ SUPERVISED METHODS #################
#####################################################
#####################################################



################################
####### PRE-PROCESSING #########
################################


#NORMALIZAÇÃO do train e do test sets

min_data<- sapply(train_data[cols_of_interest], min)
max_data<- sapply(train_data[cols_of_interest], max)

scale_norm = max_data - min_data

# Min-Max normalization for each numeric column

train_data[cols_of_interest] <- scale(train_data[cols_of_interest],
                          center = min_data,
                          scale = scale_norm)
test_data[cols_of_interest] <- scale(test_data[cols_of_interest],
                         center = min_data,
                         scale = scale_norm)



#CROSS VALIDATION
cv_control <- trainControl(method = "cv", number = 15)



####################################
###### K nearest neighbors #########
####################################

#KNN with CV to tune the parameter k
knn_model <- train(labels ~ .,
                   method     = "knn",
                   tuneGrid   = expand.grid(k = seq(1, 40, 2)),
                   trControl  = cv_control,
                   metric     = "Accuracy",
                   data       = train_data)


print(knn_model)

#plot of the accuracy VS k
plot(knn_model, col="blue", pch = 20, lwd = 1.5)

#Saving the best k
best_k <- knn_model$bestTune$k

print(best_k)

#predictions
predictions_knn <- predict(knn_model, newdata = test_data)

#Confusion Matrix
cm_knn <- as.data.frame(table(test_data$labels, predict(knn_model, newdata = test_data)))
cm_knn$Var2 <- factor(cm_knn$Var2, levels=rev(levels(cm_knn$Var2)))

#print(cm_knn)

#Plotting the Confusion Matrix
ggplot(cm_knn, mapping = aes(x = Var1,
                             y = predictions_knn)) +
  geom_tile(aes(fill = Freq)) +
  labs(x = "Real",y = "Prediction") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue",
                      high = "cornflowerblue",
                      trans = "log",
                      breaks = seq(min(cm_knn$Freq), max(cm_knn$Freq), by = as.integer(((max(cm_knn$Freq)-min(cm_knn$Freq))/2)))) +
  ggtitle('Confusion Matrix of KNN')




####################################
########## Nayve Bayes #############
####################################

#Nayve Bayes model
nb_tuneGrid <- expand.grid(.usekernel = TRUE,
                           .adjust = c(0.5, 1, 1.5),
                           .laplace = c(0, 0.5, 1)
)

naive_model <- train(labels ~ .,
                     method     = "naive_bayes",
                     trControl  = cv_control,
                     metric     = "Accuracy",
                     tuneGrid   = nb_tuneGrid,
                     data       = train_data)


#Confusion Matrix
cm_nb <- as.data.frame(table(test_data$labels, predict(naive_model, newdata = test_data)))
cm_nb$Var2 <- factor(cm_nb$Var2, levels=rev(levels(cm_nb$Var2)))


#plotting the Confusion matrix
ggplot(cm_nb, mapping = aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  labs(x = "Real", y = "Predicted") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue",
                      high = "cornflowerblue",
                      breaks = seq(50, 500, by = 225)) +
  ggtitle('Confusion Matrix of Naive bayes')


####################################
########## Decision Tree ###########
####################################

#Decision Tree model
tree_model <- rpart(labels ~ ., data = train_data)

#Confusion matrix
cm_dt <- as.data.frame(table(test_data$labels, predict(tree_model, newdata = test_data, type = "class")))
cm_dt$Var2 <- factor(cm_dt$Var2, levels=rev(levels(cm_dt$Var2)))

ggplot(cm_dt, mapping = aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  labs(x = "Real", y = "Predicted") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue",
                      high = "cornflowerblue",
                      breaks = seq(50, 500, by = 225)) +
  ggtitle('Confusion Matrix of Decison Tree')



##################################
######### Radom Forest ###########
##################################

#Random Forest Model with hyperparameter tuning
forest_model <- train(labels ~ .,
                      method = "rf",
                      trControl = cv_control,
                      metric = "Accuracy",
                      tuneGrid = expand.grid(mtry = seq(2.5, 4.5, 0.2)), 
                      data = train_data)



#plot of the accuracy VS mtry
plot(forest_model, col="blue", pch = 20, lwd = 1.5)

#Confusion matrix
cm_rf <- as.data.frame(table(test_data$labels, predict(forest_model, newdata = test_data)))
cm_rf$Var2 <- factor(cm_rf$Var2, levels=rev(levels(cm_rf$Var2)))


#Plotting the Confusion Matrix
ggplot(cm_rf, mapping = aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  labs(x = "Real", y = "Predicted") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue",
                      high = "cornflowerblue",
                      breaks = seq(50, 500, by = 225)) +
  ggtitle('Confusion Matrix of Random Forest')


##################################
############# SVM ################
##################################

#SVM Model
#svm_model <- svm(labels ~ ., data = train_data)

svm_tuneGrid <- expand.grid(.cost = 5,
                            .Loss = c(1, 1.1, 1.2, 1.3, 1.4),
                            .weight = c(0.9)
)

svm_model <- train(labels ~ .,
                   method = "svmLinearWeights2",
                   trControl = cv_control,
                   metric = "Accuracy",
                   tuneGrid = svm_tuneGrid, 
                   data = train_data)

best_cost <- svm_model$bestTune$cost
best_loss <- svm_model$bestTune$Loss
best_weight <- svm_model$bestTune$weight

best_cost
best_loss
best_weight


#Confusion Matrix
cm_svm <- as.data.frame(table(test_data$labels, predict(svm_model, newdata = test_data)))
cm_svm$Var2 <- factor(cm_svm$Var2, levels=rev(levels(cm_svm$Var2)))

ggplot(cm_svm, mapping = aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  labs(x = "Real", y = "Predicted") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue",
                      high = "cornflowerblue",
                      breaks = seq(50, 500, by = 225)) +
  ggtitle('Confusion Matrix of SVM')



##################################
###### Gradient Boosting #########
##################################

xgb_model <- train(
  labels ~ .,                 
  data = train_data,           
  method = "xgbTree",    
  trControl = cv_control ,
  tuneGrid = expand.grid(
    nrounds = seq(50, 500, 50),
    max_depth = 4,
    colsample_bytree = 1, #seq(1,8,1)
    eta =0.3, #seq(0.1, 0.9, 0.1)
    gamma=0,
    min_child_weight = 4, #seq(1,8,1)
    subsample = 1 )) #seq(1,8,1)


#print(xgb_model)

#plot of the accuracy VS k
plot(xgb_model, col = 'blue', pch = 20, lwd = 1.5)

#Saving the best k
#best_nrounds <- xgb_model$bestTune$nrounds

best_colsample <- xgb_model$bestTune$min_child_weight

print(best_colsample)

predictions_xgb <- predict(xgb_model, newdata = test_data)


#Confusion Matrix
cm_xgb <- as.data.frame(table(test_data$labels, predict(xgb_model, newdata = test_data)))
cm_xgb$Var2 <- factor(cm_xgb$Var2, levels=rev(levels(cm_xgb$Var2)))


#Plotting the Confusion Matrix
ggplot(cm_xgb, mapping = aes(x = Var1,
                             y = predictions_xgb)) +
  geom_tile(aes(fill = Freq)) +
  labs(x = "Real",y = "Prediction") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue",
                      high = "cornflowerblue",
                      trans = "log",
                      breaks = seq(min(cm_xgb$Freq), max(cm_xgb$Freq), by = as.integer(((max(cm_xgb$Freq)-min(cm_xgb$Freq))/2)))) +
  ggtitle('Confusion Matrix of Gradient Boosting')


confusion_matrix_clusters_supervised <- table(predict(xgb_model, newdata = test_data), test_data$labels)
print(confusion_matrix_clusters_supervised)



#####################3
#ACCURACY

#Accuracy for KNN #0.905
accuracy_knn <- sum(cm_knn$Freq[c(1,4)]) / sum(cm_knn$Freq)
cat("Accuracy for KNN:", accuracy_knn, "\n")

#Accuracy for Naive Bayes #0.882
accuracy_nb <- sum(cm_nb$Freq[c(1,4)]) / sum(cm_nb$Freq)
cat("Accuracy for Naive Bayes:", accuracy_nb, "\n")

#Accuracy for Decision Tree #0.892
accuracy_dt <- sum(cm_dt$Freq[c(1,4)]) / sum(cm_dt$Freq)
cat("Accuracy for Decision Tree:", accuracy_dt, "\n")

#Accuracy for Random Forest #0.95
accuracy_rf <- sum(cm_rf$Freq[c(1,4)]) / sum(cm_rf$Freq)
cat("Accuracy for Random Forest:", accuracy_rf, "\n")

#Accuracy for SVM #0.889
accuracy_svm <- sum(cm_svm$Freq[c(1,4)]) / sum(cm_svm$Freq)
cat("Accuracy for SVM:", accuracy_svm, "\n")

#Accuracy for Gradient Boosting #0.96
accuracy_xgb <- sum(cm_xgb$Freq[c(1,4)]) / sum(cm_xgb$Freq)
cat("Accuracy for eXtreme Gradient Boosting:", accuracy_xgb, "\n")



#Sensitivity
sensitivity_knn <- cm_knn$Freq[c(4)] / sum(cm_knn$Freq[c(2,4)])
cat("Sensitivity for KNN", sensitivity_knn)

sensitivity_nb <- cm_nb$Freq[c(4)] / sum(cm_nb$Freq[c(2,4)])
cat("Sensitivity for NB", sensitivity_nb)

sensitivity_dt <- cm_dt$Freq[c(4)] / sum(cm_dt$Freq[c(2,4)])
cat("Sensitivity for DT", sensitivity_dt)

sensitivity_rf <- cm_rf$Freq[c(4)] / sum(cm_rf$Freq[c(2,4)])
cat("Sensitivity for RF", sensitivity_rf)

sensitivity_svm <- cm_svm$Freq[c(4)] / sum(cm_svm$Freq[c(2,4)])
cat("Sensitivity for SVM", sensitivity_svm)

sensitivity_xgb <- cm_xgb$Freq[c(4)] / sum(cm_xgb$Freq[c(2,4)])
cat("Sensitivity for XGB", sensitivity_xgb)



#Specificity
specificity_knn <- cm_knn$Freq[c(1)] / sum(cm_knn$Freq[c(1,3)])
cat("Specificity for KNN", specificity_knn)

specificity_nb <- cm_nb$Freq[c(1)] / sum(cm_nb$Freq[c(1,3)])
cat("Specificity for NB", specificity_nb)

specificity_dt <- cm_dt$Freq[c(1)] / sum(cm_dt$Freq[c(1,3)])
cat("Specificity for DT", specificity_dt)

specificity_rf <- cm_rf$Freq[c(1)] / sum(cm_rf$Freq[c(1,3)])
cat("Specificity for RF", specificity_rf)

specificity_svm <- cm_svm$Freq[c(1)] / sum(cm_svm$Freq[c(1,3)])
cat("Specificity for RF", specificity_svm)

specificity_xgb <- cm_xgb$Freq[c(1)] / sum(cm_xgb$Freq[c(1,3)])
cat("Specificity for XGB", specificity_xgb)


#BAcc
bacc_knn <- (sensitivity_knn + specificity_knn)/2.0
cat("BAcc for KNN", bacc_knn)

bacc_nb <- (sensitivity_nb + specificity_nb)/2.0
cat("BAcc for NB", bacc_nb)

bacc_dt <- (sensitivity_dt + specificity_dt)/2.0
cat("BAcc for DT", bacc_dt)

bacc_rf <- (sensitivity_rf + specificity_rf)/2.0
cat("BAcc for RF", bacc_rf)

bacc_svm <- (sensitivity_svm + specificity_svm)/2.0
cat("BAcc for SVM", bacc_svm)

bacc_xgb <- (sensitivity_xgb + specificity_xgb)/2.0
cat("BAcc for XGB", bacc_xgb)




#Combining all the Confusion Matrices
combined_cm <- rbind(transform(cm_knn, Model = 'KNN'),
                     transform(cm_nb, Model = 'NB'),
                     transform(cm_dt, Model = 'DT'),
                     transform(cm_rf, Model = 'RF'),#substituir por cm_rf
                     transform(cm_svm, Model = 'SVM'),#substituir por cm_svm
                     transform(cm_xgb, Model = 'XGB'))#substituir por cm_xbg

combined_cm$Model <- factor(combined_cm$Model, levels = c('KNN', 'NB', 'DT', 'RF', 'SVM', 'XGB'))

# Plot using facet_grid with two lines and three columns
ggplot(combined_cm, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  labs(x = "Real", y = "Predicted") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "cornflowerblue",
                      breaks = seq(13, 524, by = 255)) +
  facet_wrap(~Model, scales = 'free', ncol = 3)+
  theme(strip.background = element_rect(fill = "transparent", color = "white", linewidth = 0.5),
        panel.background = element_rect(fill = "transparent", color = "white"))


#################################################
####### Supervised Methods + Clustering #########
#################################################


#########################################################################################
########################################## K-Means ######################################
#########################################################################################

train_data_no_labels <- train_data[, -which(names(train_data) == "labels")]

# Perform unsupervised k-means clustering
km_supervised.res <- kmeans(train_data_no_labels, 2, nstart = 25)

# Extract cluster labels
cluster_labels_supervised <- km_supervised.res$cluster

#create a data set to use in the supervised methods
train_data_clusters <- train_data_no_labels

#adding the new labels from the clustering
train_data_clusters$labels <- cluster_labels_supervised

train_data_clusters$labels <- factor(train_data_clusters$labels, levels = c(1, 2))


#Cross validation
cv_control <- trainControl(method = "cv", number = 15)


#############################################
###### Gradient Boosting + Clusters #########
#############################################

xgb_model_clusters <- train(
  labels ~ .,                 
  data = train_data_clusters,           
  method = "xgbTree",    
  trControl = cv_control ,
  tuneGrid = expand.grid(
    nrounds = seq(300, 1000, 100),
    max_depth = 4,
    colsample_bytree = 1, #seq(1,8,1)
    eta =0.3, #seq(0.1, 0.9, 0.1)
    gamma=0,
    min_child_weight = 4, #seq(1,8,1)
    subsample = 1 )) #seq(1,8,1)


#print(xgb_model)

#plot of the accuracy VS k
plot(xgb_model_clusters, col = 'blue', pch = 20, lwd = 1.5)

#Saving the best k
#best_nrounds <- xgb_model$bestTune$nrounds

predictions_xgb_clusters <- predict(xgb_model_clusters, newdata = test_data)
print(predictions_xgb_clusters)

#Confusion Matrix
cm_xgb_clusters <- as.data.frame(table(test_data$labels, predict(xgb_model_clusters, newdata = test_data)))
print(levels(cm_xgb_clusters$Var2))
cm_xgb_clusters$Var2 <- factor(cm_xgb_clusters$Var2, levels=rev(levels(cm_xgb_clusters$Var2)))
print(levels(cm_xgb_clusters$Var2))



#Plotting the Confusion Matrix
ggplot(cm_xgb_clusters, mapping = aes(x = Var1,
                             y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  labs(x = "Real",y = "Prediction") +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "lightblue",
                      high = "cornflowerblue",
                      breaks = seq(20, 500, 100)) +
  ggtitle('Confusion Matrix of Gradient Boosting in Cluster Results')




#Accuracy for Gradient Boosting #0.96
accuracy_xgb_clusters <- sum(cm_xgb_clusters$Freq[c(1,4)]) / sum(cm_xgb_clusters$Freq)
cat("Accuracy for eXtreme Gradient Boosting using clusters:", accuracy_xgb_clusters, "\n")

#Sensitivity
sensitivity_xgb_clusters <- cm_xgb_clusters$Freq[c(4)] / sum(cm_xgb_clusters$Freq[c(4,3)])
cat("Sensitivity for XGB using clusters:", sensitivity_xgb_clusters)

#Specificity
specificity_xgb_clusters <- cm_xgb_clusters$Freq[c(1)] / sum(cm_xgb_clusters$Freq[c(1,2)])
cat("Specificity for XGB using clusters:", specificity_xgb_clusters)

#BAcc
bacc_xgb_clusters <- (sensitivity_xgb_clusters + specificity_xgb_clusters)/2.0
cat("BAcc for XGB using clusters:", bacc_xgb_clusters)



library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(fs) 

# Read the data
data <- read_csv("278k_song_labelled.csv")

# Filter the data for labels 0 and 1
data <- subset(data, labels %in% c(0, 1))
label_colors <- c("blue", "red")
cols_of_interest <- colnames(data)[-c(1, length(data))]
print(cols_of_interest)

# Function to create histogram
create_histogram <- function(data, col, label_colors) {
  ggplot(data, aes_string(x = col, fill = factor(labels))) +
    geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of", col, "by Emotion"), x = col, y = "Frequency", fill = "Emotion") +
    scale_fill_manual(values = label_colors, breaks = c(0, 1), name = "Emotion") +
    theme_minimal()
}

# Function to create boxplot
create_boxplot <- function(data, col, label_colors) {
  ggplot(data, aes_string(x = factor(labels), y = col, fill = factor(labels))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", col, "by Emotion"), x = "Emotion", y = col, fill = "Emotion") +
    scale_fill_manual(values = label_colors, breaks = c(0, 1), name = "Emotion") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) 
}

###############################################################################
######################## Distribuições Iniciais ###############################
###############################################################################

#Duration
ggplot(data, aes(x = `duration (ms)`, fill = factor(labels))) +
  geom_histogram(bins = 20, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Duration by Emotion", x = "Duartion (1e+06 ms)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1.5e7, by = 0.5e6), labels = seq(0, 1.5e7, by = 0.5e6) / 1e6)


#Accousticness
ggplot(data, aes(x = acousticness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Acousticness by Emotion", x = "Acousticness", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +
  theme_minimal()

#Danceability
ggplot(data, aes(x = danceability, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Danceability by Emotion", x = "Danceability ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Energy
ggplot(data, aes(x = energy, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Energy by Emotion", x = "Energy ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Loudness
ggplot(data, aes(x = loudness, fill = factor(labels))) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Loudness by Emotion", x = "Loudness (dB)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Speechiness
ggplot(data, aes(x = speechiness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Speechiness by Emotion", x = "Speechiness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Acousticness
ggplot(data, aes(x = acousticness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Acousticness by Emotion", x = "Acousticness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Instrumentalness
ggplot(data, aes(x = instrumentalness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Instrumentalness by Emotion", x = "Instrumentalness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Liveness
ggplot(data, aes(x = liveness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Liveness by Emotion", x = "Liveness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Valence
ggplot(data, aes(x = valence, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Valence by Emotion", x = "Valence ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Tempo
ggplot(data, aes(x = tempo, fill = factor(labels))) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Tempo by Emotion", x = "Tempo (BPM)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Spec_rate
ggplot(data, aes(x = spec_rate, fill = factor(labels))) +
  geom_histogram(bins = 10, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Spec_rate by Emotion", x = "Spec_rate ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

###############################################################################
############################# Boxplots Iniciais ###############################
###############################################################################

#Duration
ggplot(data, aes(x = factor(labels), y = `duration (ms)`, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Duration", x = "", y = "Duration (ms)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Danceability
ggplot(data, aes(x = factor(labels), y = danceability, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Danceability", x = "", y = "Danceability", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Energy
ggplot(data, aes(x = factor(labels), y = energy, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Energy", x = "", y = "Energy", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Loudness
ggplot(data, aes(x = factor(labels), y = loudness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Loudness", x = "", y = "Loudness (dB)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Speechiness
ggplot(data, aes(x = factor(labels), y = speechiness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Speechiness", x = "", y = "Speechiness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Acousticness
ggplot(data, aes(x = factor(labels), y = acousticness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Acousticness", x = "", y = "Acousticness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Instrumentalness
ggplot(data, aes(x = factor(labels), y = instrumentalness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Instrumentalness", x = "", y = "Instrumentalness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Liveness
ggplot(data, aes(x = factor(labels), y = liveness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Liveness", x = "", y = "Liveness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Valence
ggplot(data, aes(x = factor(labels), y = valence, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Valence", x = "", y = "Valence", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Tempo
ggplot(data, aes(x = factor(labels), y = tempo, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Tempo", x = "", y = "Tempo (BPM)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Spec_rate
ggplot(data, aes(x = factor(labels), y = spec_rate, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Spec_rate", x = "", y = "Spec_rate", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Correlation matrix
corrplot(cor(data[, c(2:13)]), method = "square", type = "full", order = "original",
         tl.col = "black", addCoef.col = "black", number.cex = 0.5, tl.cex = 0.6,
         title = "Correlation Matrix", mar = c(0, 0, 1, 0)
)

###############################################################################
######################### Z-Score Normalization ###############################
###############################################################################

# Our variables are all numerical
# Z-score transformation function (skipping over missing values)
z_score <- function(x) {(x - mean(x)) / sd(x)}

# Apply Z-score normalization to each column
summary(data)
data[cols_of_interest] <- lapply(data[cols_of_interest], z_score)
summary(data)

###############################################################################
################ Distribuições após Z-Score Transformation ####################
###############################################################################

#Duration
ggplot(data, aes(x = `duration (ms)`, fill = factor(labels))) +
  geom_histogram(bins = 20, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Duration by Emotion", x = "Duartion (1e+06 ms)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 1.5e7, by = 0.5e6), labels = seq(0, 1.5e7, by = 0.5e6) / 1e6)


#Accousticness
ggplot(data, aes(x = acousticness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Acousticness by Emotion", x = "Acousticness", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +
  theme_minimal()

#Danceability
ggplot(data, aes(x = danceability, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Danceability by Emotion", x = "Danceability ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Energy
ggplot(data, aes(x = energy, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Energy by Emotion", x = "Energy ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Loudness
ggplot(data, aes(x = loudness, fill = factor(labels))) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Loudness by Emotion", x = "Loudness (dB)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Speechiness
ggplot(data, aes(x = speechiness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Speechiness by Emotion", x = "Speechiness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Acousticness
ggplot(data, aes(x = acousticness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Acousticness by Emotion", x = "Acousticness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Instrumentalness
ggplot(data, aes(x = instrumentalness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Instrumentalness by Emotion", x = "Instrumentalness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Liveness
ggplot(data, aes(x = liveness, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Liveness by Emotion", x = "Liveness ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Valence
ggplot(data, aes(x = valence, fill = factor(labels))) +
  geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Valence by Emotion", x = "Valence ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Tempo
ggplot(data, aes(x = tempo, fill = factor(labels))) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Tempo by Emotion", x = "Tempo (BPM)", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

#Spec_rate
ggplot(data, aes(x = spec_rate, fill = factor(labels))) +
  geom_histogram(bins = 10, color = "black", alpha = 0.7) +
  labs(title = "Distribution of Spec_rate by Emotion", x = "Spec_rate ", y = "Frequency", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  facet_grid(.~labels) +  
  theme_minimal()

###############################################################################
################# Boxplots após Z-Score Transformation ########################
###############################################################################

#Duration
ggplot(data, aes(x = factor(labels), y = `duration (ms)`, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Duration", x = "", y = "Duration (ms)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Danceability
ggplot(data, aes(x = factor(labels), y = danceability, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Danceability", x = "", y = "Danceability", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Energy
ggplot(data, aes(x = factor(labels), y = energy, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Energy", x = "", y = "Energy", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Loudness
ggplot(data, aes(x = factor(labels), y = loudness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Loudness", x = "", y = "Loudness (dB)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Speechiness
ggplot(data, aes(x = factor(labels), y = speechiness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Speechiness", x = "", y = "Speechiness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 

#Acousticness
ggplot(data, aes(x = factor(labels), y = acousticness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Acousticness", x = "", y = "Acousticness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Instrumentalness
ggplot(data, aes(x = factor(labels), y = instrumentalness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Instrumentalness", x = "", y = "Instrumentalness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Liveness
ggplot(data, aes(x = factor(labels), y = liveness, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Liveness", x = "", y = "Liveness", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Valence
ggplot(data, aes(x = factor(labels), y = valence, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Valence", x = "", y = "Valence", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Tempo
ggplot(data, aes(x = factor(labels), y = tempo, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Tempo", x = "", y = "Tempo (BPM)", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Spec_rate
ggplot(data, aes(x = factor(labels), y = spec_rate, fill = factor(labels))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Spec_rate", x = "", y = "Spec_rate", fill = "Emotion") +
  scale_fill_manual(values = label_colors, breaks = c("0", "1"), name = "Emotion") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#Correlation matrix
corrplot(cor(data[, c(2:13)]), method = "square", type = "full", order = "original",
         tl.col = "black", addCoef.col = "black", number.cex = 0.5, tl.cex = 0.6,
         title = "Correlation Matrix", mar = c(0, 0, 1, 0)
)

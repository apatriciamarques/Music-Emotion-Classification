library(readr)
library(ggplot2)
library(corrplot)


data <- read_csv("278k_song_labelled.csv")

data <- subset(data, labels %in% c(0, 1))


ggplot(data, aes(x = factor(labels, labels = c("sad", "happy")))) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black", alpha = 0.7) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Emotions", x = "Emotion", y = "Frequency")

label_colors <- c("blue", "green")
# Distribuição das variáveis
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

#Box plots
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

#Não há colunas não numéricas por isso todas as colunas vão ser alvo de normalização
#Normalização Min-Max 
# Define a function for Min-Max normalization
min_max_normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}

# Min-Max normalization for each numeric column
data_normalized_min_max <- as.data.frame(lapply(data, min_max_normalize))

#Yeo-Johnson
library(caret)
preProcValues <- preProcess(data, method = c("YeoJohnson"))
data_normalized_yeo_johnson <- predict(preProcValues, data)

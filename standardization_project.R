library(readr)
library(ggplot2)
library(corrplot)
library(psych)


data <- read_csv("MEFT/4º ano/Análise Multivariada/Projeto/278k_song_labelled.csv")

data <- subset(data, labels %in% c(0, 1))


#describe(data)

#Standardization with mean=0 and sd=1
#Standardization function
standardise_mean <- function(x) {(x - mean(x))/sd(x)}

#applying the standardization to all columns but the labels
data_std_mean <- as.data.frame(lapply(data[, -ncol(data)], standardise_mean))

#Adding the labels column to the frame
data_std_mean$labels <- data[, ncol(data)]

#describe(data_std_mean)


#Standardization MAD=1
#Function
standardise_median <- function(x) {(x - median(x))/mad(x)}

#applying the standardization to all columns but the labels
data_std_median <- as.data.frame(lapply(data[, -ncol(data)], standardise_median))

#Adding the labels column to the frame
data_std_median$labels <- data[, ncol(data)]
write.csv(data_std_median, "data_std_median.csv", row.names=TRUE)

describe(data_std_median)



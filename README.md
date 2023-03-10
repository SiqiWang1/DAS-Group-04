# DAS-Group-04
hi
#load package
library(tidyverse)
library(ggplot2)
library(dplyr)
library(moderndive)
library(gapminder)
library(skimr)
library(corrplot)
library(ggcorrplot)
library(GGally)

household <- read.csv("dataset4.csv") 

household %>%
skim()

colnames(household)

selected_vars <- household[, sapply(household, is.numeric)]

# Scale the numerical variables
scaled_vars <- scale(selected_vars)

hosehold_num <- household[, sapply(household, is.numeric)]

hosehold_num <- household[, sapply(household, is.numeric)] %>%
select(-Total.Number.of.Family.members)

hosehold_num_with_Y <- household[, sapply(household, is.numeric)]

hist(household$Total.Number.of.Family.members)

## Plot the Correlation Matrix

cor_matrix <- cor(scaled_vars) %>% #create the correlation matrix of variables
  round(2)

cor_matrix

corrplot <- corrplot(cor_matrix, method = "color", addCoef.col = "gray",type = "upper",)

scaled_vars <- data.frame(scaled_vars)

ggpairs(scaled_vars)


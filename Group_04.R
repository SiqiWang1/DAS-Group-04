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
library(gridExtra)

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

colnames(household)
household[,"Electricity"] <- factor(household[,"Electricity"])

p1<-ggplot(data = household, mapping = aes(x = Household.Head.Sex, y = Total.Number.of.Family.members)) +
    geom_boxplot(aes(fill =Household.Head.Sex ) )+
   labs(x = "Sex", y = "Total.Number.of.Family.member",
        title = "gender")  + 
   scale_x_discrete(labels = c("female","male"))

p2<-ggplot(data = household, mapping = aes(x = Type.of.Household, y = Total.Number.of.Family.members)) +
   geom_boxplot(aes(fill =Type.of.Household ) )+
   labs(x = "tyoe of household", y = "Total.Number.of.Family.member",
         title = "type")  + 
    scale_x_discrete(labels = c("extend","single","two or more"))

p3<-ggplot(data = household, mapping = aes(x = Electricity, y = Total.Number.of.Family.members)) +
  geom_boxplot(aes(fill =Electricity ) )+
  labs(x = "Electricity", y = "Total.Number.of.Family.member",
       title = "Electricity")  + 
  scale_x_discrete(labels = c("no","yes"))

grid.arrange(p1, p2, p3, nrow=2, ncol = 2)
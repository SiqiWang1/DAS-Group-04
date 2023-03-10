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
library(janitor)
library(gridExtra)
library(MASS)
library(BMA)

# skim the dataset
household <- read.csv("dataset4.csv") 
household %>%
  skim()

household$Electricity <- as.factor(household$Electricity)
colnames(household)

# test if the distribution of y is poisson dist.

hist(household$Total.Number.of.Family.members)

#################### Plot the Correlation Matrix ####################

# get all numerical variables to check on the Correlation
hosehold_num <- household[, sapply(household, is.numeric)]

cor_matrix <- cor(hosehold_num) %>% #create the correlation matrix of variables
  round(2)
cor_matrix

corrplot <- corrplot(cor_matrix, method = "color", addCoef.col = "gray",type = "upper",)

ggpairs(hosehold_num)

# ### Scale the numerical variables
# hosehold_cat <- household %>%
#   select("Electricity","Household.Head.Sex","Type.of.Household")
# 
# scaled_vars <- data.frame(scale(hosehold_num)) %>%
#   select(-Total.Number.of.Family.members)
# 
# scaled_household <- cbind(hosehold_cat,scaled_vars,household$Total.Number.of.Family.members)


###################       Boxplot      ###################
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

###################   check on categorical date    ###################
household %>% 
  tabyl(Total.Number.of.Family.members,Household.Head.Sex) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() # To show original counts

###################        plot of histogram       ###################
# barplot of Total.Number.of.Family.members and Household.Head.Sex
H1 <- ggplot(household, aes(x= Total.Number.of.Family.members,  y = ..prop.., group=Household.Head.Sex, fill=Household.Head.Sex)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

# barplot of Total.Number.of.Family.members and Electricity 
H2 <- ggplot(household, aes(x= Total.Number.of.Family.members,  y = ..prop.., group=Electricity, fill=Electricity)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

# barplot of Total.Number.of.Family.members and Type.of.Household
H3 <- ggplot(household, aes(x= Total.Number.of.Family.members,  y = ..prop.., group=Type.of.Household, fill=Type.of.Household)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

grid.arrange(H1, H2, H3, ncol=1)

###################   model fitting   ###################

m1 <- glm(formula = Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + 
      Household.Head.Sex + Household.Head.Age + Type.of.Household + House.Floor.Area +
      House.Age + Number.of.bedrooms + Electricity, 
    family = poisson(link = "log"), data = household)

m2 <- glm(formula = Total.Number.of.Family.members ~ Total.Food.Expenditure, family = poisson(link = "log"), data = household)

summary(m1)
summary(m2)


# Use BIC to do variable selection
output <- bic.glm(Total.Number.of.Family.members ~ Total.Household.Income + Total.Food.Expenditure + 
                     Household.Head.Sex + Household.Head.Age + Type.of.Household + House.Floor.Area +
                     House.Age + Number.of.bedrooms + Electricity, 
                   glm.family = "poisson" , data = household)
summary(output)

# Try AIC (please check later)
m1.aic <- step(m1)

# Use the StepAIC function to perform a stepwise regression
# step.model.b <- stepAIC(m1, direction = "both")
# summary(step.model.b)




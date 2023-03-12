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

# check the skewness and kurtosis results

library(moments)
skewness(household$Total.Number.of.Family.members)
kurtosis(household$Total.Number.of.Family.members)

#Based on the skewness and kurtosis results, we can determine that the distribution of "y" does not conform to the assumption of a strict Poisson distribution. 
#Specifically, skewness values greater than 1 indicate that the data distribution is right-skewed, and kurtosis values greater than 3 indicate that the data distribution is sharper than the Poisson distribution.
#In such cases, a Negative Binomial Distribution (NBD) regression model may be considered, as it can be fitted when a Poisson regression model is not up to the task. 


#################### Plot the Correlation Matrix ####################

# Simplified column names 
colnames(household)[1]<-"Income"             # original name "Total.Household.Income" 
colnames(household)[3]<-"FoodExp"            # original name "Total.Food.Expenditure"   
colnames(household)[4]<-"Householder_Sex"    # original name "Household.Head.Sex"
colnames(household)[5]<-"Householder_Age"    # original name "Household.Head.Age"
colnames(household)[6]<-"Household_Type"     # original name "Type.of.Household"
colnames(household)[7]<-"Number_Members"     # original name "Total.Number.of.Family.members"
colnames(household)[8]<-"Floorarea"          # original name "House.Floor.Area" 
colnames(household)[10]<-"Number_bedrooms"   # original name "Number.of.bedrooms"

# get all numerical variables to check on the Correlation
household_num <- household[, sapply(household, is.numeric)]

cor_matrix <- cor(household_num) %>% #create the correlation matrix of variables
  round(2)
cor_matrix

corrplot <- corrplot(cor_matrix, method = "color", addCoef.col = "gray",type = "upper",)

ggpairs(hosehold_num)

# ### Scale the numerical variables
# hosehold_cat <- household %>%
#   select("Electricity","Household.Head.Sex","Type.of.Household")
# 
# scaled_vars <- data.frame(scale(household_num)) %>%
#   select(-Total.Number.of.Family.members)
# 
# scaled_household <- cbind(household_cat,scaled_vars,household$Total.Number.of.Family.members)


###################       Boxplot      ###################
p1<-ggplot(data = household, mapping = aes(x = Householder_Sex, y = Number_Members)) +
  geom_boxplot(aes(fill =Householder_Sex ) )+
  labs(x = "Householder_Sex", y = "Number_Members",
       title = "gender")  + 
  scale_x_discrete(labels = c("female","male"))

p2<-ggplot(data = household, mapping = aes(x = Household_Type, y = Number_Members)) +
  geom_boxplot(aes(fill = Household_Type))+
  labs(x = "Household_Type", y = "Number_Members",title = "type") +
  scale_x_discrete(labels = c("Extended", "Single", "Two or More"))

p3<-ggplot(data = household, mapping = aes(x = Electricity, y = Number_Members)) +
  geom_boxplot(aes(fill =Electricity ) )+
  labs(x = "Electricity", y = "Number_Members",
       title = "Electricity")  + 
  scale_x_discrete(labels = c("no","yes"))

grid.arrange(p1, p2, p3, nrow=3, ncol = 1)

###################   check on categorical date    ###################
household %>% 
  tabyl(Number_Members, Householder_Sex) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() # To show original counts

###################        plot of histogram       ###################
# barplot of Number_Members and Householder_Sex
H1 <- ggplot(household, aes(x= Number_Members,  y = ..prop.., group=Householder_Sex, fill=Householder_Sex)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

# barplot of Number_Members and Electricity 
H2 <- ggplot(household, aes(x= Number_Members,  y = ..prop.., group=Electricity, fill=Electricity)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

# barplot of Number_Members and Household_Type
H3 <- ggplot(household, aes(x= Number_Members,  y = ..prop.., group=Household_Type, fill=Household_Type)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

grid.arrange(H1, H2, H3, ncol=1)

###################   model fitting   ###################

m1 <- glm(formula = Number_Members ~ Income + FoodExp + Householder_Sex + 
            Householder_Age + Household_Type + Floorarea +
            House.Age + Number_bedrooms + Electricity, 
          family = poisson(link = "log"), data = household)

m2 <- glm(formula = Number_Members ~ FoodExp, family = poisson(link = "log"), data = household)

summary(m1)
summary(m2)


# Use BIC to do variable selection
output <- bic.glm(Number_Members ~ Income + FoodExp + Householder_Sex + 
                    Householder_Age + Household_Type + Floorarea +
                    House.Age + Number_bedrooms + Electricity, 
                   glm.family = "poisson" , data = household)
summary(output)

# Try AIC (please check later)
m1.aic <- step(m1)

# Use the StepAIC function to perform a stepwise regression
# step.model.b <- stepAIC(m1, direction = "both")
# summary(step.model.b)


# Negative Binomial Distribution：
library(MASS) 
m1 <- glm.nb(formula = Number_Members ~ Income + FoodExp + Householder_Sex + 
            Householder_Age + Household_Type + Floorarea +
            House.Age + Number_bedrooms + Electricity, data = household)
summary(model)


# Goodness-of-fit test
chisq <- with(m1, sum((household$Number_Members- fitted.values)^2/fitted.values))
df <- with(m1, df.residual)
p <- with(m1, pchisq(chisq, df, lower.tail = FALSE))
cat("Chi-square test statistic = ", chisq, "\n")
cat("df = ", df, "\n")
cat("p-value = ", p, "\n")



# The coef() function obtains the coefficients of the model.
# The confint() function obtains the confidence interval of the model coefficients.

exp(cbind(OR = coef(m1), confint(m1)))

#The OR value is equal to the regression coefficient of the indexed variable. 
#Specifically, if the coefficient of an explanatory variable in the regression model is b, its corresponding OR value is exp(b)




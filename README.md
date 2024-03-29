---
title: "Analysis of Factors Affecting Number of People in Household of Philippines"
author: "Siqi Wang, Yubin Lyu, Liting Wang, Han Xu, Jiahao Mao"
output:
  pdf_document: default
  github_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
options(scipen = 1, digits = 2) #set to two decimal
knitr::opts_chunk$set(echo = FALSE, eval = TRUE, warning = FALSE, message = FALSE, comment = NA)
```


```{r libraries}
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
library(moments)
library(kableExtra)
```

# Introduction

The Family Income and Expenditure Survey (FIES) is a significant source of data for understanding the wellbeing of households in Philippines. It provides valuable information on family income and expenditure, which can be used to investigate various research questions related to household characteristics.

In this analysis, we are interested in identifying which household-related factors influence the size of a household. Using Generalized Linear Model (GLM), we will explore the datasets obtained from the FIES survey for XII - SOCCSKSARGEN region in Philippines. The results of our analysis could help the government to make informed decisions related to household policies and other related matters.


## Data Processing
```{r data, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}
# Load data
household <- read.csv("dataset4.csv") 
# Factorize the categorical variables
household$Electricity <- as.factor(household$Electricity)
household$Household.Head.Sex <- as.factor(household$Household.Head.Sex)
household$Type.of.Household <- as.factor(household$Type.of.Household)
```

```{r rename, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}
# Simplified column names 
colnames(household)[1]<-"Income"             # original name "Total.Household.Income" 
colnames(household)[3]<-"FoodExp"            # original name "Total.Food.Expenditure"   
colnames(household)[4]<-"Householder_Sex"    # original name "Household.Head.Sex"
colnames(household)[5]<-"Householder_Age"    # original name "Household.Head.Age"
colnames(household)[6]<-"Household_Type"     # original name "Type.of.Household"
colnames(household)[7]<-"Number_Members"     # original name "Total.Number.of.Family.members"
colnames(household)[8]<-"Floorarea"          # original name "House.Floor.Area" 
colnames(household)[10]<-"Number_bedrooms"   # original name "Number.of.bedrooms"
household$Household_Type <- ifelse(household$Household_Type == "Two or More Nonrelated Persons/Members", "two or more", household$Household_Type)     # change the long type name "Two or More Unrelated Persons/Members"
```

## Data Summary
```{r skim, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}
# Summary of Categorical Variables
household_cat <- household %>%
   dplyr::select("Electricity","Householder_Sex","Household_Type")
summary(household_cat)

# Summary of Numerical Variables
household_num <- household[, sapply(household, is.numeric)]
my_skim <- skim_with(base = sfl(n = length))
household_num %>%
  my_skim() %>%
  transmute(Variable=skim_variable, n=n, 
            Mean = format(signif(numeric.mean, 3), scientific = TRUE, digits = 2), 
            SD = format(signif(numeric.sd, 3), scientific = TRUE, digits = 2),
            Min= format(signif(numeric.p0, 3), scientific = TRUE, digits = 2), 
            Median=format(signif(numeric.p50, 3), scientific = TRUE, digits = 2),  
            Max=format(signif(numeric.p100, 3), scientific = TRUE, digits = 2), 
            IQR = format(signif(numeric.p75-numeric.p50, 3), scientific = TRUE, digits = 2) ) %>%
   kable(caption = '\\label{tab:summarybyskim} Summary statistics of variables',
         booktabs = TRUE, linesep = "", digits = 2) %>%
   kable_styling(font_size = 10, latex_options = "hold_position")
```


# Distribution Check

Test if the distribution of y follows the poisson distribution.

```{r pcheck, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}
# If a variable follows poisson distribution variance will equal to mean.
print(var(household$Number_Members))
print(mean(household$Number_Members))

# As we can see there is no significant difference between mean and variance
# Plot the histogram of response variable and compare it with poisson distribution.
hist(household$Number_Members, freq = FALSE, xlab = "Data", 
     main = "Histogram of Number of Family members")
# Overlay a Poisson probability mass function
x <- 0:max(household$Number_Members)
lines(x, dpois(x, lambda = 4), col = "red")
```
Based on the plot we can see that the distribution follows poisson dist when lambda = 4

# Correlation Matrix and ggpairs

```{r correlation, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}
# Create the correlation matrix of variables
cor_matrix <- cor(household_num) %>% 
  round(2)
cor_matrix

corrplot <- corrplot(cor_matrix, method = "color", addCoef.col = "gray",type = "upper",)
# Create the ggpairs of variables
ggpairs(household_num)

```

# Boxplots of Variables

```{r boxplot, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, out.width = '60%'}
# Explanatory analysis on numeric variables
ggplot(data = household, mapping = aes(x = Householder_Sex, y = Number_Members)) +
  geom_boxplot(aes(fill =Householder_Sex ) )+
  labs(x = "Householder_Sex", y = "Number_Members",
       title = "gender")  + 
  scale_x_discrete(labels = c("female","male"))

ggplot(data = household, mapping = aes(x = Household_Type, y = Number_Members)) +
  geom_boxplot(aes(fill = Household_Type))+
  labs(x = "Household_Type", y = "Number_Members",title = "type") +
  scale_x_discrete(labels = c("Extended", "Single", "Two or More"))+
  theme(legend.position = "bottom")

ggplot(data = household, mapping = aes(x = Electricity, y = Number_Members)) +
  geom_boxplot(aes(fill =Electricity ) )+
  labs(x = "Electricity", y = "Number_Members",
       title = "Electricity")  + 
  scale_x_discrete(labels = c("no","yes"))
```


# Histogram of Variables

```{r histogram, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE,out.width = '60%'}
# Explanatory analysis on categorical variables
household %>% 
  tabyl(Number_Members, Householder_Sex) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() # To show original counts

# barplot of Number_Members and Householder_Sex
ggplot(household, aes(x= Number_Members,  y = ..prop.., group=Householder_Sex, fill=Householder_Sex)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

# barplot of Number_Members and Electricity 
ggplot(household, aes(x= Number_Members,  y = ..prop.., group=Electricity, fill=Electricity)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion")

# barplot of Number_Members and Household_Type
ggplot(household, aes(x= Number_Members,  y = ..prop.., group=Household_Type, fill=Household_Type)) + 
  geom_bar(position="dodge", stat="count") +
  labs(y = "Proportion") +
  theme(legend.position = "bottom")

```

# Model Fitting

Since our response variable follows poisson distribution, we will use glm with poisson distribution to fit the model.However, the regression coefficients for "household income" and "food expenditure" were found to be smaller than expected, possibly due to the large scale of these variables. To address this issue, a log transformation was applied to these variables, which effectively normalized their scale and improved the accuracy of the regression coefficients.

```{r poissson, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}
#model1 with all variables
m1 <- glm(formula = Number_Members ~ Income + FoodExp + Householder_Sex + 
            Householder_Age + Household_Type + Floorarea +
            House.Age + Number_bedrooms + Electricity, 
          family = poisson(link = "log"), data = household)


#model2 with log transformation for Income and FoodExp
m2 <- glm(formula = Number_Members ~ log(Income) + log(FoodExp) +Householder_Sex + 
            Householder_Age + Household_Type + Floorarea +
            House.Age + Number_bedrooms + Electricity, 
          family = poisson(link = "log"), data = household)

summary(m1)
summary(m2)

```

## Use BIC to do variable selection

BIC is implemented to found the best fitting model. In the process of model selection, the posterior probability can be utilized to evaluate the impact of each explanatory variable on the response variable and to facilitate the selection of the best model.
The results show that model 1 has the highest posterior probability of 0.61, suggesting that it is the most suitable model for explaining the response variable.

```{r BIC, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}

output <- bic.glm(Number_Members ~ log(Income) + log(FoodExp) + Householder_Sex + 
                    Householder_Age + Household_Type + Floorarea +
                    House.Age + Number_bedrooms + Electricity, 
                   glm.family = "poisson" , data = household)

summary(output)

# Name the best model selected by BIC m3
m3 <- glm(Number_Members ~ log(Income) + log(FoodExp) + Householder_Sex + 
            Householder_Age + Household_Type +
            House.Age + Electricity, 
          family = "poisson" , data = household)
summary(m3)
    
```

## Negative Binomial Distribution

The variance(4.9) of y is slightly larger than the mean(4.5) of y, therefore a Negative Binomial Distribution model is fitted to reduce the issue of overdispersion.  

```{r NB, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}

m4 <- glm.nb(formula = Number_Members ~ log(Income) + log(FoodExp) + Householder_Sex + 
            Householder_Age + Household_Type + Floorarea +
            House.Age + Number_bedrooms + Electricity, data = household)
summary(m4)

```

# Deviance plots

Deviance plots is shown below.

```{r deviance, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}
resp <- resid(m1, type = "pearson") 
resd <- resid(m1, type = "deviance")
r1<- ggplot(m1, aes(sample = resp)) + geom_point(stat = "qq", color = "#7fc97f") + ylab("Pearson residuals")
r2<- ggplot(m1, aes(sample = resd)) + geom_point(stat = "qq", color = "#7fc97f") + ylab("Deviance residuals")
r3<- ggplot(m1, aes(x = predict(m1, type="link"), y =resd))+ geom_point(col = "#7fc97f") +
  ylab("Deviance residuals") + xlab("Linear predictor") 
grid.arrange(r1, r2, r3, nrow = 1)

resp2 <- resid(m2, type = "pearson") 
resd2 <- resid(m2, type = "deviance")
r4<- ggplot(m2, aes(sample = resp2)) + geom_point(stat = "qq", color = "#7fc97f") + ylab("Pearson residuals")
r5<- ggplot(m2, aes(sample = resd2)) + geom_point(stat = "qq", color = "#7fc97f") + ylab("Deviance residuals")
r6<- ggplot(m2, aes(x = predict(m2, type="link"), y =resd2))+ geom_point(col = "#7fc97f") +
  ylab("Deviance residuals") + xlab("Linear predictor") 
grid.arrange(r4, r5, r6, nrow = 1)
```

# Model Evaluation

```{r model_evaluation, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}
# Poisson model
c(m1$deviance, m1$aic)
# poission model with log transformation
c(m2$deviance, m2$aic)
# BIC model
c(m3$deviance, m3$aic)
# Negative binomial model
c(m4$deviance, m4$aic)
```

# Goodness-of-fit test

```{r GOF, echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE}

chisq <- with(m1, sum((household$Number_Members- fitted.values)^2/fitted.values))
df <- with(m1, df.residual)
p <- with(m1, pchisq(chisq, df, lower.tail = FALSE))
cat("Chi-square test statistic = ", chisq, "\n")
cat("df = ", df, "\n")
cat("p-value = ", p, "\n")


# The coef() function obtains the coefficients of the model.
# The confint() function obtains the confidence interval of the model coefficients.

exp(cbind(OR = coef(m1), confint(m1)))

```

In Poisson regression, OR (odds ratio) represents the probability ratio (probability ratio) of a set of variables, which is the ratio of the probability of a dependent variable between the levels of two different independent variables. Usually, a larger value of OR means that a variable has a greater effect on the dependent variable. In exp(cbind(OR = coef(model), confint(model))), coef(model) gives the coefficients of all the variables and exp converts them to OR values.

The OR value is equal to the regression coefficient of the indexed variable. The coefficient of an explanatory variable (Householder_SexMale) in the regression model is 1.30, its corresponding OR value is exp(b).
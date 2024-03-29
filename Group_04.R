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
library(moments)

# skim the dataset
household <- read.csv("dataset4.csv")

household$Electricity <- as.factor(household$Electricity)
household$Household.Head.Sex <- as.factor(household$Household.Head.Sex)
household$Type.of.Household <- as.factor(household$Type.of.Household)
 
skim(household)


# Test if the distribution of y is poisson dist.
print(var(household$Total.Number.of.Family.members))
# [1] 4.906328
print(mean(household$Total.Number.of.Family.members))
# [1] 4.532045

hist(household$Total.Number.of.Family.members, freq = FALSE, xlab = "Data", main = "Histogram of Number of Family members")
# Overlay a Poisson probability mass function
x <- 0:max(data)
lines(x, dpois(x, lambda = 4), col = "red")

# based on the plot we can see that the distribution follows poisson dist when lambda = 4


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

ggpairs(household_num)

# ### Scale the numerical variables
# household_cat <- household %>%
#   dplyr::select("Electricity","Householder_Sex","Household_Type")
# 
# scaled_vars <- data.frame(scale(household_num)) %>%
#    dplyr::select(-Number_Members)
#  
# scaled_household <- cbind(household_cat,scaled_vars,household$Number_Members) %>%
#   rename("Number_Members" ="household$Number_Members")


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

#model with all variables
m1 <- glm(formula = Number_Members ~ Income + FoodExp + Householder_Sex + 
            Householder_Age + Household_Type + Floorarea +
            House.Age + Number_bedrooms + Electricity, 
          family = poisson(link = "log"), data = household)

#model with log transformation of Income and FoodExp
m2 <- glm(formula = Number_Members ~ log(Income) + log(FoodExp) +Householder_Sex + 
            Householder_Age + Household_Type + Floorarea +
            House.Age + Number_bedrooms + Electricity, 
          family = poisson(link = "log"), data = household)

summary(m1)
summary(m2)

# check on overdispersion
deviance(m1)/df.residual(m1)
deviance(m2)/df.residual(m2)

ggplot(m1, aes(x=log(fitted(m2)), y=log((household$Number_Members-fitted(m2))^2)))+ 
  geom_point(col="#f46d43") +
  geom_abline(slope=1, intercept=0, col="#a6d96a", linewidth=1) + 
  ylab(expression((y-hat(mu))^2)) + 
  xlab(expression(hat(mu)))


# Use BIC to do variable selection
output <- bic.glm(Number_Members ~ log(Income) + log(FoodExp) + Householder_Sex + 
                    Householder_Age + Household_Type + Floorarea +
                    House.Age + Number_bedrooms + Electricity, 
                   glm.family = "poisson" , data = household)
summary(output)

bic <- glm(Number_Members ~ log(Income) + log(FoodExp) + Householder_Sex + 
            Householder_Age + Household_Type +
            House.Age + Electricity, 
          family = "poisson" , data = household)

# Try AIC (please check later)
# m1.aic <- step(m1, direction = "forward"))

# Use the StepAIC function to perform a stepwise regression
# step.model.b <- stepAIC(m1, direction = "both")
# summary(step.model.b)

# # Perform forward selection
# forward_model <- stepAIC(m2, direction = "forward")
# 
# # Perform backward elimination
# backward_model <- stepAIC(m2, direction = "backward")
# 
# # Compare the models and select the final model
# final_model <- ifelse(AIC(forward_model) < AIC(backward_model), forward_model, backward_model)
# 
# summary(backward_model)

# Negative Binomial Distribution：
m3 <- glm.nb(formula = Number_Members ~ Income + FoodExp + Householder_Sex + 
            Householder_Age + Household_Type + Floorarea +
            House.Age + Number_bedrooms + Electricity, data = household)
summary(m3)

###################       deviance plots        ################
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
grid.arrange(p4, p5, p6, nrow = 1)

###################       model comparison    ################

# Poisson model
c(m1$deviance, m1$aic)
# poission model with log transformation
c(m2$deviance, m2$aic)
# Negative binomial model
c(m3$deviance, m3$aic)
# BIC model
c(bic$deviance, bic$aic)

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




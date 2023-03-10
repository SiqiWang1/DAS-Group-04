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



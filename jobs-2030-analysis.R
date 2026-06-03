
install.packages("tidyverse")   # data wrangling + ggplot2 for viz
install.packages("skimr")       # beautiful summary statistics
install.packages("janitor")     # cleaning column names
install.packages("corrplot")    # correlation matrix visualization
install.packages("randomForest") # for modeling later

# Run this EVERY session (loads into memory)
library(tidyverse)
library(skimr)
library(janitor)
library(corrplot)
library(randomForest)

df <- read.csv("AI_Impact_on_Jobs_2030.csv")
head(df)
str(df)
summary(df)
skim(df)


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

# diagnose everthing in the data set outler and everthing.
diagnose_column <- function(df, col) {
  x <- df[[col]]
  
  m    <- mean(x, na.rm = TRUE)
  s    <- sd(x, na.rm = TRUE)
  cv   <- (s / m) * 100
  
  q25  <- quantile(x, 0.25, na.rm = TRUE)
  q75  <- quantile(x, 0.75, na.rm = TRUE)
  iqr  <- q75 - q25
  
  lower <- q25 - (1.5 * iqr)
  upper <- q75 + (1.5 * iqr)
  
  outliers <- sum(x < lower | x > upper, na.rm = TRUE)
  
  cat("Column        :", col, "\n")
  cat("Mean          :", round(m, 2), "\n")
  cat("Median        :", round(median(x, na.rm = TRUE), 2), "\n")
  cat("SD            :", round(s, 2), "\n")
  cat("CV            :", round(cv, 2), "%\n")
  cat("IQR           :", round(iqr, 2), "\n")
  cat("Lower fence   :", round(lower, 2), "\n")
  cat("Upper fence   :", round(upper, 2), "\n")
  cat("Outliers found:", outliers, "\n")
  cat("Spread        :", ifelse(cv < 15, "Low", 
                                ifelse(cv < 30, "Moderate", 
                                       ifelse(cv < 50, "High", "Very High"))), "\n")
}
## for all the columns
numeric_cols <- df %>% 
  select(where(is.numeric)) %>% 
  names()

for (col in numeric_cols) {
  diagnose_column(df, col)
  cat("-------------------\n")
}

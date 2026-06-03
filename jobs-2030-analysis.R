
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

# job title by industry 
df %>%
  count(Industry, Job_Title) %>%
  ggplot(aes(x = reorder(Industry, n), y = n, fill = Job_Title)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Job Titles Distribution Across Industries",
    x = "Industry",
    y = "Number of Jobs",
    fill = "Job Title"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
# 1a number of jobs by industry 
df %>%
  count(Industry) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Industry, n), y = n)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  coord_flip() +
  labs(
    title = "Number of Jobs by Industry",
    x = "Industry",
    y = "Count"
  ) +
  theme_minimal()

#1b most common job titles 
df %>%
  count(Job_Title) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Job_Title, n), y = n)) +
  geom_bar(stat = "identity", fill = "#55A868") +
  coord_flip() +
  labs(
    title = "Most Common Job Titles",
    x = "Job Title",
    y = "Count"
  ) +
  theme_minimal()


#chapter1b future demad score by job title 
df %>%
  group_by(Job_Title) %>%
  summarise(avg_demand = mean(Future_Demand_Score, na.rm = TRUE)) %>%
  arrange(desc(avg_demand)) %>%
  ggplot(aes(x = reorder(Job_Title, avg_demand), y = avg_demand)) +
  geom_bar(stat = "identity", fill = "#E87D2B") +
  coord_flip() +
  labs(
    title = "Average Future Demand Score by Job Title",
    x = "Job Title",
    y = "Average Demand Score"
  ) +
  theme_minimal()

# chapter1.3 hiring trend 2026 industry
df %>%
  count(Industry, Hiring_Trend_2026) %>%
  group_by(Industry) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(Industry, pct), y = pct, fill = Hiring_Trend_2026)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Hiring Trend 2026 by Industry",
    x = "Industry",
    y = "Percentage (%)",
    fill = "Hiring Trend"
  ) +
  scale_fill_manual(values = c(
    "Growing"  = "green",
    "Stable"   = "blue", 
    "Declining" = "red"
  )) +
  theme_minimal() +
  theme(legend.position = "bottom")

#chapter2.1 #AI replacement with the risk 
df %>%
  group_by(Industry) %>%
  summarise(avg_risk = mean(AI_Replacement_Risk, na.rm = TRUE)) %>%
  arrange(desc(avg_risk)) %>%
  ggplot(aes(x = reorder(Industry, avg_risk), y = avg_risk)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(
    title = "Average AI Replacement Risk by Industry",
    x = "Industry",
    y = "Average Risk Score"
  ) +
  theme_minimal()



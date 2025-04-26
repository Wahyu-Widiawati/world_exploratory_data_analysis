# World Happiness Exploratory Data Analysis
# Author: Wahyu Widiawati
# Date: 2023-03-19

# --- 1. Introduction ---
# Metadata:
# - Data Source: https://worldhappiness.report/
# - Data Range: 2018-2022

# World happiness is evaluated by several parameters such as:
# GDP per capita, Social support, Healthy life expectancy, Freedom to make life choices, Generosity, and Perceptions of corruption.

# Analysis Questions:
# 1. How have the rankings changed over time?
# 2. What factors are most strongly correlated with happiness scores?

# --- 2. Preparation ---
## 2.1 Load Libraries
library(dplyr)
library(ggplot2)
library(corrplot)

## 2.2 Import Datasets
df_2018 <- read.csv("dataset_2018.csv")
df_2019 <- read.csv("dataset_2019.csv")
df_2020 <- read.csv("dataset_2020.csv")
df_2021 <- read.csv("dataset_2021.csv")
df_2022 <- read.csv("dataset_2022.csv")

# --- 3. Data Cleaning and Formatting ---
## 3.1 List Fields
names(df_2018)
names(df_2019)
names(df_2020)
names(df_2021)
names(df_2022)

## 3.2 Remove Unnecessary Fields
df_2020 <- df_2020[, -c(3,5:14,21)]
df_2021 <- df_2021[, -c(3,5:14,21)]
df_2022 <- df_2022[, -c(4:6)]

## 3.3 Standardize Column Names
colnamesconv <- names(df_2018)
colnames(df_2020) <- colnamesconv
colnames(df_2021) <- colnamesconv
colnames(df_2022) <- colnamesconv

## 3.4 Check Missing Values
sum(is.na(df_2018))
sum(is.na(df_2019))
sum(is.na(df_2020))
sum(is.na(df_2021))
sum(is.na(df_2022))

## 3.5 Handle Missing Values
which(is.na(df_2022), arr.ind = TRUE)
df_2022 <- na.omit(df_2022)

## 3.6 Add Year Field
df_2018$Year <- "2018"
df_2019$Year <- "2019"
df_2020$Year <- "2020"
df_2021$Year <- "2021"
df_2022$Year <- "2022"

## 3.7 Bind Datasets
dfAll <- rbind(df_2018, df_2019, df_2020, df_2021, df_2022)

# --- 4. Exploratory Data Analysis ---
## 4.1 Descriptive Statistics

### 4.1.1 Overview
glimpse(dfAll)
unique(dfAll$Country.or.region)
head(df_2018)
head(df_2022)

### 4.1.2 Mean Happiness Score
dfAll_mean_by_year <- dfAll %>%
  group_by(Year) %>%
  summarize(mean_score = mean(Score))

dfAll_mean <- dfAll %>%
  summarize(overall_mean = mean(Score))

# Convert Year to numeric
dfAll_mean_by_year$Year <- as.numeric(dfAll_mean_by_year$Year)

# Plot Mean Happiness Score
ggplot(dfAll_mean_by_year, aes(x = Year, y = mean_score)) +
  geom_line(color = "blue") +
  labs(title = "Mean of Happiness Scores by Year", x = "Year", y = "Mean Score")

### 4.1.3 Median Happiness Score
dfAll_median_by_year <- dfAll %>%
  group_by(Year) %>%
  summarize(median_score = median(Score))

dfAll_median <- dfAll %>%
  summarize(overall_median = median(Score))

# Plot Median Happiness Score
dfAll_median_by_year$Year <- as.numeric(dfAll_median_by_year$Year)
ggplot(dfAll_median_by_year, aes(x = Year, y = median_score)) +
  geom_line(color = "blue") +
  labs(title = "Median of Happiness Scores by Year", x = "Year", y = "Median Score")

### 4.1.4 Mode (Histogram)
hist(dfAll$Score, breaks = 5, col = "darkblue", labels = TRUE,
     ylim = c(0, 250), xlab = "Score", ylab = "Frequency",
     main = "Distribution of Happiness Scores")

## 4.2 Multivariate Analysis

### 4.2.1 Top 5 Rankings Over Time
dfAll$Year <- as.numeric(dfAll$Year)
dfAll_rank <- dfAll %>%
  select(Year, Country.or.region, Overall.rank) %>%
  filter(Overall.rank <= 5)

# Plot Top 5 Rankings
ggplot(dfAll_rank, aes(x = Year, y = Overall.rank, color = Country.or.region)) +
  geom_line() +
  labs(title = "Top 5 Countries' Happiness Rankings (2018-2022)", x = "Year", y = "Rank", color = "Country") +
  theme_bw()

### 4.2.2 Correlation Analysis
subset_dfAll <- dfAll %>%
  select(Score, GDP.per.capita, Social.support, Healthy.life.expectancy,
         Freedom.to.make.life.choices, Generosity, Perceptions.of.corruption)

# Convert to numeric and remove NA
subset_dfAll$Perceptions.of.corruption <- as.numeric(subset_dfAll$Perceptions.of.corruption)
subset_dfAll <- na.omit(subset_dfAll)

# Correlation Matrix
corr_matrix <- cor(subset_dfAll)

# Correlation Plot
corrplot(corr_matrix, method = "color", type = "full", tl.col = "black",
         tl.srt = 45, tl.cex = 0.8, col = colorRampPalette(c("#ADD8E6", "#000080"))(50),
         addCoef.col = "white", number.cex = 0.7)

# --- End of Script ---

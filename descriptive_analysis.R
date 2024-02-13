# Author: Nermin Ghith


# Descriptive Data Analysis

# Load necessary libraries
library(haven)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)

# Load the data
data <- read_dta("data_43291_Stata.dta")

# Basic summary of data
skim(data)

# Summary for specific conditions
summary(subset(data, poorarea == 1))
summary(subset(data, richarea == 1))

# Descriptive statistics by neighborhood type
data %>%
  mutate(agegr = as.factor(agegroup)) %>%
  group_by(richarea, agegr) %>%
  summarise(across(c(psycmed, private, poor, male), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"))

# Author: Nermin Ghith

# Mathematical Modeling

# Load necessary libraries
library(haven)
library(dplyr)
library(lme4)
library(pROC)
library(performance)
library(ggplot2)

# Load the data
data <- read_dta("data_43291_Stata.dta")

# Simple logistic regression
model1 <- glm(psycmed ~ male + as.factor(agegroup) + poor, data = data, family = "binomial")

# Odds ratios for simple logistic regression
exp(coef(model1))

# Multilevel logistic regression
model2 <- glmer(psycmed ~ male + as.factor(agegroup) + poor + (1 | neigh), data = data, family = "binomial")

# Odds ratios for multilevel logistic regression
exp(fixef(model2))

# ROC analysis for simple logistic regression
pred1 <- predict(model1, type = "response")
roc1 <- roc(data$psycmed, pred1)
auc1 <- auc(roc1)

# ROC analysis for multilevel logistic regression
pred2 <- predict(model2, type = "response", re.form = NULL)
roc2 <- roc(data$psycmed, pred2)
auc2 <- auc(roc2)

# Intraclass Correlation Coefficient (ICC) for multilevel models
icc(model2)

# Plotting ROC curves
plot(roc1, col = "red")
lines(roc2, col = "blue")

# Generating rankings and plotting for neighborhood effects
data$predict_effect <- predict(model2, re.form = NULL)  # Predictions for fixed effects
data$rank <- rank(data$predict_effect)

ggplot(data, aes(x = rank, y = predict_effect)) +
  geom_point() +
  geom_errorbar(aes(ymin = predict_effect - 1.96 * se, ymax = predict_effect + 1.96 * se), width = 0.1)

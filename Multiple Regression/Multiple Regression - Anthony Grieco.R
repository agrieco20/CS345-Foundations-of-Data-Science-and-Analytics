#Author: Anthony Grieco
#Date: 4/23/2024
#Class: Foundations of Data Science and Analytics

library(tidyverse)
library(GGally)

evals <- read_csv("evals.csv")

glimpse(evals)
summary(evals)

#-------------------------
#1. Describe the distribution of score. Is the distribution skewed? What does that tell you about how students rate courses? Is this what you expected to see? Why, or why not?
evals %>%
  select(contains("score")) %>%
  ggpairs()

#-------------------------
#2. (R only) Run the following linear model. Drop one variable at a time and peek at the adjusted R-squared. Removing which variable increases adjusted R-squared the most?
model  <- lm(score ~ rank + gender + ethnicity + language + age + cls_perc_eval
             + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(model)

#Removing "cls_profs" increased adjusted R-Squared the Most

#-------------------------
#3. (R only) Using backward-selection and adjusted R-squared as the selection criterion, determine the best model. 
model  <- lm(score ~ gender + ethnicity + language + age + cls_perc_eval
             + cls_students + cls_credits + bty_avg, data = evals)
summary(model)

#-------------------------
#4. Write out the linear model for predicting score based on the final model you settled on in #3.
#See Accompanying Word Document

#-------------------------
#5. Based on your final model, describe the characteristics of a professor and course at University of Texas at Austin that would be associated with a high evaluation score.
#See Accompanying Word Document

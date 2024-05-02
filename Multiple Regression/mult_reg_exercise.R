library(tidyverse)

evals <- read_csv("evals.csv")

glimpse(evals)
summary(evals)

evals %>% distinct(language)
evals %>% distinct(ethnicity)

#Explore potential relationship between bty_avg and score

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_point()

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter()

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  geom_smooth(method = "lm")

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

cor(evals$bty_avg, evals$score)

#potential collinearity

#The data set contains several variables on the 
#beauty score of the professor: 
#individual ratings from each of the six students 
#who were asked to score the physical appearance 
#of the professors and the average of these six scores. 
#Let’s take a look at the relationship between one of 
#these scores and the average beauty score.

ggplot(data = evals, aes(x = bty_f1lower, y = bty_avg)) +
  geom_point()

cor(evals$bty_avg, evals$bty_f1lower)

install.packages("GGally")
library(GGally)

evals %>%
  select(contains("bty")) %>%
  ggpairs()

#split by gender

evals %>%
  filter(gender=="female") %>%
  ggplot(aes(x = bty_avg, y = score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

evals %>%
  filter(gender=="female") %>%
  summarize(cor(bty_avg, score))

evals %>%
  filter(gender=="male") %>%
  ggplot(aes(x = bty_avg, y = score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)

evals %>%
  filter(gender=="male") %>%
  summarize(cor(bty_avg, score))

#Multiple regression model

model <- lm(score ~ bty_avg + gender, data = evals)
summary(model)

#talk about gendermale=1

model <- lm(score ~ rank + gender + ethnicity + language + age + cls_perc_eval 
            + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(model)

#drop variables

model <- lm(score ~ rank + gender + ethnicity + language + age + cls_perc_eval 
            + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(model)

#Drop one variable at a time and peek at the adjusted R2. 
#Removing which variable increases adjusted R2 the most? 
#Drop the variable with the highest p-value and re-fit the model. 
#Did the coefficients and significance of the other explanatory 
#variables change with this variable removed? 
#(One of the things that makes multiple regression 
#interesting is that coefficient estimates depend on the other 
#variables that are included in the model.) If not, what does 
#this say about whether or not the dropped variable was collinear 
#with the other explanatory variables?
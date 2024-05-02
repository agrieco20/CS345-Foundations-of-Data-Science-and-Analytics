library(tidyverse)

NBA <- read_csv("NBA_TEAM_STATS.csv")

summary(NBA)

#Trying to Predict Win Percentage

NBA <- NBA %>% 
  mutate(win_per = W/(W+L))

#Creating new 3 Point Percentage column (trying to see whether greater 3PP impacts win rate)
NBA <- NBA %>%
  mutate(`3PP` = `3PM`/`3PA`)

ggplot(NBA, aes(x=`3PP`, y = win_per)) +
  geom_point()

linmod <- lm(win_per ~ `3PP`, data = NBA) #Creates linear regression of win percentage compared to successful 3 point percentage
summary(linmod)

linmod2 <- lm(win_per ~ `3PP` + YEAR_END, data=NBA)
summary(linmod2)

modNBA <- NBA %>%
  filter(NBA$YEAR_END > 2012)

linmod3 <- lm(win_per ~ `3PP`, data=modNBA)
summary(linmod3)

cor(modNBA$win_per, modNBA$`3PP`) #Checks how correlated the data is (Multiple R-Squared)

oldNBA <- NBA %>%
  filter(NBA$YEAR_END < 2013)

linmod4 <- lm(win_per ~ `3PP`, data=oldNBA)
summary(linmod4)

cor(oldNBA$win_per, oldNBA$`3PP`)

#In the modern NBA, successful 3PP is a greater indicator of win percentage than it is in the old because there is a significantly higher correlation between the two in modern NBA 

#Author: Anthony Grieco
#Date: 3/27/2024
#Class: Foundations of Data Science and Analytics

library(tidyverse)
library(dplyr)

nonvoters <- read_csv("nonvoters_data.csv") #2

summary(nonvoters)

#------------------------------------------------------------
#3 (There are a total of 5,836 people with unique identifies who responded to the survey)
numRespond <- nonvoters %>%
  group_by(RespId) %>%
  summarize(n=n())
count(numRespond)

#------------------------------------------------------------
glimpse(nonvoters) #4 (There are a large number of "NA" responses for Q22, Q29_1, Q29_2, Q29_3, Q29_4, Q29_5, Q29_6, Q29_7, Q29_8, Q29_9, Q29_10, Q30, Q31, Q32, and Q33 among others)

mean(nonvoters$ppage) #6

nonvoters %>% distinct(voter_category) #7

nonvoters %>% count(voter_category) #8

nonvoters %>% distinct(income_cat) #9

nonvoters %>% count(income_cat) #10

#------------------------------------------------------------
#11 (There is a value that was entered ("-1") which shouldn't be possible considering that the possible entries could only range from 1 to 4 in the codebook)
ggplot(nonvoters, aes(x=Q2_3)) +
  geom_histogram()

#------------------------------------------------------------
#12 - Filters out anomalous data (represented as "-1") that was entered during questions (columns) in the survey
nonvoters <- nonvoters %>% 
  filter(Q5 != -1) %>% 
  filter(Q21 != -1) %>%
  filter(Q25 != -1) %>%
  filter(Q26 != -1) %>%
  filter(Q30 != -1)
nonvoters
#------------------------------------------------------------
#13
nonvoters %>%
  filter(Q16 > 0) %>%
  ggplot(aes(x=Q16)) +
  geom_histogram()

nonvoters %>%
  filter(Q22 > 0) %>%
  ggplot(aes(x=Q22)) +
  geom_histogram()

nonvoters %>%
  filter(Q24 > 0) %>%
  ggplot(aes(x=Q24)) +
  geom_histogram()

#------------------------------------------------------------
#14
republicans <- nonvoters %>% 
  filter(Q30 == 1)
count(republicans)

#------------------------------------------------------------
#15
s_repub <- nonvoters %>% 
  filter(Q31 == 1)
count(s_repub)

#------------------------------------------------------------
#16

#Percentage of Identified Republicans Amongst Total Respondents
percentRepub <- count(republicans) / sum(nonvoters %>% count(Q30))
percentRepub <- percentRepub * 100
percentRepub <- paste(round(percentRepub, digits = 2), "%", sep="")
percentRepub

#Percentage of Identified Strong Republicans Amongst Total Identified Republican Respondents
percent_StrRepub <- count(s_repub) / count(republicans)
percent_StrRepub <- percent_StrRepub * 100
percent_StrRepub <- paste(round(percent_StrRepub, digits = 2), "%", sep="")
percent_StrRepub

#------------------------------------------------------------
#17
#Race
ggplot(nonvoters, aes(x=voter_category, fill = race)) +
  geom_bar()

#Income
ggplot(nonvoters, aes(x=voter_category, fill = income_cat)) +
  geom_bar()

#Age
ageRange <- cut(nonvoters$ppage, 5) #Creates 5 equally spaced age ranges (roughly 15 years each) that the age data from the survey (ppage) can automatically be filtered by 

ggplot(nonvoters, aes(x=voter_category, fill = ageRange)) +
  geom_bar()

#Education
ggplot(nonvoters, aes(x=voter_category, fill = educ)) +
  geom_bar()

#Party ID
party <- as.factor(nonvoters$Q30)
party

ggplot(nonvoters, aes(x=voter_category, fill = party)) +
  geom_bar()

#------------------------------------------------------------
#18

  #1.Voter Category related to Gender
ggplot(nonvoters, aes(x=voter_category, fill = gender)) +
  geom_bar()

  #2.Voter Category related to a Whether Respondent Regularly Votes in the National Election
regVote <- as.factor(nonvoters$Q26)

ggplot(nonvoters, aes(x=voter_category, fill = regVote)) +
  geom_bar()

  #3.Voter Category related to How Closely Respondent is Following the 2020 Presidential Race
closeFollow <- as.factor(nonvoters$Q25)

ggplot(nonvoters, aes(x=voter_category, fill = closeFollow)) +
  geom_bar()

  #4.Voter Category related to Whether Respondent is Planning on voting in the 2020 Presidential Race
planVote <- as.factor(nonvoters$Q21)

ggplot(nonvoters, aes(x=voter_category, fill = planVote)) +
  geom_bar()

  #5.Voter Category related to Whether Respondent thinks Outcome of the 2020 Presidential Race will Change Their Life
lifeChange <- as.factor(nonvoters$Q5)

ggplot(nonvoters, aes(x=voter_category, fill = lifeChange)) +
  geom_bar()

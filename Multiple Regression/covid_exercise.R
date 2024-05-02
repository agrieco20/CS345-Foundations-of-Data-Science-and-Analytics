library(tidyverse)

covid <- read_csv("covid.csv")

glimpse(covid)
summary(covid)
covid %>% distinct(age_group)
covid %>% distinct(vaccine_status)

#Calculate mortality rate by vaccination status

covid_summary <- covid %>%
  group_by(vaccine_status, outcome) %>%
  summarize(count = n())
  
covid_summary <- covid_summary %>%
  group_by(vaccine_status) %>%
  mutate(total = sum(count))

covid_summary <- covid_summary %>%
  mutate(percent = 100 * count/total)

covid_summary %>%
  filter(outcome == "death") %>%
  ggplot(aes(x=vaccine_status, y=percent)) +
  geom_bar(stat="identity")

#Calculate mortality rate by age group and vaccination status

covid_summary2 <- covid %>%
  group_by(age_group, vaccine_status, outcome) %>%
  summarize(count = n())

covid_summary2 <- covid_summary2 %>%
  group_by(age_group, vaccine_status) %>%
  mutate(total = sum(count))

covid_summary2 <- covid_summary2 %>%
  mutate(percent = 100 * count/total)

mort_rate <- covid_summary2 %>%
  filter(outcome=="death") %>%
  select(age_group, vaccine_status, percent)

ggplot(mort_rate, aes(fill=vaccine_status, y=percent, x=age_group)) + 
  geom_bar(position="dodge", stat="identity")

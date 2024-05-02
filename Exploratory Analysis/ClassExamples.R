library(tidyverse)

#Categorical Data (mostly)

comics <- read_csv("comics.csv")

summary(comics)
glimpse(comics)

comics %>%
  arrange(desc(appearances)) %>%
  select(name, Series, appearances)

comics %>%
  filter(gender == "female characters") %>%
  arrange(desc(appearances)) %>%
  select(name, Series, appearances)

comics %>% distinct(id)
comics %>% distinct(align)

comics %>%
  count(align, id)

comics %>%
  group_by(align, id) %>%
  summarize(n=n())

comics %>%
  group_by(align, id) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = id, values_from=n)

ggplot(comics, aes(x=id)) +
  geom_bar()

ggplot(comics, aes(x=id, fill=align)) +
  geom_bar()

comics %>% distinct(sex)

filter(comics, sex == "genderfluid characters")

comics %>%
  count(year, sex) %>%
  pivot_wider(names_from = sex, values_from = n) %>%
  print(n=50)


#Restructure for Line Graph
comgen <- select(comics, sex, year)
comgen2 <- comgen %>%
  arrange(year)

comgenM <- comgen %>%
  group_by(year, sex) %>%
  filter(sex == "male characters") %>%
  summarize(n=n())

comgenF <- comgen %>%
  group_by(year, sex) %>%
  filter(sex == "female characters") %>%
  summarize(n=n())

comgenM <- rename(comgenM, male = n)
comgenF <- rename(comgenF, female = n)

comgenA <- merge(comgenM, comgenF, by = "year", all=TRUE)

comgenA <- comgenA %>%
  filter(!is.na(year)) %>%
  select(year, male, female)

ggplot(comgenA, aes(x=year)) + 
  geom_line(aes(y=male), color="darkred") +
  geom_line(aes(y=female), color="steelblue")

#Proportions
#Proportion of all entries in the data
comics %>% 
  count(id, align) %>%
  mutate(prop = n/sum(n)) %>%
  pivot_wider(names_from = align, values_from = prop) %>%
  print(n=50)

#Proportion of entries in each column of the data (relative to one another)
comics %>% 
  count(id, align) %>%
  group_by(id) %>%
  mutate(prop = n/sum(n)) %>%
  pivot_wider(names_from = align, values_from = prop) %>%
  print(n=50)

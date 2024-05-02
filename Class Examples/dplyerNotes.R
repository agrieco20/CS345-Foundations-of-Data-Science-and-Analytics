library(tidyverse)
# install.packages(lubridate)
library(lubridate)

mydf <- read.csv("charts.csv")
dim(mydf)
summary(mydf)
glimpse(mydf)
names(mydf) #Check column names

#Change column names (new name [on left] now equal to old name [on right])
mydf <- mydf %>% rename(weeks = "weeks.on.board", lastweek = "last.week", peakrank = "peak.rank")
summary(mydf)

#Create a dataframe that contains only the data we want to look at
#The select() takes all the data from the selected columns to create a new data frame with
df <- mydf %>% select(date, song, artist, rank, weeks)
names(df)
df <- mydf %>% select(date:artist, weeks) #Selects all columns of data from original data set between "date" and "artist" in addition to "weeks" so that you don't have to list all of them manually (ORDER MATTERS)
df <- mydf %>% select(-lastweek) #Selects all data from existing data frame except for the "lastweek" column
mydf %>% select(artist)
print(n=100, select(mydf, artist))

min(df$date) #Returns minimum date
max(df$date) #Returns maximum date

artists <- df %>% distinct(artist) #Allows us to see total number of different entries under artist in all of the data

write_csv(artists, "artists.csv") #Writes out a file containing the "artists" file

head(artists) #Returns Top 5 Artists
tail(artists) #Returns Bottom 5 Artists

#FILTER - finds all rows containing data with a certain entry under a given column name 
df %>% filter(date == "2020-04-04")

df %>% filter(date == "2020-04-04", rank == 1) #Filters with multiple variables
df %>% filter(date == "2020-04-04", rank == 1 | rank == 10) #Uses boolean operators to further filter the data
# "|" OR
# "&" AND
# "!" NOT
# "%in% Checks for something thats within a range given range - ex: df %>% filter(date == "2020-04-04", rank %in% c(1:10)

oneHits <- df %>%
  filter(rank==1) %>%
  add_count(artist) %>% #Automatically counts the number of times that an artist appears on the list
  filter(n==1)

df_sml <- df %>% filter(rank == 1)
temp <- df_sml %>% 
  filter(rank==1) %>%
  add_count(artist) %>%
  filter(n>1)

#MUTATE - adds variables/columns to a dataframe based on other variables (ex: to break out the date from an initial column/variable named date-year)
df_sml <- df_sml %>% mutate(year = year(date), 
                           month = month(date), 
                           day = day(date))

#ARRANGE - Sorts the data by rearranging the order of columns as they appear in a dataframe
df_sml %>% arrange(year, month, day)

#SUMMARIZE
summarize(df_sml, ave_weeks = mean(weeks))

#GROUP_BY - groups the data by a certain parameter
#"n()" - returns number of observations in the given group
df_sml %>% group_by(artist) %>%
  summarize(n1=n()) %>%
  arrange(desc(n1)) #"Desc" returns the data in descending fashion

df_sml %>% 
  group_by(artist) %>%
  summarize(n_times_at_1 = n()) %>%
  slice_max(order_by = n_times_at_1, n = 20) %>%
  ggplot(aes(x = n_times_at_1, 
             y = reorder(artist, n_times_at_1))) +
  geom_bar(stat = "identity", fill = "blue", alpha = .6, width = .3) +
  labs(x = "Number 1 Hits from 1958 - 2021", y = "Artist") +
  theme_minimal()


if(!require(gutenbergr)){
  install.packages("gutenbergr")
  library(gutenbergr)
}
if(!require(tidytext)){
  install.packages("tidytext")
  library(tidytext)
}

# install.packages("tidyverse")
library(tidyverse)

gutenberg_works(title == "Adventures of Huckleberry Finn")

# huck_finn <-gutenberg_download(19640, mirror = "https://mirrors.xmission.com/gutenberg")

# This has been changed so we need to go find the compressed text file
# and download it locally.

url =("https://www.gutenberg.org/files/32325/32325-0.zip")
download.file(url, "32325-0.zip")

# Instead of going out to the Internet - we specify that we are using a local file
huck_finn <-gutenberg_download(19640, files="32325-0.zip")

data(stop_words)
tidy_huck_finn <- huck_finn %>%
  unnest_tokens(word, text) %>% #tokenize 
  anti_join(stop_words) #remove stop words
tidy_huck_finn

tidy_huck_finn %>% count(word, sort = TRUE) %>% top_n(5)
huck <- tidy_huck_finn %>% dplyr::filter(word == "huck" | word == "huckleberry") %>% count(word, sort = TRUE)
jim <- tidy_huck_finn %>% dplyr::filter(word == "jim") %>% count(word, sort = TRUE)
tom <- tidy_huck_finn %>% dplyr::filter(word == "tom") %>% count(word, sort = TRUE)

namesdf <- bind_rows(huck,jim,tom)
namesdf

ggplot(namesdf, aes(x=word, y=n, fill=word)) + 
  geom_bar(stat="identity") +
  labs(title=paste("Name Occurances in Adventures of Huckleberry Hinn"), x = "Name", y = "Count")


# This Internet reference works fine so we don't have to download locally
gutenberg_works(title == "Little Women")
little_women <- gutenberg_download(514, mirror = "http://mirrors.xmission.com/gutenberg/")
little_women

data(stop_words)
tidy_little_women <- little_women %>%
  unnest_tokens(word, text) %>% #tokenize 
  anti_join(stop_words) #remove stop words
tidy_little_women

tidy_little_women %>% count(word, sort = TRUE) %>% top_n(10)
jo <- tidy_little_women %>% dplyr::filter(word == "jo") %>% count(word, sort = TRUE)
meg <- tidy_little_women %>% dplyr::filter(word == "meg") %>% count(word, sort = TRUE)
amy <- tidy_little_women %>% dplyr::filter(word == "amy") %>% count(word, sort = TRUE)
laurie <- tidy_little_women %>% dplyr::filter(word == "laurie") %>% count(word, sort = TRUE)
beth <- tidy_little_women %>% dplyr::filter(word == "beth") %>% count(word, sort = TRUE)
names2df <- bind_rows(jo,meg,amy,laurie,beth)
names2df

ggplot(names2df, aes(x=word, y=n, fill=word)) + 
  geom_bar(stat="identity") +
  labs(title=paste("Name Occurances in Little Women"), x = "Name", y = "Count")


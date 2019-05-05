# change this to your current working directory - I use rstudio.cloud
setwd("/cloud/project/STD_AT2")
library(tidyverse)
library(lubridate)

# read and remove duplicate
twitter_data <- read_delim('twitter_search_result.psv', delim="|")
twitter_data <- twitter_data %>% distinct() %>% mutate(geo_location = NULL)

# convert date string to real_date POSIX Date object
twitter_data <- twitter_data %>% rowwise() %>% mutate(real_date_aedt = as.POSIXct(date,format="%a %b %d %H:%M:%S AEDT %Y"))
twitter_data <- twitter_data %>% rowwise() %>% mutate(real_date_aest = as.POSIXct(date,format="%a %b %d %H:%M:%S AEST %Y"))

twitter_data <- twitter_data %>% mutate(real_date = if_else(!is.na(real_date_aest), real_date_aest, real_date_aedt))

twitter_data["real_date"]
twitter_data$real_date = as.Date(twitter_data$real_date, format = "%d/%m/%y")

#this data starts from 2007 so has a lot of items that don;t relate to Sydney lock-out laws. the first mention is on 2014-01-22. 
#so need to remove all data form before that.
twitter_data <- filter(twitter_data, real_date == '2014')

#############################################################################################################################################

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

#######################
#EDA
#######################
ggplot(twitter_data, aes(x = real_date, fill = search_string)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~search_string, ncol = 1)

unique(twitter_data$search_string) #change all these names with one item

remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- twitter_data %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))



#############################################################################################################################################
library(tidytext)
library(dplyr)       #used for data cleaning and manipulation
library(stringr)     #used for data manipulation
library(ggplot2)     #used for plotting any graphs

#library(janeaustenr) #dataset being used for analysis, included in tidytext
sentiments           #dataset of various sentiments, included in tidytext

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)

twitter_data <- twitter_data$text %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) 

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()





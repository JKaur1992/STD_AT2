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

library(tidyverse) 
library(lubridate)

rent <- read_csv('STD_AT2/CLEAN DATA/clean_rent.csv')
rent <- (rent %>% filter(str_detect(Quarter,'^Mar'))
  %>% rowwise() %>% mutate(YEAR = as.numeric(strsplit(Quarter,'\\.')[[1]][2]) + 2000)
  %>% mutate(X1 = NULL, Quarter = NULL)
  %>% filter(YEAR >= 2011 & YEAR <= 2016))

(median_income <- read_csv('STD_AT2/CLEAN DATA/Income.csv') 
  %>% filter(YEAR <= 2016))

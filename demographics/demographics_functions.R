library(tidyverse)
library(readxl)
Income <- read_excel('STD_AT2/demographics/Income.xls')

write_csv(x = Income %>% filter(CODE >= 10000 & CODE < 20000)
     %>% rowwise() %>% mutate(LGA = (strsplit(LABEL,' ')[[1]][1]))
     %>% mutate(LABEL = NULL, CODE = NULL)
     %>% select(LGA,YEAR),'STD_AT2/CLEAN DATA/Income.csv')

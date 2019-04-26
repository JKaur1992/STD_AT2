library(tidyverse)
library(dplyr)

########## Data Loading ##################

rent_1br = read.csv("1br.csv")
rent_2br = read.csv("2br.csv")
rent_3br = read.csv("3br.csv")
rent_4br = read.csv("4br+.csv")

############### Data Cleaning ################

#Filtering columns to extract data from 2008 - 2017 

rent_1br <- rent_1br %>%
  select(LGA.Ring.SD.NAME, Mar.08,Jun.08,Sep.08,Dec.08,Mar.09,Jun.09,Sep.09,Dec.09,Mar.10,Jun.10,Sep.10,Dec.10,Mar.11,Jun.11,Sep.11,Dec.11,Mar.12,Jun.12,Sep.12,Dec.12,Mar.13,Jun.13,Sep.13,Dec.13,Mar.14,Jun.14,Sep.14,Dec.14,Mar.15,Jun.15,Sep.15,Dec.15,Mar.16,Jun.16,Sep.16,Dec.16,Mar.17,Jun.17)

rent_2br <- rent_2br %>%
  select(LGA.Ring.SD.NAME, Mar.08,Jun.08,Sep.08,Dec.08,Mar.09,Jun.09,Sep.09,Dec.09,Mar.10,Jun.10,Sep.10,Dec.10,Mar.11,Jun.11,Sep.11,Dec.11,Mar.12,Jun.12,Sep.12,Dec.12,Mar.13,Jun.13,Sep.13,Dec.13,Mar.14,Jun.14,Sep.14,Dec.14,Mar.15,Jun.15,Sep.15,Dec.15,Mar.16,Jun.16,Sep.16,Dec.16,Mar.17,Jun.17)

rent_3br <- rent_3br %>%
  select(LGA.Ring.SD.NAME, Mar.08,Jun.08,Sep.08,Dec.08,Mar.09,Jun.09,Sep.09,Dec.09,Mar.10,Jun.10,Sep.10,Dec.10,Mar.11,Jun.11,Sep.11,Dec.11,Mar.12,Jun.12,Sep.12,Dec.12,Mar.13,Jun.13,Sep.13,Dec.13,Mar.14,Jun.14,Sep.14,Dec.14,Mar.15,Jun.15,Sep.15,Dec.15,Mar.16,Jun.16,Sep.16,Dec.16,Mar.17,Jun.17)

rent_4br <- rent_4br %>%
  select(LGA.Ring.SD.NAME, Mar.08,Jun.08,Sep.08,Dec.08,Mar.09,Jun.09,Sep.09,Dec.09,Mar.10,Jun.10,Sep.10,Dec.10,Mar.11,Jun.11,Sep.11,Dec.11,Mar.12,Jun.12,Sep.12,Dec.12,Mar.13,Jun.13,Sep.13,Dec.13,Mar.14,Jun.14,Sep.14,Dec.14,Mar.15,Jun.15,Sep.15,Dec.15,Mar.16,Jun.16,Sep.16,Dec.16,Mar.17,Jun.17)

# Renaming First Column

names(rent_1br) <- c("region")
names(rent_2br) <- c("region")
names(rent_3br) <- c("region")
names(rent_4br) <- c("region")

################ End Of Cleaning ##############


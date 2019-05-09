library(tidyverse) 
library(lubridate)

rent <- read_csv('STD_AT2/CLEAN DATA/clean_rent.csv')
rent <- (rent %>% filter(str_detect(Quarter,'^Mar'))
  %>% rowwise() %>% mutate(YEAR = as.numeric(strsplit(Quarter,'\\.')[[1]][2]) + 2000)
  %>% mutate(X1 = NULL, Quarter = NULL)
  %>% filter(YEAR >= 2011 & YEAR <= 2016))

(median_income <- read_csv('STD_AT2/CLEAN DATA/Income.csv') 
  %>% filter(YEAR <= 2016))
median_income$`Median annual employee income ($)` <- as.numeric(median_income$`Median Employee income $`)
rent$`Rent_1_bedroom ($)` <- as.numeric(rent$Rent_1_bedroom)
rent$`Rent_2_bedroom ($)` <- as.numeric(rent$Rent_2_bedroom)
rent$`Rent_3_bedroom ($)` <- as.numeric(rent$Rent_3_bedroom)
rent$`Rent_4_bedroom ($)` <- as.numeric(rent$Rent_4_bedroom)

rent_median_income$YEAR = as.character(rent_median_income$YEAR)
rent_median_income <- inner_join(rent, median_income)

ggplot(rent_median_income,aes(x = `Median annual employee income ($)`, y = `Rent_1_bedroom ($)`, color=YEAR)) +
  geom_point() +
  facet_wrap(vars(YEAR)) +
  geom_smooth(method = 'lm')

ggplot(rent_median_income,aes(x = `Median annual employee income ($)`, y = `Rent_2_bedroom ($)`, color=YEAR)) +
  geom_point() +
  facet_wrap(vars(YEAR)) +
  geom_smooth(method = 'lm')

ggplot(rent_median_income,aes(x = `Median annual employee income ($)`, y = `Rent_3_bedroom ($)`, color=YEAR)) +
  geom_point() +
  facet_wrap(vars(YEAR)) +
  geom_smooth(method = 'lm')

ggplot(rent_median_income,aes(x = `Median annual employee income ($)`, y = `Rent_4_bedroom ($)`, color=YEAR)) +
  geom_point() +
  facet_wrap(vars(YEAR)) +
  geom_smooth(method = 'lm')

  #scale_x_discrete(limits = c(30000, 70000))

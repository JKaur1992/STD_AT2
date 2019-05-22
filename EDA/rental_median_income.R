library(tidyverse) 
library(lubridate)


parse_date_time('Mar.13','m.y')

rent <- read_csv('STD_AT2/CLEAN DATA/clean_rent.csv')
rent <- (rent %>% 
           rowwise() %>% mutate(YEAR = as.numeric(strsplit(Quarter,'\\.')[[1]][2]) + 2000) %>% 
           mutate(X1 = NULL, Quarter = NULL) %>% 
           filter(YEAR >= 2011 & YEAR <= 2016))

rent <- rent %>% mutate(Rent_1_bedroom = as.numeric(Rent_1_bedroom), 
                        Rent_2_bedroom = as.numeric(Rent_2_bedroom),
                        Rent_3_bedroom = as.numeric(Rent_3_bedroom),
                        Rent_4_bedroom = as.numeric(Rent_4_bedroom))

rent

View(rent %>% group_by(LGA, YEAR) %>% summarize(`Average rent 1 bedroom` = mean(Rent_1_bedroom, na.rm = TRUE),
                                      `Average rent 2 bedroom` = mean(Rent_2_bedroom, na.rm = TRUE),
                                      `Average rent 3 bedroom` = mean(Rent_3_bedroom, na.rm = TRUE),
                                      `Average rent 4 bedroom` = mean(Rent_4_bedroom, na.rm = TRUE)))


rent %>% filter(LGA == 'Hunters Hill', YEAR == 2016)

rent_quarter <- read_csv('STD_AT2/CLEAN DATA/clean_rent.csv') %>% rowwise() %>% mutate(YEAR_Quarter = parse_date_time(Quarter, 'm.y')) %>% 
  filter(YEAR_Quarter >= as.Date('2011-01-01'), YEAR_Quarter <= as.Date('2016-01-01'))
rent_quarter <- rent_quarter %>% rowwise() %>% mutate(YEAR = as.numeric(strsplit(Quarter,'\\.')[[1]][2]) + 2000)
rent_quarter %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Randwick" )

rent_quarter$YEAR_Quarter <- as.Date(rent_quarter$YEAR_Quarter)



View(rent_quarter)
plot_1 <- rent_quarter %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Randwick" ) %>%
  ggplot(aes(x = YEAR, y = Rent_1_bedroom))+
  geom_point(aes(color = LGA), size = 1.5) + geom_vline(xintercept = 2014 , linetype="dotted", 
                                                        color = "black", size=1.5) + theme_bw()
rent_quarter %>% filter(LGA=="Sydney")
(plot_1 <- rent_quarter %>% filter(LGA=="Sydney", YEAR == 2011) %>%
  ggplot(aes(x = as.numeric(YEAR_Quarter), y = Rent_1_bedroom))+
  geom_point(aes(color = LGA), osize = 1.5))
  
(plot_1 <- rent_quarter %>% filter(LGA=="Sydney" | LGA == 'Woollahra' | LGA == "Waverley" | LGA == "Randwick") %>%
    ggplot(aes(x = YEAR_Quarter, y = Rent_1_bedroom, group = 1))+
    geom_line(aes(color = LGA)) + 
    facet_wrap(vars(LGA)))

(plot_1 <- rent_quarter %>% filter(LGA=="Sydney" | LGA=="Leichhardt" | LGA == 'Inner Ring' | LGA == "Randwick") %>%
    ggplot(aes(x = YEAR_Quarter, y = Rent_2_bedroom, group = 1))+
    geom_line(aes(color = LGA, group = LGA)) +
    geom_vline(xintercept = as.Date("2014/02/01") , linetype="dotted", color = "black", size=1.5) +
    labs(x = 'Time', y = 'Rent per week', title = '2 bedroom rent across quarters') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)))
    


(plot_1 <- rent_quarter %>% filter(LGA=="Sydney") %>%
  ggplot(aes(x = YEAR_Quarter))+
  geom_path(aes(y = Rent_1_bedroom), size = 1.5))

(plot_1 <- rent_quarter %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Randwick" ) %>%
    ggplot(aes(x = YEAR_Quarter, y = Rent_1_bedroom, group = 1))+
    geom_line(aes(color = LGA)))



(sal_plot <- data %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Randwick" ) %>%
  ggplot(aes(x = YEAR, y = income))+
  geom_line( aes(color = LGA), size = 1.5) + geom_vline(xintercept = 2014 , linetype="dotted", 
                                                        color = "black", size=1.5) + theme_bw())


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

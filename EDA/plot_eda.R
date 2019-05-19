library(tidyverse) 
library(lubridate)
library(ggplot2)

rent <- read_csv('clean_rent.csv')
rent <- (rent %>% filter(str_detect(Quarter,'^Mar'))
         %>% rowwise() %>% mutate(YEAR = as.numeric(strsplit(Quarter,'\\.')[[1]][2]) + 2000)
         %>% mutate(X1 = NULL, Quarter = NULL)
         %>% filter(YEAR >= 2011 & YEAR <= 2016))

rent_2 <- read_csv('clean_rent.csv')
rent_2 <- (rent_2 %>%
              rowwise() %>% mutate(YEAR = as.numeric(strsplit(Quarter,'\\.')[[1]][2]) + 2000)
         %>% mutate(X1 = NULL, Quarter = NULL)
         %>% filter(YEAR >= 2011 & YEAR <= 2016))

(median_income <- read_csv('Income.csv') 
  %>% filter(YEAR <= 2016))
median_income$`income` <- as.numeric(median_income$`Median Employee income $`)
rent$`Rent_1_bedroom` <- as.numeric(rent$Rent_1_bedroom)
rent$`Rent_2_bedroom` <- as.numeric(rent$Rent_2_bedroom)
rent$`Rent_3_bedroom` <- as.numeric(rent$Rent_3_bedroom)
rent$`Rent_4_bedroom` <- as.numeric(rent$Rent_4_bedroom)

data <- inner_join(rent, median_income)
#data$YEAR = as.character(rent_median_income$YEAR)
data$`Median Employee income $` <- NULL
str(data)

#lubridate::ymd(data$YEAR, truncated = 2L)

##data$number <- NULL
#data <- data %>% mutate(`Rent/Income` =  Rent_1_bedroom*100/(income/52.1429))
#colnames(data)[colnames(data)=="Rent_1_bedroom*100/(income/52.1429)"] <- "Rent/Income"

data %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Randwick" ) 

plot_1 <- data %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Randwick" ) %>%
  ggplot(aes(x = YEAR, y = Rent_1_bedroom))+
  geom_line( aes(color = LGA), size = 1.5) + geom_vline(xintercept = 2014 , linetype="dotted", 
                                                        color = "black", size=1.5) + theme_bw()
plot_1

plot_2 <- data %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Willoughby" ) %>%
  ggplot(aes(x = YEAR, y = Rent_2_bedroom))+
  geom_line( aes(color = LGA), size = 2) + geom_vline(xintercept = 2014 , linetype="dotted", 
                                                      color = "black", size=1.5) + theme_bw()
plot_2

plot_3 <- data %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Willoughby" ) %>%
  ggplot(aes(x = YEAR, y = Rent_3_bedroom))+
  geom_line( aes(color = LGA), size = 2)
plot_3

plot_4 <- data %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Willoughby" ) %>%
  ggplot(aes(x = YEAR, y = Rent_4_bedroom))+
  geom_line( aes(color = LGA), size = 2)
plot_4

sal_plot <- data %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Randwick" ) %>%
  ggplot(aes(x = YEAR, y = income))+
  geom_line( aes(color = LGA), size = 1.5) + geom_vline(xintercept = 2014 , linetype="dotted", 
                                                      color = "black", size=1.5) + theme_bw()
sal_plot

library("ggpubr")

ggarrange(plot_1, sal_plot ,
          labels = c("Weekly Rent($) Trend: 1 Bedroom Homes", " Median Income($) Trend"), ncol = 1, nrow = 2)

ggarrange(plot_2, sal_plot ,
          labels = c("Weekly Rent($) Trend: 2 Bedroom Homes ", " Median Income($) Trend"), ncol = 1, nrow = 2)


rentvsinc_plot <- data %>% filter(LGA=="Sydney"  | LGA == "Woollahra" | LGA == "Waverley" | LGA == "Randwick" ) %>%
  ggplot(aes(x = income, y = Rent_3_bedroom )) +
  geom_line( aes(color = LGA), size = 1.5) + labs(title= "Rent vs Income ($): 3 Bedroom Homes")
rentvsinc_plot




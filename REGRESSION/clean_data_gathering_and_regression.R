library(tidyverse) 
library(lubridate)
library(dplyr)
# setwd("C:/Users/mjg07/OneDrive/Documents/MDSI/36103 Statistical Thinking for Data Science/Assignment 2")

###################################
# Functions to setup data imports #
###################################

get_clean_rent_data_by_LGA_year <- function() {
  rent <- read_csv('STD_AT2/CLEAN DATA/clean_rent.csv')
  rent <- (rent %>% 
             rowwise() %>% mutate(year = as.numeric(strsplit(Quarter,'\\.')[[1]][2]) + 2000) %>% 
             mutate(X1 = NULL, Quarter = NULL) %>% 
             filter(year >= 2011 & year <= 2016))
  
  rent <- rent %>% mutate(Rent_1_bedroom = as.numeric(Rent_1_bedroom), 
                          Rent_2_bedroom = as.numeric(Rent_2_bedroom),
                          Rent_3_bedroom = as.numeric(Rent_3_bedroom),
                          Rent_4_bedroom = as.numeric(Rent_4_bedroom))

  #TODO get the Average rent across state  
  
  
  return (rent %>% group_by(LGA, year) %>% summarize(Average_rent_1_bedroom = mean(Rent_1_bedroom, na.rm = TRUE),
                                                     Average_rent_2_bedroom = mean(Rent_2_bedroom, na.rm = TRUE),
                                                    Average_rent_3_bedroom = mean(Rent_3_bedroom, na.rm = TRUE),
                                                    Average_rent_4_bedroom = mean(Rent_4_bedroom, na.rm = TRUE)))
}

get_clean_median_income_by_LGA_year <-  function() {
  return (median_income <- read_csv('STD_AT2/CLEAN DATA/Income.csv') 
   %>% filter(YEAR <= 2016) %>% rename('year' = 'YEAR') %>% mutate(Median_income= as.numeric(`Median Employee income $`)) %>%
   mutate(`Median Employee income $`= NULL))
}

get_offence_population_by_LGA_year <-  function() {
  return(offence_pop <- read_csv('STD_AT2/CLEAN DATA/offence_data_EDA.csv') %>% rename('year' = 'Year') %>%
     mutate(Population_Density = as.numeric(Population_Density)))
  
}

get_business_entries_rate_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/Business.csv') %>% select(LGA, year, New_business_entries_rate))
}


get_hostpitalisation_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/alcohol_hosp_death.csv'))
}

get_frequency_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/alcohol_freq_LGA.csv'))
}

# Import the data

rent <- get_clean_rent_data_by_LGA_year()

income <- get_clean_median_income_by_LGA_year()

offence_pop <- get_offence_population_by_LGA_year()

hospitalisation <- get_hostpitalisation_by_LGA_year();

business <- get_business_entries_rate_by_LGA_year();

frequency <- get_frequency_by_LGA_year();

# Join  all the data sets together
joined_data <- inner_join(inner_join(inner_join(inner_join(inner_join(rent, income, by = c('LGA','year')), offence_pop, by = c('LGA', 'year')), business, by = c('LGA', 'year')), hospitalisation, by = c('LGA', 'year')), frequency, by = c('LGA', 'year'))
# Joining all the data we end up with a very limited data set (down from 1200+ observations down to only 145....)

# So we will join parts individual to minimise data loss.

# Filter to >2007 and join health data to freq, income, offence_pop
hospitalisation <- filter(hospitalisation, year >2007)
join_health <- inner_join(hospitalisation,frequency,by =c('LGA','year'))

join_health <- inner_join(join_health, income, by =c('LGA','year'))
join_health <- inner_join(join_health, offence_pop, by = c('LGA','year'))


# Convert "rate" to "counts" for death and hosp (was rate per 100,000 pop) so can do poisson regression model
join_health <- join_health %>%
  mutate(.,hosp_count = round(hosp_rate), death_count = round(death_rate), hosp_prob = hosp_rate / 100000, death_prob = death_rate / 100000)

#==== Regressions for Alcohol Related Hospitalizations ====
# Run regression - on hosp count, trial forward and backward feature selection to optimise the model for the lowest AIC measure
glm_base<-glm(hosp_count ~ Median_income + Population_Density, family = poisson(), data = join_health)

glm_var<-glm(hosp_count ~ Median_income + Population_Density + freq_daily, family = poisson(), data = join_health)

glm_var2<-glm(hosp_count ~ Median_income + Population_Density + freq_daily+freq_weekly, family = poisson(), data = join_health)

glm_basevar<-glm(hosp_count ~ Population_Density + freq_daily+freq_weekly, family = poisson(), data = join_health)

glm_basevar2<-glm(hosp_count ~ Population_Density + freq_daily+freq_weekly+freq_less_weekly, family = poisson(), data = join_health)

glm_basevar3<-glm(hosp_count ~ Population_Density +freq_daily+freq_weekly+freq_less_weekly+freq_never, family = poisson(), data = join_health)

glm_baseinter<-glm(hosp_count ~ Population_Density*freq_daily, family = poisson, data = join_health)


glm_baseinter<-glm(hosp_count ~ Population_Density*freq_daily + Population_Density*freq_never, family = poisson, data = join_health)


#--------------AIC 12717 --------------------#
glm_baseinter<-glm(hosp_count ~ year*Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = poisson, data = join_health)
#-------------------------------------------#


# glm_binom<-glm(hosp_prob ~ year*Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = binomial(logit), data = join_health)  # We tried a Binomial but  not a good idea.... 

# Summary of the outputs of the various models 
summary(glm_base)
summary(glm_var)
summary(glm_var2)
summary(glm_basevar)
summary(glm_basevar2)
summary(glm_basevar3)
summary(glm_baseinter)

# summary(glm_binom)

step(glm,scope=~Median_income + Population_Density + freq_daily, direction="forward")


glm(Average_rent_1_bedroom ~ Median_income + Population_Density, family = gaussian(), data = joined_data)


# ==== Regression For Alcohol related Deaths ====
linear_Hosp_death <- lm(hosp_count ~ death_count, data = join_health) #how similar hosp and death behaves 
summary(linear_Hosp_death)

#--------------AIC 2234 --------------------#
glm_base_death<-glm(death_count ~ Median_income + Population_Density, family = poisson(), data = join_health)
# ------------------------------------------#


glm_var_death<-glm(death_count ~ Median_income + Population_Density + freq_daily, family = poisson(), data = join_health)

glm_var2_death<-glm(death_count ~ Median_income + Population_Density +freq_weekly, family = poisson(), data = join_health)

glm_var3_death<-glm(death_count ~ Median_income + Population_Density +freq_less_weekly, family = poisson(), data = join_health)

glm_basevar_death<-glm(death_count ~ Median_income + Population_Density +freq_daily+freq_weekly+freq_less_weekly+freq_never, family = poisson(), data = join_health)



glm_baseinter_death<-glm(death_count ~ Population_Density*freq_daily, family = poisson, data = join_health)

glm_baseinter_death<-glm(death_count ~ Population_Density*freq_daily + Population_Density*freq_never, family = poisson, data = join_health)

glm_baseinter_death<-glm(death_count ~ year*Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = poisson, data = join_health)


glm_time_death<-glm(death_count ~ Median_income + Population_Density + year*freq_weekly, family = poisson(), data = join_health)

glm_timevar_death<-glm(death_count ~ Median_income + Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = poisson(), data = join_health)


# Summary of the outputs of the various models deaths
summary(glm_base_death)
summary(glm_var_death)
summary(glm_var2_death)
summary(glm_var3_death)
summary(glm_basevar_death)
summary(glm_baseinter_death)
summary(glm_time_death)
summary(glm_timevar_death)


#==== Regression for Violence count ====
glm_base_violence<-glm(violence_count ~ Median_income + Population_Density, family = poisson(), data = join_health)

glm_var_violence<-glm(violence_count ~ Median_income + Population_Density + freq_daily, family = poisson(), data = join_health)

glm_var2_violence<-glm(violence_count ~ Median_income + Population_Density + freq_daily+ freq_weekly, family = poisson(), data = join_health)

glm_var3_violence<-glm(violence_count ~ Median_income  + freq_daily+ freq_weekly + freq_never, family = poisson(), data = join_health)

glm_baseint_violence<-glm(violence_count ~ Median_income + Population_Density*freq_daily, family = poisson(), data = join_health)

glm_baseint_violence<-glm(violence_count ~ Median_income  + Population_Density*freq_daily+ Population_Density*freq_weekly + Population_Density*freq_never, family = poisson(), data = join_health)


#--------------AIC 58701 --------------------#
glm_basetime_violence<-glm(violence_count ~ Median_income + year*Population_Density + Population_Density*freq_daily+ Population_Density*freq_weekly + Population_Density*freq_never, family = poisson(), data = join_health)

summary(glm_basetime_violence)
#--------------------------------------------#


summary(glm_base_violence)
summary(glm_var_violence)
summary(glm_var2_violence)
summary(glm_var3_violence)
summary(glm_baseint_violence)
summary(glm_basetime_violence)

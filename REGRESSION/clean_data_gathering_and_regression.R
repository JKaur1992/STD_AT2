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

# Create a sum of all cinsumer (all freq)
join_health <- join_health %>%
  mutate(totcons = freq_daily + freq_less_weekly + freq_weekly)

#Create an indicative variable for pre and post law 
join_health$Postlaw<- ifelse(join_health$year > 2014, 1,0)

#==== Regressions for Alcohol Related Hospitalizations ====
# Run regression - on hosp count, trial forward and backward feature selection to optimise the model for the lowest AIC measure
hosp1<-glm(hosp_count ~ Median_income + Population_Density, family = poisson(), data = join_health)
summary(hosp1)

hosp2<-glm(hosp_count ~ Median_income + Population_Density + freq_daily, family = poisson(), data = join_health)
summary(hosp2)

hosp3<-glm(hosp_count ~ Median_income + Population_Density + freq_daily+freq_weekly, family = poisson(), data = join_health)
summary(hosp3)

hosp4<-glm(hosp_count ~ Population_Density + freq_daily+freq_weekly, family = poisson(), data = join_health)
summary(hosp4)

hosp5<-glm(hosp_count ~ Population_Density + freq_daily+freq_weekly+freq_less_weekly, family = poisson(), data = join_health)
summary(hosp5)

hosp6<-glm(hosp_count ~ Population_Density +freq_daily+freq_weekly+freq_less_weekly+freq_never, family = poisson(), data = join_health)
summary(hosp6)

hosp7<-glm(hosp_count ~ Population_Density*freq_daily, family = poisson, data = join_health)
summary(hosp7)


hosp8<-glm(hosp_count ~ Population_Density*freq_daily + Population_Density*freq_never, family = poisson, data = join_health)
summary(hosp8)

# ------ BEST REGRESSION FOR HOSPITALIZATIONS WITHOUT 2014 INDICATIVE -------#
hosp9<-glm(hosp_count ~ year + Population_Density + freq_daily+freq_weekly+freq_less_weekly, family = poisson(), data = join_health)
summary(hosp9)
#----------------------------------------------------#

hosp10<-glm(hosp_count ~ year*Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = poisson, data = join_health)
summary(hosp10)

hosp11<-glm(hosp_count ~ year + Population_Density + totcons, family = poisson(), data = join_health)
summary(hosp11)

hosp12<-glm(hosp_count ~ year + Population_Density + freq_daily+freq_weekly+freq_less_weekly + Postlaw, family = poisson(), data = join_health)
summary(hosp12)
# ------ BEST REGRESSION FOR HOSPITALIZATIONS -------#
hosp13<-glm(hosp_count ~ Population_Density + freq_daily+freq_weekly+freq_less_weekly + Postlaw, family = poisson(), data = join_health)
summary(hosp13)
#----------------------------------------------------#

hosp14<-glm(hosp_count ~ Population_Density +totcons +Postlaw, family = poisson(), data = join_health)
summary(hosp14)

# glm_binom<-glm(hosp_prob ~ year*Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = binomial(logit), data = join_health)  # We tried a Binomial but  not a good idea.... 


# summary(glm_binom)

step(glm,scope=~Median_income + Population_Density + freq_daily, direction="forward")


glm(Average_rent_1_bedroom ~ Median_income + Population_Density, family = gaussian(), data = joined_data)


# ==== Regression For Alcohol related Deaths ====
linear_Hosp_death <- lm(hosp_count ~ death_count, data = join_health) #how similar hosp and death behaves 
summary(linear_Hosp_death)



death1<-glm(death_count ~ Median_income + Population_Density, family = poisson(), data = join_health)
summary(death1)


glm_var_death<-glm(death_count ~ Median_income + Population_Density + freq_daily, family = poisson(), data = join_health)

glm_var2_death<-glm(death_count ~ Median_income + Population_Density +freq_weekly, family = poisson(), data = join_health)

glm_var3_death<-glm(death_count ~ Median_income + Population_Density +freq_less_weekly, family = poisson(), data = join_health)

glm_basevar_death<-glm(death_count ~ Median_income + Population_Density +freq_daily+freq_weekly+freq_less_weekly+freq_never, family = poisson(), data = join_health)


glm_baseinter_death<-glm(death_count ~ Population_Density*freq_daily, family = poisson, data = join_health)

glm_baseinter_death<-glm(death_count ~ Population_Density*freq_daily + Population_Density*freq_never, family = poisson, data = join_health)

glm_baseinter_death<-glm(death_count ~ year*Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = poisson, data = join_health)


glm_time_death<-glm(death_count ~ Median_income + Population_Density + year*freq_weekly, family = poisson(), data = join_health)

glm_timevar_death<-glm(death_count ~ Median_income + Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = poisson(), data = join_health)

deathXX<-glm(death_count ~ Median_income + Population_Density + totcons, family = poisson(), data = join_health)
summary(deathXX)

death20<-glm(death_count ~ Median_income + Population_Density + Postlaw, family = poisson(), data = join_health)
summary(death20)


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
violence1<-glm(violence_count ~ Median_income + Population_Density, family = poisson(), data = join_health)
summary(violence1)

violence2<-glm(violence_count ~ Median_income + Population_Density + freq_daily, family = poisson(), data = join_health)
summary(violence2)

violence3<-glm(violence_count ~ Median_income + Population_Density + freq_daily+ freq_weekly, family = poisson(), data = join_health)
summary(violence3)

violence4<-glm(violence_count ~ Population_Density + freq_daily+ freq_weekly + freq_never, family = poisson(), data = join_health)
summary(violence4)

#--------------BEST REGRESSION FOR VIOLENCE WITHOUT 2014 INDICATIVE ------------------------------#
violence5<-glm(violence_count ~ year +Population_Density  + freq_daily+ freq_weekly + freq_never, family = poisson(), data = join_health)
summary(violence5)

violence6<- glm(violence_count ~ year +Population_Density  +totcons , family = poisson(), data = join_health)
summary(violence6)

#--------------BEST REGRESSION FOR VIOLENCE ------------------------------#
violence7<-glm(violence_count ~ year +Population_Density  + freq_daily+ freq_weekly + freq_never + Postlaw, family = poisson(), data = join_health)
summary(violence7)
#--------------------------------------------#





# ====== PREDICTIONS =========#####
# ---- Hospitalizatons -------#####
#Sydnay LGA 
PredHosp <- join_health %>%
  select(LGA,hosp_count, year,Population_Density,freq_daily,freq_weekly,freq_less_weekly) 


trainsetHosp <- PredHosp%>%
  filter(year <= 2013)

testsetHosp <- PredHosp%>%
  filter(year > 2013)





nrow(trainsetPHS)
nrow(testsetPHS)
nrow(PredHospSyd)

SydneyHosp<- glm(hosp_count ~ year + Population_Density + freq_daily + freq_weekly + freq_less_weekly, family = poisson(), data = trainsetPHS)

testsetPHS$Prediction <- predict(SydneyHosp, newdata=testsetPHS, type = "response" )
view(testsetPHS)

SydneyHospPredVsReal<- testsetPHS %>%
  select(LGA, year ,hosp_count, Prediction)
view(SydneyHospPredVsReal)

PredHospSyd <- PredHospSyd %>%
  left_join(SydneyHospPredVsReal, Prediction,by=c("LGA", "year", "hosp_count"))

PredHospSyd <- transform(PredHospSyd, Pred2=ifelse(year==2014, hosp_count, Prediction))
view(PredHospSyd)

rmse(PredHospSyd$hosp_count, PredHospSyd$Prediction)


#Plot the difference 
ggplot(data = PredHospSyd) + 
  geom_line(mapping = aes(x = year, y = hosp_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Prediction"), show.legend = TRUE)+
  xlab("Year") + ylab("Hospitalization") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=750), colour="black", angle=90, vjust = 1.2) +
  labs(title="Syndey Hospitalization Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()
  

#  Parramata LGA
PredHospPar <- join_health %>%
  filter(LGA=="Parramatta")%>%
  select(LGA,hosp_count, year,Population_Density,freq_daily,freq_weekly,freq_less_weekly) 

trainsetPHP <- PredHospPar[1:3,]
testsetPHP <- PredHospPar[4:5,]

nrow(trainsetPHP)
nrow(testsetPHP)
nrow(PredHospPar)

ParraHosp<- glm(hosp_count ~ year + Population_Density + freq_daily + freq_weekly + freq_less_weekly, family = poisson(), data = trainsetPHP)

testsetPHP$Prediction <- predict(ParraHosp, newdata=testsetPHP, type = "response" )
view(testsetPHP)

ParramattaHospPredVsReal<- testsetPHP %>%
  select(LGA, year ,hosp_count, Prediction)
view(ParramattaHospPredVsReal)

PredHospPar <- PredHospPar %>%
  left_join(ParramattaHospPredVsReal, Prediction,by=c("LGA", "year", "hosp_count"))

PredHospPar <- transform(PredHospPar, Pred2=ifelse(year==2014, hosp_count, Prediction))
view(PredHospPar)

#Plot the difference 
ggplot(data = PredHospPar) + 
  geom_line(mapping = aes(x = year, y = hosp_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Hospitalization") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=700), colour="black", angle=90, vjust = 1.2) +
  labs(title="Parramatta Hospitalization Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()


# Inner West

# ---- Death ---####
PredHospSyd <- join_health %>%
  filter(LGA=="Sydney")%>%
  select(LGA,hosp_count, year,Population_Density,freq_daily,freq_weekly,freq_less_weekly) 

trainsetPHS <- PredHospSyd[1:3,]
testsetPHS <- PredHospSyd[4:5,]

nrow(trainsetPHS)
nrow(testsetPHS)
nrow(PredHospSyd)

SydneyHosp<- glm(hosp_count ~ year + Population_Density + freq_daily + freq_weekly + freq_less_weekly, family = poisson(), data = trainsetPHS)
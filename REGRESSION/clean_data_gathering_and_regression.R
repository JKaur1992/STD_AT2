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

get_age_by_LGA_year <- function() {
  return(read_csv('STD_AT2/CLEAN DATA/Population_Clean.csv') %>% 
     select(LGA,Year,`Working Age Population (15-64 years)`,Male_Median_Age,Female_Median_Age,Person_Median_Age) %>%
     rename('year' = 'Year') %>% rename('Working_Age_Population_15_64_years' = `Working Age Population (15-64 years)`) %>%
    mutate(Working_Age_Population_15_64_years = as.numeric(Working_Age_Population_15_64_years)) %>%
     mutate(Male_Median_Age = as.numeric(Male_Median_Age)) %>%
     mutate(Female_Median_Age = as.numeric(Female_Median_Age)) %>%
     mutate(Person_Median_Age = as.numeric(Person_Median_Age))
   )
 
}

get_unemployment_rate_by_LGA_year <- function() {
  return (read_csv('STD_AT2/CLEAN DATA/Unemployment.csv') %>%
    rename('year' = 'YEAR') %>% mutate(Unemployment_rate = as.numeric(Unemployment_rate)))
}

# Import the data

rent <- get_clean_rent_data_by_LGA_year()

income <- get_clean_median_income_by_LGA_year()

offence_pop <- get_offence_population_by_LGA_year()

hospitalisation <- get_hostpitalisation_by_LGA_year();

business <- get_business_entries_rate_by_LGA_year();

frequency <- get_frequency_by_LGA_year();

age <- get_age_by_LGA_year();

unemployment_rate <- get_unemployment_rate_by_LGA_year();

# Join  all the data sets together
joined_data <- inner_join(inner_join(inner_join(inner_join(inner_join(rent, income, by = c('LGA','year')), offence_pop, by = c('LGA', 'year')), business, by = c('LGA', 'year')), hospitalisation, by = c('LGA', 'year')), frequency, by = c('LGA', 'year'))
# Joining all the data we end up with a very limited data set (down from 1200+ observations down to only 145....)

# So we will join parts individual to minimise data loss.

# Filter to >2007 and join health data to freq, income, offence_pop
hospitalisation <- filter(hospitalisation, year >2007)

join_health <- inner_join(hospitalisation,frequency,by =c('LGA','year'))
join_health <- inner_join(join_health, income, by =c('LGA','year'))
join_health <- inner_join(join_health, offence_pop, by = c('LGA','year'))

#new variables 
join_health <- inner_join(join_health, age, by = c('LGA','year'))
join_health <- inner_join(join_health, unemployment_rate, by = c('LGA','year'))


# Convert "rate" to "counts" for death and hosp (was rate per 100,000 pop) so can do poisson regression model
join_health <- join_health %>%
  mutate(.,hosp_count = round(hosp_rate), death_count = round(death_rate), hosp_prob = hosp_rate / 100000, death_prob = death_rate / 100000)

# Create a sum of all consumer (all freq od consumption)
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


hosp9<-glm(hosp_count ~ year + Population_Density + freq_daily+freq_weekly+freq_less_weekly, family = poisson(), data = join_health)
summary(hosp9)

# -----------------------#
hosp10<-glm(hosp_count ~ year + Population_Density + freq_daily+ freq_weekly+freq_less_weekly+ Working_Age_Population_15_64_years, family = poisson(), data = join_health)
summary(hosp10)
# -----------------------#

hosp11<-glm(hosp_count ~ year + Population_Density + freq_daily+ freq_weekly+freq_less_weekly+ Working_Age_Population_15_64_years + Postlaw, family = poisson(), data = join_health)
summary(hosp11)


# ------2014  indicative-----------------#
hosp12<-glm(hosp_count ~  Population_Density + freq_daily+ freq_weekly+freq_less_weekly+ Working_Age_Population_15_64_years + Postlaw, family = poisson(), data = join_health)
summary(hosp12)
# -----------------------#





# glm_binom<-glm(hosp_prob ~ year*Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = binomial(logit), data = join_health)  # We tried a Binomial but  not a good idea.... 


# summary(glm_binom)

step(glm,scope=~Median_income + Population_Density + freq_daily, direction="forward")


glm(Average_rent_1_bedroom ~ Median_income + Population_Density, family = gaussian(), data = joined_data)


# ==== Regression For Alcohol related Deaths ====
linear_Hosp_death <- lm(hosp_count ~ death_count, data = join_health) #how similar hosp and death behaves 
summary(linear_Hosp_death)


death1<-glm(death_count ~ Median_income + Population_Density, family = poisson(), data = join_health)
summary(death1)

death2<-glm(death_count ~ Median_income + Population_Density + freq_daily, family = poisson(), data = join_health)
summary(death2)

death3<-glm(death_count ~ Median_income + Population_Density +freq_weekly, family = poisson(), data = join_health)

death4<-glm(death_count ~ Median_income + Population_Density +freq_less_weekly, family = poisson(), data = join_health)

death5-glm(death_count ~ Median_income + Population_Density +freq_daily+freq_weekly+freq_less_weekly+freq_never, family = poisson(), data = join_health)


death6<-glm(death_count ~ Population_Density*freq_daily, family = poisson, data = join_health)

death7<-glm(death_count ~ Population_Density*freq_daily + Population_Density*freq_never, family = poisson, data = join_health)

death8<-glm(death_count ~ year*Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = poisson, data = join_health)


death9<-glm(death_count ~ Median_income + Population_Density + year*freq_weekly, family = poisson(), data = join_health)

death10<-glm(death_count ~ Median_income + Population_Density + freq_daily +freq_weekly+freq_less_weekly + freq_never, family = poisson(), data = join_health)

death11<-glm(death_count ~ Median_income + Population_Density + totcons, family = poisson(), data = join_health)
summary(death11)

# -----------------------#
death12<-glm(death_count ~ Median_income + Population_Density + Working_Age_Population_15_64_years, family = poisson(), data = join_health)
summary(death12)
# -----------------------#

# ------2014  indicative-----------------#
death13<-glm(death_count ~ Median_income + Population_Density + Working_Age_Population_15_64_years + Postlaw, family = poisson(), data = join_health)
summary(death13)
# -----------------------#


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
#

violence6<- glm(violence_count ~ year +Population_Density  +totcons , family = poisson(), data = join_health)
summary(violence6)

violence7<-glm(violence_count ~ year +Population_Density  + freq_daily+ freq_weekly + freq_never + Working_Age_Population_15_64_years, family = poisson(), data = join_health)
summary(violence7)

#--------------BEST REGRESSION FOR VIOLENCE ------------------------------#
violence8<-glm(violence_count ~ year +Population_Density  + freq_daily+ freq_weekly + freq_never + Working_Age_Population_15_64_years + Postlaw, family = poisson(), data = join_health)
summary(violence8)
#--------------------------------------------#





# ====== PREDICTIONS =========#####
# ---- Hospitalizatons -------#####
#Use only variables important in the regression fro hosp
PredHosp <- join_health %>%
  select(LGA,hosp_count, year,Population_Density,freq_daily,freq_weekly,freq_less_weekly, Working_Age_Population_15_64_years) 

#partitioning (2012-2013)/(2014-2016)
trainsetHosp <- PredHosp%>%
  filter(year <= 2013)

testsetHosp <- PredHosp%>%
  filter(year > 2013)

nrow(trainsetHosp)
nrow(testsetHosp)
nrow(PredHosp)
nrow(trainsetHosp) + nrow(testsetHosp) ==nrow(PredHosp)

#Model
Hosp_glm<- glm(hosp_count ~ year + Population_Density + freq_daily+ freq_weekly+freq_less_weekly+ Working_Age_Population_15_64_years, family = poisson(), data = trainsetHosp)

testsetHosp$Prediction <- predict(Hosp_glm, newdata=testsetHosp, type = "response" )

summary(Hosp_glm)


#Just to compare the actual vs prediction
HospPredVsReal<- testsetHosp %>%
  select(LGA, year ,hosp_count, Prediction)

#Join the preditions to the entire data set for plot
PredHosp <- PredHosp %>%
  left_join(HospPredVsReal, Prediction,by=c("LGA", "year","hosp_count"))  #this is droping some observations 

PredHosp <- transform(PredHosp, Pred2=ifelse(year==2013, hosp_count, Prediction))
view(PredHosp)

#Plot the difference for Sydney 
PredHospSyd <- PredHosp %>%
  filter(LGA == "Sydney")

ggplot(data = PredHospSyd) + 
  geom_line(mapping = aes(x = year, y = hosp_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Prediction"), show.legend = TRUE)+
  xlab("Year") + ylab("Hospitalization") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=750), colour="black", angle=90, vjust = 1.2) +
  labs(title="Syndey Hospitalization Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()
  

#Plot the difference for Parramatta 
PredHospPar <- PredHosp %>%
  filter(LGA=="Parramatta")

ggplot(data = PredHospPar) + 
  geom_line(mapping = aes(x = year, y = hosp_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Hospitalization") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=700), colour="black", angle=90, vjust = 1.2) +
  labs(title="Parramatta Hospitalization Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()

#Plot the difference for North Sydney
PredHospNorth <- PredHosp %>%
  filter(LGA=="North Sydney")

ggplot(data = PredHospNorth) + 
  geom_line(mapping = aes(x = year, y = hosp_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Hospitalization") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=700), colour="black", angle=90, vjust = 1.2) +
  labs(title="North Sydney Hospitalization Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()


#Plot the difference for North Sydney
PredHospInner <- PredHosp %>%
  filter(LGA=="Inner West")

ggplot(data = PredHospInner) + 
  geom_line(mapping = aes(x = year, y = hosp_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Hospitalization") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=700), colour="black", angle=90, vjust = 1.2) +
  labs(title="Inner West Hospitalization Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()

# ---- Deaths ---- 
PredDeaths <- join_health %>%
  select(LGA,year,death_count, Median_income, Population_Density,Working_Age_Population_15_64_years ) 

#partitioning (2012-2013)/(2014-2016)
trainsetDeaths <- PredDeaths %>%
  filter(year <= 2013)

testsetDeaths <- PredDeaths%>%
  filter(year > 2013)

nrow(trainsetDeaths)
nrow(testsetDeaths)
nrow(PredDeaths)
nrow(trainsetDeaths) + nrow(testsetDeaths) ==nrow(PredDeaths)

#Model
Deaths_glm<- glm(death_count ~ Median_income + Population_Density + Working_Age_Population_15_64_years, family = poisson(), data = trainsetDeaths)

testsetDeaths$Prediction <- predict(Deaths_glm, newdata=testsetDeaths, type = "response" )

summary(Deaths_glm)


#Just to compare the actual vs prediction
DeathsPredVsReal<- testsetDeaths %>%
  select(LGA, year ,death_count, Prediction)

#Join the preditions to the entire data set for plot
PredDeaths <- PredDeaths %>%
  left_join(DeathsPredVsReal, Prediction,by=c("LGA", "year","death_count"))  #this is droping some observations 

PredDeaths <- transform(PredDeaths, Pred2=ifelse(year==2013, death_count, Prediction))
view(PredDeaths)


#Plot the difference for Sydney 
PredDeathsSyd <- PredDeaths %>%
  filter(LGA == "Sydney")

ggplot(data = PredDeathsSyd) + 
  geom_line(mapping = aes(x = year, y = death_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Prediction"), show.legend = TRUE)+
  xlab("Year") + ylab("Deaths") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=30), colour="black", angle=90, vjust = 1.2) +
  labs(title="Syndey Deaths Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()


#Plot the difference for Parramatta 
PredDeathsPar <- PredDeaths %>%
  filter(LGA=="Parramatta")

ggplot(data = PredDeathsPar) + 
  geom_line(mapping = aes(x = year, y = death_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Deaths") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=17.5), colour="black", angle=90, vjust = 1.2) +
  labs(title="Parramatta Deaths Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()

#Plot the difference for North Sydney
PredDeathsNorth <- PredDeaths %>%
  filter(LGA=="North Sydney")

ggplot(data = PredDeathsNorth) + 
  geom_line(mapping = aes(x = year, y = death_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Deaths") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=13), colour="black", angle=90, vjust = 1.2) +
  labs(title="North Sydney Deaths Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()


#Plot the difference for North Sydney
PredDeathsInner <- PredDeaths %>%
  filter(LGA=="Inner West")

ggplot(data = PredDeathsInner) + 
  geom_line(mapping = aes(x = year, y = death_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Deaths") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=20), colour="black", angle=90, vjust = 1.2) +
  labs(title="Inner West Deaths Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()

# ---- Crime ----
#Use only variables important in the regression fro hosp
PredCrime <- join_health %>%
  select(LGA,violence_count, year,Population_Density,freq_daily,freq_weekly,freq_never, Working_Age_Population_15_64_years) 

#partitioning (2012-2013)/(2014-2016)
trainsetCrime <- PredCrime%>%
  filter(year <= 2013)

testsetCrime <- PredCrime%>%
  filter(year > 2013)

nrow(trainsetCrime)
nrow(testsetCrime)
nrow(PredCrime)
nrow(trainsetCrime) + nrow(testsetCrime) ==nrow(PredCrime)

#Model
Crime_glm<- glm(violence_count ~ year +Population_Density  + freq_daily+ freq_weekly + freq_never + Working_Age_Population_15_64_years, family = poisson(), data = trainsetCrime)

testsetCrime$Prediction <- predict(Crime_glm, newdata=testsetCrime, type = "response" )

summary(Crime_glm)


#Just to compare the actual vs prediction
CrimePredVsReal<- testsetCrime %>%
  select(LGA, year ,violence_count, Prediction)

#Join the preditions to the entire data set for plot
PredCrime <- PredCrime %>%
  left_join(CrimePredVsReal, Prediction,by=c("LGA", "year","violence_count"))  #this is droping some observations 

PredCrime <- transform(PredCrime, Pred2=ifelse(year==2013, violence_count, Prediction))
view(PredCrime)


#Plot the difference for Sydney 
PredCrimeSyd <- PredCrime %>%
  filter(LGA == "Sydney")

ggplot(data = PredCrimeSyd) + 
  geom_line(mapping = aes(x = year, y = violence_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Prediction"), show.legend = TRUE)+
  xlab("Year") + ylab("Violence") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=3500), colour="black", angle=90, vjust = 1.2) +
  labs(title="Syndey Violence Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()


#Plot the difference for Parramatta 
PredCrimePar <- PredCrime %>%
  filter(LGA=="Parramatta")

ggplot(data = PredCrimePar) + 
  geom_line(mapping = aes(x = year, y = violence_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Violence") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=1000), colour="black", angle=90, vjust = 1.2) +
  labs(title="Parramatta Violence Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()

#Plot the difference for North Sydney
PredCrimeNorth <- PredCrime %>%
  filter(LGA=="North Sydney")

ggplot(data = PredCrimeNorth) + 
  geom_line(mapping = aes(x = year, y = violence_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Crime") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=1200), colour="black", angle=90, vjust = 1.2) +
  labs(title="North Sydney Crime Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()


#Plot the difference for North Sydney
PredCrimeInner <- PredCrime %>%
  filter(LGA=="Inner West")

ggplot(data = PredCrimeInner) + 
  geom_line(mapping = aes(x = year, y = violence_count, color="Actual"), show.legend = TRUE)+
  geom_line(mapping = aes(x = year, y = Pred2, color="Predicted"), show.legend = TRUE)+
  xlab("Year") + ylab("Violence") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "blue", size=0.5)  +
  geom_text(aes(x=(2014), label="Law", y=1300), colour="black", angle=90, vjust = 1.2) +
  labs(title="Inner West Violence Count", subtitle = "Predictions vs Real")  +
  theme(plot.title = element_text(hjust=0.5),plot.subtitle  = element_text(hjust=0.5))+
  theme_classic()


# EXTRA TESTS ####
install.packages("AER")
library(AER)

dispersiontest(Hosp_glm)
dispersiontest(Deaths_glm)
dispersiontest(Crime_glm)

dispersiontest(hosp12)
dispersiontest(death13)
dispersiontest(violence8)



ggplot(PredHosp)+geom_bar(aes(x=hosp_count))
ggplot(PredDeaths)+geom_bar(aes(x=death_count))
ggplot(PredCrime)+geom_bar(aes(x=violence_count))

#has deaths a normal distr?
deathN<-glm(death_count ~ Median_income + Population_Density + Working_Age_Population_15_64_years + Postlaw, family = binomial(), data = join_health)
summary(deathN)


hospN<-glm(hosp_count ~  Population_Density + freq_daily+ freq_weekly+ Working_Age_Population_15_64_years + Postlaw, family = inverse.gaussian(), data = join_health)
summary(hospN)

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


################### Rent Regression ##################################
# Joining Rent Data Independently ####

# Rent Join with freq, income
join_rent_1 <- NULL
rent$Average_rent_3_bedroom <- NULL
rent$Average_rent_3_bedroom <- NULL

joined_data$Average_rent_3_bedroom <- NULL
joined_data$Average_rent_4_bedroom <- NULL


join_rent_1 <- inner_join(rent,frequency,by =c('LGA','year'))
# Observations dropped from 348 to 216
join_rent_2 <- inner_join(join_rent_1, offence_pop, by =c('LGA','year'))
# Observations dropped from 216 to 175
join_rent_3 <- inner_join(join_rent_2, income, by = c('LGA','year'))
# Observations dropped to 135

# Modek using join_rent_1 
model_rent1_1 <-lm( Average_rent_1_bedroom ~ . - LGA - X1 ,  data = join_rent_1)
summary(model_rent1_1)

# Model using joini_rent_2 , other independent variable violence_count
model_rent1_2 <-lm( Average_rent_1_bedroom ~ . - LGA - X1 - violence_count, data = join_rent_2)
summary(model_rent1_2)

# Modek using join_rent_3
model_rent1_3 <-lm( Average_rent_1_bedroom ~ . - LGA - X1 - violence_count , data = join_rent_3)
summary(model_rent1_3)

# Model using joined_data
# Predicting rent against all possible dependent variables ( removed othter joined independent variables)
model_rent1_all <-lm( Average_rent_1_bedroom ~ . - Average_rent_2_bedroom - year - LGA - violence_count -  New_business_entries_rate - X1.x - X1.y - hosp_rate - death_rate , data = joined_data)
summary(model_rent1_all)

# Adding a variable for pre-post lockout
joined_data$lockout = ifelse(joined_data$year < 2014, 0,1)

# Model using joined_data + lockout
# Predicting rent against all possible dependent variables ( removed othter joined independent variables)
model_rent1_5 <-lm( Average_rent_1_bedroom ~ . -Average_rent_2_bedroom - LGA - violence_count -  New_business_entries_rate - X1.x - X1.y - hosp_rate - death_rate , data = joined_data)
summary(model_rent1_5)
# Lockout variable is insignificant
joined_data$lockout <- NULL

# Train test split using pre post lockout laws.
joined_data<- joined_data[!(joined_data$LGA=="Kiama" | joined_data$LGA=="Wollondilly"),]

train_rent <- subset(joined_data, year == 2012 | year == 2013 | year == 2014)
test_rent <- subset(joined_data, year == 2015 | year == 2016 | year == 2014)

install.packages("caret")
library('caret')
library('ROCR')
library('Metrics')

rent_model <- lm( Average_rent_1_bedroom ~. - freq_less_weekly - freq_daily - Person_Population_Number_Total  - LGA - lockout - year - Average_rent_2_bedroom  - violence_count -  New_business_entries_rate - X1.x - X1.y - hosp_rate - death_rate , data = train_rent)
summary(rent_model)

test_rent$predicted <- predict(rent_model, test_rent, type="response")
test_rent$predicted <- as.numeric(test_rent$predicted)

mae( test_rent$Average_rent_1_bedroom , test_rent$predicted)
mse(test_rent$Average_rent_1_bedroom , test_rent$predicted)
rmse(test_rent$Average_rent_1_bedroom , test_rent$predicted)  

### Plots
varImp(rent_model)
p1 <- plot(rent_model)
a <- varImp(rent_model)
plot(a)
write.csv(a, file = 'J.csv')
VIMP <- read.csv('J.csv')

plot_var_1 <- ggplot(VIMP, aes( x= X , y = Overall , fill = X)) +
  geom_bar(stat="identity")  + theme_bw() + theme(axis.text.x=element_blank()) +
  ggtitle("Variable Importance Plot : 1 Bedroom Rent")  + ylab("Importance") + xlab("Variable")
plot_var_1

syd_all <-  joined_data[(joined_data$LGA=="Sydney"),]
Syd_Pred <- test_rent[(test_rent$LGA=="Sydney"),]


ggplot() + 
  geom_line(data = syd_all , aes(x = year, y = Average_rent_1_bedroom), color = "blue") +
  geom_point(data = syd_all , aes(x = year, y = Average_rent_1_bedroom), color = "black") +
    geom_line(data = Syd_Pred , aes(x = year, y = predicted  ), color = "red") +
  geom_point(data = Syd_Pred , aes(x = year, y = predicted  ), color = "black") +
    xlab('Dates') + geom_line(aes( x = 2014 , y=450:560  ), color = "black", linetype=2) +
theme_bw() +  ylab('Rent') + ggtitle("Sydney LGA 1 Bedroom Rent : Actual vs Predicted") + theme(legend.justification = c("right", "top")) 

woo_all <-  joined_data[(joined_data$LGA=="Parramatta"),]
woo_Pred <- test_rent[(test_rent$LGA=="Parramatta"),]

ggplot() + 
  geom_line(data = woo_all , aes(x = year, y = Average_rent_1_bedroom), color = "blue") +
  geom_point(data = woo_all , aes(x = year, y = Average_rent_1_bedroom), color = "black") +
  geom_line(data = woo_Pred , aes(x = year, y = predicted  ), color = "red") +
  geom_point(data = woo_Pred , aes(x = year, y = predicted  ), color = "black") +
  xlab('Dates') + geom_line(aes( x = 2014 , y=300:500  ), color = "black", linetype=2) +
  theme_bw() +  ylab('Rent') + ggtitle("Parramatta LGA 1 Bedroom Rent : Actual vs Predicted") + theme(legend.justification = c("right", "top")) 

woo_all <-  joined_data[(joined_data$LGA=="North Sydney"),]
woo_Pred <- test_rent[(test_rent$LGA=="North Sydney"),]

ggplot() + 
  geom_line(data = woo_all , aes(x = year, y = Average_rent_1_bedroom), color = "blue") +
  geom_point(data = woo_all , aes(x = year, y = Average_rent_1_bedroom), color = "black") +
  geom_line(data = woo_Pred , aes(x = year, y = predicted  ), color = "red") +
  geom_point(data = woo_Pred , aes(x = year, y = predicted  ), color = "black") +
  xlab('Dates') + geom_line(aes( x = 2014 , y=430:600  ), color = "black", linetype=2) +
  theme_bw() +  ylab('Rent') + ggtitle("North Sydney LGA 1 Bedroom Rent : Actual vs Predicted") + theme(legend.justification = c("right", "top")) 


###### 2 Bedroom Model #########################

joined_data<- joined_data[!(joined_data$LGA=="Kiama" | joined_data$LGA=="Wollondilly"),]

rent_model_1 <- lm( Average_rent_2_bedroom ~ . -Average_rent_1_bedroom - LGA - violence_count -  New_business_entries_rate - X1.x - X1.y - hosp_rate - death_rate , data = joined_data)
summary(rent_model_1)

train_rent <- subset(joined_data, year == 2012 | year == 2013 | year == 2014)
test_rent <- subset(joined_data, year == 2015 | year == 2016 | year == 2014)

rent_model <- lm( Average_rent_2_bedroom ~ . - freq_less_weekly -freq_daily  - Person_Population_Number_Total - year - lockout - Average_rent_1_bedroom - LGA - violence_count -  New_business_entries_rate - X1.x - X1.y - hosp_rate - death_rate , data = train_rent)
summary(rent_model)

test_rent$predicted <- predict(rent_model, test_rent, type="response")
test_rent$predicted <- as.numeric(test_rent$predicted)

mae( test_rent$Average_rent_2_bedroom , test_rent$predicted)
mse(test_rent$Average_rent_2_bedroom , test_rent$predicted)
rmse(test_rent$Average_rent_2_bedroom , test_rent$predicted)  

### Plots #########
varImp(rent_model)
p1 <- plot(rent_model)
p1
c <- varImp(rent_model)
write.csv(c, file = 't.csv')
VIMP <- read.csv('t.csv')

plot_var_1 <- ggplot(VIMP, aes( x= X , y = Overall , fill = X)) +
  geom_bar(stat="identity")  + theme_bw() + theme(axis.text.x=element_blank()) +
  ggtitle("Variable Importance Plot : 2 Bedroom Rent")  + ylab("Importance") + xlab("Variable")
plot_var_1

syd_all <-  joined_data[(joined_data$LGA=="Sydney"),]
Syd_Pred <- test_rent[(test_rent$LGA=="Sydney"),]

ggplot() + 
  geom_line(data = syd_all , aes(x = year, y = Average_rent_1_bedroom), color = "blue") +
  geom_point(data = syd_all , aes(x = year, y = Average_rent_1_bedroom), color = "black") +
  geom_line(data = Syd_Pred , aes(x = year, y = predicted  ), color = "red") +
  geom_point(data = Syd_Pred , aes(x = year, y = predicted  ), color = "black") +
  xlab('Dates') + geom_line(aes( x = 2014 , y=490:730  ), color = "black", linetype=2) +
  theme_bw() +  ylab('Rent') + ggtitle("Sydney LGA 2 Bedroom Rent : Actual vs Predicted") + theme(legend.justification = c("right", "top")) 

woo_all <-  joined_data[(joined_data$LGA=="Parramatta"),]
woo_Pred <- test_rent[(test_rent$LGA=="Parramatta"),]

ggplot() + 
  geom_line(data = woo_all , aes(x = year, y = Average_rent_1_bedroom), color = "blue") +
  geom_point(data = woo_all , aes(x = year, y = Average_rent_1_bedroom), color = "black") +
  geom_line(data = woo_Pred , aes(x = year, y = predicted  ), color = "red") +
  geom_point(data = woo_Pred , aes(x = year, y = predicted  ), color = "black") +
  xlab('Dates') + geom_line(aes( x = 2014 , y=300:600 ), color = "black", linetype=2) +
  theme_bw() +  ylab('Rent') + ggtitle("Parramatta LGA 2 Bedroom Rent : Actual vs Predicted") 

woo_all <-  joined_data[(joined_data$LGA=="North Sydney"),]
woo_Pred <- test_rent[(test_rent$LGA=="North Sydney"),]

ggplot() + 
  geom_line(data = woo_all , aes(x = year, y = Average_rent_1_bedroom), color = "blue") +
  geom_point(data = woo_all , aes(x = year, y = Average_rent_1_bedroom), color = "black") +
  geom_line(data = woo_Pred , aes(x = year, y = predicted  ), color = "red") +
  geom_point(data = woo_Pred , aes(x = year, y = predicted  ), color = "black") +
  xlab('Dates') + geom_line(aes( x = 2014 , y=430:750  ), color = "black", linetype=2) +
  theme_bw() +  ylab('Rent') + ggtitle("North Sydney LGA 2 Bedroom Rent : Actual vs Predicted") + theme(legend.justification = c("right", "top")) 

######### Business  Model ##################

# Joining Business Data Independently ##

# Business Join with freq, income
join_business_1 <- NULL
join_business_1 <- inner_join(business,frequency,by =c('LGA','year'))
# Observations dropped from 900 to 600
join_business_2 <- inner_join(join_business_1, offence_pop, by =c('LGA','year'))
join_business <- inner_join(join_business_2, income, by = c('LGA','year'))
# Observations dropped to 450

join_business$X1 <- NULL
join_business$violence_count <- NULL

join_business <-  join_business[complete.cases(join_business), ]
join_business$lockout = ifelse(join_business$year < 2014, 0,1)

model_business_1 = lm(New_business_entries_rate ~. - LGA , data = join_business)
summary(model_business_1)

model_business_2 = lm(New_business_entries_rate ~. - LGA  - year , data = join_business)
summary(model_business_2)

model_business_3 = lm(New_business_entries_rate ~. -Person_Population_Number_Total  -freq_less_weekly - freq_never - freq_daily - freq_weekly  - LGA - lockout - year , data = join_business)
summary(model_business_3)

plot(model_business_3)

varImp(model_business_3)
a <- varImp(model_business_3)
write.csv(a, file = 'do.csv')
VIMP <- read.csv('do.csv')

plot_var_1 <- ggplot(VIMP, aes( x= X , y = Overall , fill = X)) +
  geom_bar(stat="identity")  + theme_bw() + theme(axis.text.x=element_blank()) +
  ggtitle("Variable Importance Plot : New Business")  + ylab("Importance") + xlab("Variable")
plot_var_1

train_business <- subset(join_business, year == 2014)
test_business <- subset(join_business, year == 2015 | year == 2016 | year == 2014)

model_business = lm(New_business_entries_rate ~. - LGA - year - freq_never - freq_daily - freq_weekly, data = test_business)
summary(model_business)

test_business$predicted <- predict(model_business, test_business, type="response")

syd_bus <-  join_business[(join_business$LGA=="Sydney"),]
syd_pred <- test_business[(test_business$LGA=="Sydney"),]

ggplot() + 
  geom_line(data = syd_bus , aes(x = year, y = (New_business_entries_rate *100)), color = "blue") +
  geom_point(data = syd_bus , aes(x = year, y = (New_business_entries_rate *100)), color = "black") +
  geom_line(data = syd_pred , aes(x = year, y = (predicted*100))  , color = "red") +
  geom_point(data = syd_pred , aes(x = year, y = (predicted*100)), color = "black") +
  xlab('Dates') + geom_line(aes( x = 2014 , y=0:50  ), color = "black", linetype=2) +
 theme_bw() +  ylab('New Business Entry Rate (%)') + ggtitle("New Business Entries : Actual (Blue) vs Predicted (Red) ") 


########################################## ;) ##############################
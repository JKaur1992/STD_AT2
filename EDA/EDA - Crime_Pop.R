rm(list=ls())

library(tidyverse)
library(readxl)
library(Amelia)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

setwd("~/GitHub/STD_AT2/CLEAN DATA")
offence_data <- read_csv("offence_data.csv")
colnames(offence_data)
str(offence_data)

#split date into 2 columns
offence_data <- offence_data %>%
  separate(.,"year",c("Day","Month","Year"),sep="/")

####################################
#filter data for 2014, Sydney and plot it
####################################
offence_data_2014 <- filter(offence_data, ( Year == 2014))
offence_data_2014 <- filter(offence_data_2014, ( LGA == 'Sydney'))
offence_data_2014 <- filter(offence_data_2014, ( Postcode == 2000))
#plot it
ggplot(offence_data_2014, aes(x = Month, y = violence_count, group = 1)) +
  geom_point() +
  geom_line()

##############################################################
#combine the data from monthly to annual
offence_data_ag = aggregate(violence_count ~ Year + LGA, data = offence_data, FUN = sum)
str(offence_data_ag)

#change year from character to number so both merging datasets have same structure
offence_data_ag[1:1] = lapply(offence_data_ag[1:1], as.numeric)
str(offence_data_ag)

#filter for 2012-2017 data
offence_data_ag <- filter(offence_data_ag, ( Year == 2012 | Year == 2013 | Year == 2014 | Year == 2015 | Year == 2016 | Year == 2017))
unique(offence_data_ag$Year) #check if this worked

############################################################################################
###########EDA for offence data ONLY
############################################################################################

missmap(offence_data_ag, main = "Missing values vs observed") #nothing missing obviously
mean(offence_data_ag$violence_count)
median(offence_data_ag$violence_count)
max(offence_data_ag$violence_count)
min(offence_data_ag$violence_count)

#####further filtering required for LGAs#############################
#to filter out LGAs, need to read through some of them to pick - 'Sydney', 'Burwood', 'Campbelltown', 'Canterbury-Bankstown', 'Blacktown', 
#'Liverpool', 'Central Darling', 'Mid-Western Regional' , 'Unincorporated NSW', 'Hornsby', 'Inner West', 
#'North Sydney', 'Parramatta', 'Strathfield', 'Mosman', 'Bayside', Northern Beaches', 'North Sydney', 'Randwick'

offence_data_ag_filter <- filter(offence_data_ag, LGA == 'Strathfield')
ggplot(data = offence_data_ag_filter) + geom_line(mapping = aes(x = Year, y = violence_count))

offence_data_ag_filter1 <- filter(offence_data_ag, LGA == 'Sydney')
offence_data_ag_filter2 <- filter(offence_data_ag, LGA == 'Bayside')
offence_data_ag_filter3 <- filter(offence_data_ag, LGA == 'Central Darling')
offence_data_ag_filter4 <- filter(offence_data_ag, LGA == 'Parramatta')
offence_data_ag_filter5 <- filter(offence_data_ag, LGA == 'Canterbury-Bankstown')
offence_data_ag_filter6 <- filter(offence_data_ag, LGA == 'Inner West')

ggplot(data = offence_data_ag_filter1) + 
  geom_line(mapping = aes(x = Year, y = violence_count)) #Sydney

ggplot(data = offence_data_ag_filter2) + 
  geom_line(mapping = aes(x = Year, y = violence_count)) #Bayside

ggplot(data = offence_data_ag_filter3) + 
  geom_line(mapping = aes(x = Year, y = violence_count)) #Central darling

ggplot(data = offence_data_ag_filter4) + 
  geom_line(mapping = aes(x = Year, y = violence_count)) #Parramatta

ggplot(data = offence_data_ag_filter5) + 
  geom_line(mapping = aes(x = Year, y = violence_count)) #caterbury-bankstown

ggplot(data = offence_data_ag_filter6) + 
  geom_line(mapping = aes(x = Year, y = violence_count)) #Inner-west

################################################################################
##Filter out the important LGAs based on above analysis
#LGAs with noticable changes - Randwick?, North Sydney, Hornsby, Mosman (slightly), Uninc.NSW, Liverpool?, 
#Burwood, Sydney (obviously), Parramatta, Central darling, Bayside, Canterbury-bankstown, Inner-west, Strathfield

offence_data_ag <- filter(offence_data_ag, ( LGA == 'Sydney' | LGA == 'Randwick' | LGA == 'Canterbury-Bankstown' | 
                                               LGA == 'Central Darling' | LGA == 'Parramatta' | LGA == 'North Sydney' | 
                                               LGA == 'Inner West' | LGA == 'Hornsby' | LGA == 'Strathfield' | LGA == 'Burwood' | 
                                               LGA == 'Mosman' | LGA == 'Bayside'))
##plot 3 codes that work
ggplot(data = offence_data_ag) + 
  geom_line(mapping = aes(x = Year, y = violence_count, color = LGA)) + facet_wrap(vars(LGA))

#ggplot(offence_data_ag,aes(x = `Year`, y = `violence_count`, color=LGA)) +
#  geom_line() +
#  facet_wrap(vars(LGA)) +
#  geom_smooth()

ggplot(data = offence_data_ag, mapping = aes(x = Year, y = violence_count, colour = LGA)) + 
  geom_line() +
  geom_point()

###########just to filter it further and better
offence_data_ag1 <- filter(offence_data_ag, ( LGA == 'Sydney' | LGA == 'Inner West' | LGA == 'Bayside' | 
                                                LGA == 'Canterbury-Bankstown' | LGA == 'Hornsby' | 
                                                LGA == 'North Sydney' | LGA == 'Parramatta' | LGA == 'Unincorporated NSW')) 

unique(offence_data_ag1$LGA)

##plot 3 codes that work
ggplot(data = offence_data_ag1) + 
  geom_line(mapping = aes(x = Year, y = violence_count, color = LGA)) + facet_wrap(vars(LGA))

#ggplot(offence_data_ag1,aes(x = `Year`, y = `violence_count`, color=LGA)) +
#  geom_line() +
#  facet_wrap(vars(LGA)) +
#  geom_smooth()

ggplot(data = offence_data_ag1, mapping = aes(x = Year, y = violence_count, colour = LGA)) + 
  geom_line() +
  geom_point()

###########just to filter it further and better
offence_data_ag2 <- filter(offence_data_ag, ( LGA == 'Sydney' | LGA == 'Inner West' | LGA == 'Bayside' | 
                                                LGA == 'Hornsby' | LGA == 'Parramatta' | LGA == 'Unincorporated NSW'))

unique(offence_data_ag2$LGA)

##plot 3 codes that work
ggplot(data = offence_data_ag2) + 
  geom_line(mapping = aes(x = Year, y = violence_count, color = LGA)) + facet_wrap(vars(LGA))

#ggplot(offence_data_ag2,aes(x = `Year`, y = `violence_count`, color=LGA)) +
#  geom_line() +
#  facet_wrap(vars(LGA)) +
#  geom_smooth()

ggplot(data = offence_data_ag2, mapping = aes(x = Year, y = violence_count, colour = LGA)) + 
  geom_line() +
  geom_point()

#Sydney vs rest
offence_data_ag3 <- filter(offence_data_ag, ( LGA == 'Inner West' | LGA == 'Bayside' | LGA == 'North Sydney' | 
                                                LGA == 'Hornsby' | LGA == 'Parramatta' | LGA == 'Unincorporated NSW'))
ggplot(data = offence_data_ag3) + 
  geom_line(mapping = aes(x = Year, y = violence_count, color = LGA)) + facet_wrap(vars(LGA))

ggplot(data = offence_data_ag3, mapping = aes(x = Year, y = violence_count, colour = LGA)) + 
  geom_line() +
  geom_point()

############################################################################################
########NEXT DATASET
############################################################################################
population <- read_csv("Population_Clean.csv")
colnames(population)

#subset the population data for total and density
population_subset = select(population, LGA, Year, Person_Population_Number_Total, Population_Density)
colnames(population_subset)
str(population_subset)
#population_subset[3:4] = lapply(population_subset[3:4], as.numeric)

#MERGE these datasets
offence_data_EDA_full <- full_join(offence_data_ag, population_subset, by = c("Year", "LGA")) 
missmap(offence_data_EDA_full, main = "Missing values vs observed") #checking for missing values.
#above code still has some missing values so to have data without missing values, try Inner join

offence_data_EDA <- inner_join(offence_data_ag, population_subset, by = c("Year", "LGA"))

# Export to .csv Files 
write_csv(offence_data_EDA, path = "offence_data_EDA.csv")

###########################################################
##EDA for both
###########################################################

missmap(offence_data_EDA, main = "Missing values vs observed") #data looks good.
#check the structure of the data 
str(offence_data_EDA) 
#not good - change it but keep getting 
#offence_data_EDA[5:5] = lapply(offence_data_EDA[5:5], as.numeric)
#need to change 4th column as well but not working!!!!

#need to create a new variable for crime to pop ratio and then plot that against time and LGAs - use the LGA groupings from above
offence_pop <- mutate(offence_data_EDA, Crime_Rate = Person_Population_Number_Total/violence_count)

offence_pop <- filter(offence_data_EDA, LGA == 'Sydney')

ggplot(data = offence_pop) + 
  geom_point(mapping = aes(x = Year, y = violence_count))

ggplot(data = offence_pop, mapping = aes(x = Year, y = violence_count, colour = LGA)) + 
  geom_line() +
  geom_point()

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

#combine the data from monthly to annual
offence_data_ag=aggregate(violence_count ~ Year + LGA, data = offence_data, FUN = sum)
str(offence_data_ag)

#change year from character to number so both merging datasets have same structure
offence_data_ag[1:1] = lapply(offence_data_ag[1:1], as.numeric)
str(offence_data_ag)

######EDA for offence data alone########
missmap(offence_data_ag, main = "Missing values vs observed") #nothing missing obviously
unique(offence_data_ag$LGA)
mean(offence_data_ag$violence_count)
median(offence_data_ag$violence_count)

#filter by LGA - 'Sydney','Campbelltown','Canterbury-Bankstown','Central Darling','Hornsby','Inner West','North Sydney','Parramatta','Strathfield'
#filter <- c('Sydney', 'Burwood', 'Campbelltown', 'Canterbury-Bankstown', 'Central Darling', 'Hornsby', 'Inner West', 'North Sydney', 'Parramatta', 'Strathfield')
offence_data_ag_filter1 <- filter(offence_data_ag, LGA == 'Sydney')
offence_data_ag_filter2 <- filter(offence_data_ag, LGA == 'Burwood')
offence_data_ag_filter3 <- filter(offence_data_ag, LGA == 'Central Darling')
offence_data_ag_filter4 <- filter(offence_data_ag, LGA == 'Parramatta')

ggplot(data = offence_data_ag_filter1) + 
  geom_line(mapping = aes(x = Year, y = violence_count))

ggplot(data = offence_data_ag_filter2) + 
  geom_line(mapping = aes(x = Year, y = violence_count))

ggplot(data = offence_data_ag_filter3) + 
  geom_line(mapping = aes(x = Year, y = violence_count))

ggplot(data = offence_data_ag_filter4) + 
  geom_line(mapping = aes(x = Year, y = violence_count))

ggplot(offence_data_ag,aes(x = `Year`, y = `Violence Count`, color=LGA)) +
  geom_point() +
  facet_wrap(vars(LGA)) +
  geom_smooth(method = 'lm')

ggplot(data = offence_data_ag) +
  geom_bar(mapping = aes(x = LGA))

ggplot(data = offence_data_ag, mapping = aes(x = Year, colour = LGA)) +
  geom_freqpoly(binwidth = 0.1) #i want Y to be violece count

#ggplot(offence_data_ag, aes(Year)) + 
  geom_line(aes(y = violence_count, colour = "LGA")), linetype="dotted", color = "black", size=1.5)

###############NEXT DATASET########################################
population <- read_csv("Population_Clean.csv")
colnames(population)

#subset the population data for total and density
population_subset = select(population, LGA, Year, Person_Population_Number_Total, Population_Density)
colnames(population_subset)
str(population_subset)

#MERGE these datasets
#mergeCol <- c("LGA", "Year")
#offencedata <- merge(offence_data_ag, population_subset, by = mergeCol)
offence_data_EDA <- full_join(offence_data_ag, population_subset, by = c("Year", "LGA"))

# Export to .csv Files 
write_csv(offence_data_EDA, path = "offence_data_EDA.csv")

###########################################################
##EDA
###########################################################

missmap(offence_data_EDA, main = "Missing values vs observed") #checking for missing values. The merging didn't work

offence_data_ag <- filter(offence_data_ag, LGA == 'Sydney')
ggplot(data = offence_data_ag) + 
  geom_point(mapping = aes(x = Year, y = violence_count))

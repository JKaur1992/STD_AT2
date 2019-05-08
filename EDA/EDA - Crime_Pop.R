rm(list=ls())

library(tidyverse)
library(readxl)
library(Amelia)
library(dplyr)
library(tidyr)
library(lubridate)

offence_data <- read_csv("offence_data.csv")
colnames(offence_data)

#change year from 2-digits to 4-digits
offence_data$year <- parse_date_time('Jan-08',orders='my')

#split date into 2 columns
offence_data <- offence_data %>%
  separate(.,"year",c("Year","Month"),sep="-")

#combine the data from monthly to annual
offence_data_ag=aggregate(violence_count ~ Postcode + Offence + Year + State + LGA, data = offence_data, FUN = sum)

####
population <- read_csv("Population_Clean.csv")
colnames(population)

#subset the population data for total and density
population_subset = select(population, LGA, Year, Person_Population_Number_Total, Population_Density)
colnames(population_subset)

#merge these datasets
mergeCol <- c("LGA", "Year")
offencedata <- merge(offence_data, population_subset, by = mergeCol)

###########################################################
##EDA
###########################################################
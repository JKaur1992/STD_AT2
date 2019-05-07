rm(list=ls())

library(tidyverse)
library(readxl)
library(Amelia)
library(dplyr)
library(tidyr)
library(lubridate)

###########################################################
##EDA
###########################################################
offence_data <- read_csv("offence_data.csv")
population <- read_csv("Population_Clean.csv")

colnames(population)
colnames(offence_data)

offence_data$year <- as.POSIXct("Jan-08", "%b-%g")
offence_data$year <- parse_date_time('Jan-08',orders='my')

#split date into 2 columns
offence_data_convert <- offence_data %>%
  separate(.,"year",c("Month","Year"),sep="-")

#subset the population data for total and density
population_subset = select(population, LGA, Year, Person_Population_Number_Total, Population_Density)

mergeCol <- c("LGA")
offencedata <- merge(offence_data, population_subset, by = mergeCol)

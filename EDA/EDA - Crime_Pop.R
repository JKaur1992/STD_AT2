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
offence_data$year = as.Date(offence_data$year, format = "%m/%y")

datetxt <- c("2010-01-02", "2010-02-03", "2010-09-10")
datetxt <- c("Jan-08" : "Dec-18")
datetxt <- as.Date(datetxt)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))


population_subset = select(population, LGA, Year, Person_Population_Number_Total, Population_Density)

mergeCol <- c("LGA")
offencedata <- merge(offence_data, population_subset, by = mergeCol)

rm(list=ls())

library(tidyverse)
library(readxl)
library(Amelia)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)

offence_data <- read_csv("offence_data.csv")
colnames(offence_data)
str(offence_data)

#combine the data from monthly to annual
offence_data_ag=aggregate(violence_count ~ Year + LGA, data = offence_data, FUN = sum)

#########################################################################################
#change year from 2-digits to 4-digits
offence_data$year <- parse_date_time(c('Jan-08':'Dec-18'),orders='my')
unique(offence_data$year) # this shows that the above code is not working

offence_data$year <- as.Date("Jan-08", format = "%b - %y")

offence_data$year <- strptime(as.character(offence_data$year), "%b-%y")
offence_data$year <- format(as.Date(offence_data$year, format = "%b-%y"), "%m-%Y")

library(lubridate)
day <- c("Jan-08":"Dec-18")
as.Date(parse_date_time(day,"dmy"))
##########################################################################################

#split date into 2 columns
offence_data <- offence_data %>%
  separate(.,"year",c("Month","Year"),sep="-")


#change year from character to number so both merging datasets have same structure
offence_data_ag[1:1] = lapply(offence_data_ag[1:1], as.numeric)

####
population <- read_csv("Population_Clean.csv")
colnames(population)

#subset the population data for total and density
population_subset = select(population, LGA, Year, Person_Population_Number_Total, Population_Density)
colnames(population_subset)
str(population_subset)
str(offence_data_ag)

#merge these datasets
#mergeCol <- c("LGA", "Year")
#offencedata <- merge(offence_data_ag, population_subset, by = mergeCol)
offence_data_EDA <- full_join(offence_data_ag, population_subset, by = c("Year", "LGA"))

# Export to .csv Files 
write_csv(offence_data_EDA, path = "offence_data_EDA.csv")

###########################################################
##EDA
###########################################################


missmap(offence_data_EDA, main = "Missing values vs observed") #checking for missing values. The merging didn't work

mean(offence_data_ag$violence_count)

offence_data_ag <- filter(offence_data_ag, LGA == 'Sydney')
ggplot(data = offence_data_ag) + 
  geom_point(mapping = aes(x = Year, y = violence_count))

library(tidyverse)
library(dplyr)
library(stringr)
library(tau)
library(tm)
library(ggplot2)


####  Cleaning of new data set POPULATION####
Population <- read_csv("demographics/Population.csv")
str(Population)

# Delete extra Data columns 
Population <- Population[,-c(79:108)]

#Delete extra rows
Population <- Population[-c(1:6),]
Population <- Population[-c(3919:3925),]



#Change column Names
colnames(Population)[colnames(Population)=="X1"] <- "Code"
colnames(Population)[colnames(Population)=="Australian Bureau of Statistics"] <- "LGA"
colnames(Population)[colnames(Population)=="X3"] <- "Year"
colnames(Population)[colnames(Population)=="X71"] <- "Male_Median_Age"
colnames(Population)[colnames(Population)=="X72"] <- "Female_Median_Age"
colnames(Population)[colnames(Population)=="X73"] <- "Person_Median_Age"
colnames(Population)[colnames(Population)=="X74"] <- "Births"
Population$X75<- NULL # Delete Total Fertility Rate (per female)
colnames(Population)[colnames(Population)=="X76"] <- "Deaths"
Population$X77<- NULL # Delete Standardised_Death_Rate_per_1000_population)
colnames(Population)[colnames(Population)=="X78"] <- "Population_Density"

# Add prefixes in coulmns 
colnames(Population)[4:70]<- Population[1,4:70] # the first row will be the header
Population <- Population [-c(1:2),] # Remove firts row

colnames(Population)[4:12] <- paste("Person_Population_Rate", colnames(Population[,c(4:12)]), sep = "_") # Add a prefix to Person Population Rate 

colnames(Population)[13:31] <- paste("Male_Population_Number", colnames(Population[,c(13:31)]), sep = "_") # Add a prefix to Male Population Number 

colnames(Population)[32:50] <- paste("Female_Population_Number", colnames(Population[,c(32:50)]), sep = "_") # Add a prefix to Female Population Number 

colnames(Population)[51:69] <- paste("Person_Population_Number", colnames(Population[,c(51:69)]), sep = "_") # Add a prefix to Person Population Number 

#Delete extra words in LGA names 
Population$LGA<- gsub("\\s*\\([^\\)]+\\)","",as.character(Population$LGA))

# we first need to extract comas and then transform. THIS IS WHAT WE NEED TO DO WITH ALL VARIABLES
#Population$Population_Density <- gsub(",","",Population$Population_Density)
#Population$Population_Density<- as.numeric(as.character(Population$Population_Density))

#Delte comas from  ALL data set
Population[,3:76] <- as.data.frame(lapply(Population[,3:76], function(y) gsub(",","", y)))  ##It changed all into factors


Population[,3:76]<-as.numeric(as.character(Population[,3:76]))


#TRANFORM THE COULMNS THAT ARE GOING TO BE USED LATER ON IN THE MERGE AS.NUMBERS, WE HAVE THEM AS.CHARACTERS
Population$Code<- as.numeric(as.character(Population$Code))
Population$Year<- as.numeric(as.character(Population$Year))
Population$Person_Population_Number_Total<- as.numeric(as.character(Population$Person_Population_Number_Total))






library(magrittr)
cols = c(1, 3, 4, 5)
df[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))


#Export
write.csv(Population,"Population_Clean.csv")



#### Population EDA ####

#Extract Person_Population_Number_Total for merging with crime
Population_Clean <- read_csv("CLEAN DATA/Population_Clean.csv")
population_subset <- Population_Clean
population_subset$X1<- NULL
population_subset <- population_subset %>%
  select(LGA, Year, Person_Population_Number_Total, Population_Density)

# STRUCTURE
str(population_subset)
#Population[-c(2,3)]<- sapply(Population, as.numeric)
population_subset <- gsub(",","",population_subset)

population_subset$Person_Population_Number_Total<-as.numeric(as.character(population_subset$Person_Population_Number_Total))
population_subset$Population_Density <-as.numeric(as.character(population_subset$Population_Density))








######## Merge and EDA with crime DONALD's MERGING  ######
offence_data <- read_csv("CLEAN DATA/offence_data.csv")
offence_data <- offence_data %>%
  separate(.,"year",c("Day","Month","Year"),sep="/")
offence_data=aggregate(violence_count ~ Year + LGA, data = offence_data, FUN = sum)
str(offence_data)
offence_data[1:1] = lapply(offence_data_ag[1:1], as.numeric)
str(offence_data)

offence_data_EDA <- full_join(offence_data, population_subset, by = c("Year", "LGA"))

####--------------------------------------------------------------------------


offence_data_EDA2012_2017 <- offence_data_EDA 

offence_data_EDA2012_2017$Person_Population_Number_Total<- as.numeric(offence_data_EDA2012_2017$Person_Population_Number_Total)

offence_data_EDA2012_2017$Population_Density <- as.numeric(offence_data_EDA2012_2017$Population_Density)


offence_data_EDA2012_2017%>%   # not taking Sydney??
  filter(Year==2012:2017)

offence_data_EDA2012_2017 %>%
  filter(LGA=="Sydney") %>%
  geom_line(mapping = aes(x =Year, y= Population_Density))
  
  
  #%>%
  facet_wrap(.~ LGA)
  
  
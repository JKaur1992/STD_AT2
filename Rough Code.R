library(tidyverse)
library(readxl)
library(Amelia)

## these are just extra packages that i don't know if we'll need yet. I just copied them from my DAM Assignments for EDA.
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library(gtools) # for discretisation
library(corrplot)
library(Hmisc)
library(devtools)
library(PerformanceAnalytics)
library(FactoMineR)

################################
## Load the data
################################

postcode_data <- read_csv("7_PostcodeData2018.csv")
suburbdata <- read_csv("9_SuburbData2018.csv")

Alcohol_Violence <- read_excel("1_Alcohol Related Violence.xls")
Alcohol_Assualts <- read.csv("3_EDITED - Incidents of Assault (Non-domestic assault) occurring during Weekends Nights from January 2009 to December 2018.csv")
Domestic_Violence <- read.csv("4_EDITED - Incidents of Assault occurring during Weekends Nights on Residential Premises from January 2009 to December 2018.csv")
NSWcrimes <- read_excel("5_Incident_by_NSW.xlsm")
OPT <- read_excel("6_Offences on public transport.xls")
RCIdata <- read_excel("8_RCI_offencebymonth.xlsm") # R's for some reasons renamed all the variables. how do i go back to original names?

healthdata <- read_excel("2_AlcoholConsumptionHealthNSW.xls")

################################
## Basic Data Read
################################

sumamry(postcode_data)
dim(postcode_data)
str(postcode_data)
names(postcode_data)

summary(suburbdata)
dim(suburbdata)
str(suburbdata)
names(suburbdata)

summary(RCIdata)
dim(RCIdata)
str(RCIdata)
names(RCIdata)

summary(Alcohol_Assualts)
dim(Alcohol_Assualts)
str(Alcohol_Assualts)
names(Alcohol_Assualts)

summary(Domestic_Violence)
dim(Domestic_Violence)
str(Domestic_Violence)
names(Domestic_Violence)

#For Annual Alcohol related Domestic and Non-Domestic Crime data, 
## NEED TO ADD A COLUMN for violence type - Domestic and Non-Domestic 
## THEN MERGE the datasets.
total_alcohol <- rbind(Alcohol_Assualts, Domestic_Violence) ## this is the merging code
suburb_data <- rbind(RCIdata, suburbdata) ## this isn't working due to renaming of the variables in RCI file. maybe just skip the RCI file?

##################################################
## Check for missing values
##################################################

missmap(postcode_data, col=c("blue", "red"), legend=FALSE)
missmap(suburbdata, col=c("blue", "red"), legend=FALSE)

missmap(Alcohol_Assualts, col=c("blue", "red"), legend=FALSE)
missmap(Domestic_Violence, col=c("blue", "red"), legend=FALSE)
missmap(RCIdata, col=c("blue", "red"), legend=FALSE)

##OR

missmap(postcode_data, main = "Missing values vs observed")
missmap(suburbdata, main = "Missing values vs observed")

missmap(Alcohol_Assualts, main = "Missing values vs observed")
missmap(Domestic_Violence, main = "Missing values vs observed")
missmap(RCIdata, main = "Missing values vs observed")

########################################
##Data engineering
########################################

# select() allows you to rapidly zoom in on a useful subset using operations based on the names of the variables. 
select(postcode_data, Postcode, March 2014:Dec 2014) ## The variable need to be re-named so they can be used.

# Create a subset for Variable=X and Variable=Y
sub = subset(postcode_data, Offence category == "Liquor", select = Postcode:Dec 2018) ##NEED TO SWAP SPACES WITH _
str(sub)

############################################
##EDA
############################################

plot(Alcohol_Assualts$LGA)

ggplot(Alcohol_Violence, aes(x = Year to December 2015, fill = default)) +
  geom_bar() +
  labs(x = '2015') +
  theme_excel()


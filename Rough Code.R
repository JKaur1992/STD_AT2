setwd("~/GitHub/STD_AT2")

library(tidyverse)
library(readxl)
library(Amelia)
library(httr)
library(rsdmx)
library(jsonlite)
library(dplyr)
library(lubridate)

################################
## Load the data
################################

postcode_data <- read_csv("7_PostcodeData2018.csv")
## suburbdata <- read_csv("9_SuburbData2018.csv")
alcohol_consumption <- read_csv("beh_alc_lhn_trend.csv")
alcohol_hospitalisations <- read_csv("beh_alcafhos_lhn_trend.csv")
alcohol_frequency <- read_csv("beh_alcfreq_lhn_trend.csv")

####################################################
##RCIdata## from https://data.gov.au/dataset/ds-dga-6cdf7a25-4f2d-4bae-b3b5-61175e2b3b13/distribution/dist-dga-839fd4c3-6b4a-4658-8671-dd974b5b4bb1/details?q=

options(stringsAsFactors = TRUE)

url <- "http://data.gov.au/storage/f/2013-09-12T23%3A32%3A36.918Z/rci-offencebymonth.csv"
RCIdata <- GET(url = url)
status_code(RCIdata)
str(content(RCIdata))
head(RCIdata$content)
RCIdata <- rawToChar(RCIdata$content)
nchar(RCIdata)

RCIdata <- content(RCIdata, as = "text", encoding = "UTF-8")
df <- fromJSON(RCIdata,flatten = TRUE)
df

#########################################################

################################
## Basic Data Read
################################

## names(suburbdata)

summary(postcode_data)
## summary(suburbdata)
summary(alcohol_consumption)
summary(alcohol_hospitalisations)
summary(alcohol_frequency)

str(postcode_data)
## str(suburbdata)
str(alcohol_consumption)
str(alcohol_hospitalisations)
str(alcohol_frequency)

##################################################
## Check for missing values
##################################################

missmap(postcode_data, main = "Missing values vs observed")
## missmap(suburbdata, main = "Missing values vs observed")
missmap(alcohol_consumption, main = "Missing values vs observed")
missmap(alcohol_hospitalisations, main = "Missing values vs observed")
missmap(alcohol_frequency, main = "Missing values vs observed")

########################################
##Data Cleaning and Prep
########################################

# Clean the "alcohol hospitalisations" data
# Remove the NA's /blank data (from all the comments at the end of the csv file)
alcohol_hospitalisations <- alcohol_hospitalisations %>%
  filter (year !="") %>%
  filter ("State comparison" != "Rest of NSW")%>% # Remove the state comparison aggregate from each LHD's data
  filter ("State comparison" != "Total NSW")

# Remove the columns we don't want from alcohol hospitalisations data, and rename the remaining to be more friendly
names(alcohol_hospitalisations)
alcohol_hospitalisations <- select (alcohol_hospitalisations, -c("State comparison","LL 95% CI","UL 95% CI"))
alcohol_hospitalisations <- rename(alcohol_hospitalisations, hospitalisation_num = Number)
alcohol_hospitalisations <- rename(alcohol_hospitalisations, hospitalisation_rate = "Rate per 100,000 population")
alcohol_hospitalisations <- rename(alcohol_hospitalisations, LHD = "Local Health Districts")

#####################################################################
names(alcohol_consumption)
# Clean the alcohol consumption data        
# Remove NA rows (from comment data at end of .csv files)        
alcohol_consumption <- alcohol_consumption %>%
  filter (year !=(is.na (year)))
#Consumption data is by year... the other two are by financial year... let's forget using consumption for now and use the freq instead
#####################################################################

names(alcohol_frequency)
# Clean the alcohol frequency data
# Remove the NA's /blank data (from all the comments at the end of the csv file)
alcohol_frequency <- alcohol_frequency %>%
  filter (year !="") 
alcohol_frequency <- rename (alcohol_frequency, LHD = "Local Health Districts")
alcohol_frequency <- rename (alcohol_frequency, freq = "Drinking frequency")
alcohol_frequency <- rename (alcohol_frequency, estimate = "Actual estimate (Per cent)")

# remove the "All LHDs" aggregate data from local health districts - we have them all can sum them up if we need an all!
alcohol_frequency <- alcohol_frequency %>%
  filter (LHD !="All LHDs")
  
# remove the colums of data we don't want
alcohol_frequency <- select (alcohol_frequency, -c("Number of Respondents","LL 95% CI","UL 95% CI"))

# Use "Spread" to create a new variable / column for each "Alcohol Frequency" i.e. never, one per day etc. inpreparation for joining it to the hospitalisation data
alcohol_frequency <- alcohol_frequency %>%
  spread(key = freq, value = estimate)

names(alcohol_frequency)
# rename the new columns to be more user friendly for us
alcohol_frequency <- rename(alcohol_frequency, freq_daily =  Daily)
alcohol_frequency <- rename(alcohol_frequency, freq_less_weekly =  "Less than weekly")
alcohol_frequency <- rename(alcohol_frequency, freq_never =  Never)
alcohol_frequency <- rename(alcohol_frequency, freq_weekly = Weekly)

names(alcohol_frequency)
names(alcohol_hospitalisations)

# Years are labelled differently in the two sets - we need to make them the same (i.e. frequency data has "2002-2003"; hospitalisations has "2002-03")
unique (alcohol_frequency$year)
unique (alcohol_hospitalisations$year)
alcohol_hospitalisations <- alcohol_hospitalisations %>%
  mutate(left_year = substr(alcohol_hospitalisations$year,1,5), right_year = substr(alcohol_hospitalisations$year,6,7))

alcohol_hospitalisations <- alcohol_hospitalisations %>%
  unite ("year", left_year, right_year, sep = "20", remove = TRUE)

alcohol_hospitalisations

## Now Join the Frequency and Hospitalisations data - join by LHD, year, sex


alcohol_freq_hosp <- alcohol_hospitalisations %>%
  full_join(alcohol_frequency)

summary(alcohol_freq_hosp)
names(alcohol_freq_hosp)

str(alcohol_freq_hosp)
str(alcohol_hospitalisations)

#subset/filter postcode_data to keep data from Jan-08 to Dec-18
#filter postcode_data to keep data from Jan-08 to Dec-18 and then filter further for liquor offences
names(postcode_data)
sub = select(postcode_data, Postcode, Offence, "Jan-08" : "Dec-18")
sub

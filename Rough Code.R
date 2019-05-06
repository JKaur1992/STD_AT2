rm(list=ls()) #for clearing the environment
setwd("~/GitHub/STD_AT2")

library(tidyverse)
library(readxl)
library(Amelia)
library(dplyr)
library(tidyr)

################################
## Load the data
################################

# alcohol consumption data from http://www.healthstats.nsw.gov.au/Indicator/beh_alc_age/beh_alc_lhn_trend:
alcohol_consumption <- read_csv("beh_alc_lhn_trend.csv")

# alcohol hospitalisations data from http://www.healthstats.nsw.gov.au/Indicator/beh_alcafhos/beh_alcafhos_lhn_trend
alcohol_hospitalisations <- read_csv("beh_alcafhos_lhn_trend.csv")

# alcohol freq data from www.healthstats.nsw.gov.au/Indicator/beh_alcfreq/beh_alcfreq_lhn_trend
alcohol_frequency <- read_csv("beh_alcfreq_lhn_trend.csv")

# alcohol deaths data from http://www.healthstats.nsw.gov.au/Indicator/beh_alcafdth/beh_alcafdth_lhn_trend
alcohol_deaths <-  read_csv("beh_alcafdth_lhn_trend.csv")

# crime data by postcode from https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Datasets-.aspx
postcode_data <- read_csv("7_PostcodeData2018.csv")

## crime data by suburbs from https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Datasets-.aspx
## suburbdata <- read_csv("9_SuburbData2018.csv")

# Unemployment data by LGA from https://docs.jobs.gov.au/documents/unsmoothed-small-area-labour-markets-local-government-area-lga-series-december-quarter
unemployment_LGA <- read_csv("salm_unsmoothed_lga_datafiles_-_december_quarter_2018 (1).csv")

# LGA and Postcode mapping file Cuong created
mapping <- read_csv("Australia_lga_postcode_mappings_2016.csv")

# LGA to LHD mapping file Michael created
LGA_LHD_Map <- read_excel("LGAtoLHD.xlsx") 

###############################################################################################################################################

# crime data by LGA from https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Datasets-.aspx
#RCIdata <- read_excel("8_RCI_offencebymonth.xlsm") ## to download as excel. 
#RCIdata <- read_excel("RCI_offencebymonth.xlsm")
#forgot that this dataset is changing dates into random numbers. 
#either resolve this or exclude this data

####### other ways to load RCI data

## 1. load directly from website
## RCIdata <- read_excel("https://www.bocsar.nsw.gov.au/Documents/Datasets/RCI_offencebymonth.xlsm") # this link isn't working
## RCIdata <- read_csv("http://data.gov.au/storage/f/2013-09-12T23%3A32%3A36.918Z/rci-offencebymonth.csv") ## this is data is only until 2012!!

## 2. loading using API
library(httr)
library(rsdmx)
library(jsonlite)
library(lubridate)
library(ckanr)
## method 1 - doesn't really work
# options(stringsAsFactors = TRUE)
url <- "https://www.bocsar.nsw.gov.au/Documents/Datasets/RCI_offencebymonth.xlsm"
# url <- "https://data.nsw.gov.au/data/api/3/action/datastore_create?resource_id=1d5b2851-52e9-4327-a81b-19149c63f736" - doesn't work
RCIurl <- GET(url = url)
status_code(RCIurl) #got 200 which is good
str(content(RCIurl))
head(RCIurl$content)
RCI <- rawToChar(RCIurl$content) # doesn't work from here
nchar(RCI)
RCIurl <- content(RCIurl, as = "text", encoding = "UTF-8")
df <- fromJSON(RCIurl,flatten = TRUE)
df

## method 2 - this seems to work but can't make anything from the outcome though
# options(stringsAsFactors = TRUE)
RCIurl <- GET (url = "http://www.data.gov.au/api/3/action/group_list")
status_code(RCIurl)
str(content(RCIurl))
head(RCIurl$content)
RCI <- rawToChar(RCIurl$content)
nchar(RCI)
## substr(RCI, 1, 100)
## RCI <- content(RCI, as = "text", encoding = "UTF-8")
df <- fromJSON(RCI,flatten = TRUE) 
df

group_show('communications', as = 'table')$users
tag_list('aviation', as = 'table')
tag_show('Aviation')$packages[[1]][1:3]
organization_list()

## method 3
## RCIurl <- GET (url = "https://data.nsw.gov.au/data/api/3/action/datastore_create?resource_id=1d5b2851-52e9-4327-a81b-19149c63f736") # to create API
## RCIurl <- GET (url = "http://www.data.gov.au/api/3/action/datastore_search?resource_id=1d5b2851-52e9-4327-a81b-19149c63f736&limit=5") # to insert API####

################################################################################################################################################

################################
## Basic Data Read
################################

## names(suburbdata)

summary(postcode_data)
## summary(suburbdata)
summary(RCIdata)
summary(alcohol_consumption)
summary(alcohol_hospitalisations)
summary(alcohol_frequency)
summary(alcohol_deaths)

str(postcode_data)
## str(suburbdata)
## str(RCIdata)
str(alcohol_consumption)
str(alcohol_hospitalisations)
str(alcohol_frequency)
str(alcohol_deaths)

##################################################
## Check for missing values
##################################################

missmap(postcode_data, main = "Missing values vs observed")
## missmap(suburbdata, main = "Missing values vs observed")
missmap(alcohol_consumption, main = "Missing values vs observed")
missmap(alcohol_hospitalisations, main = "Missing values vs observed")
missmap(alcohol_frequency, main = "Missing values vs observed")
missmap(alcohol_deaths, main = "Missing values vs observed")

########################################
##Data Cleaning and Prep
########################################

# Clean unemployment data
unemployment_LGA2 <- unemployment_LGA %>%
  filter(.$'Data item' == "Unsmoothed unemployment rate (%)") %>%
  gather(., key = "QuarterYear", value = "unemploy_Rate", -c('Data item', 'Local Government Area (LGA)', 'LGA Code')) %>%
  select(.,c('Local Government Area (LGA)', 'LGA Code', 'QuarterYear', 'unemploy_Rate')) %>%
  separate(.,"QuarterYear",c("Quarter","Year"),sep ="-")

########NEXT DATASET####################################

# Clean the "alcohol hospitalisations" data
# Remove the NA's /blank data (from all the comments at the end of the csv file)
alcohol_hospitalisations <- alcohol_hospitalisations %>%
  filter (year !="")
alcohol_hospitalisations <- alcohol_hospitalisations %>% 
  filter (alcohol_hospitalisations$`State comparison` != "Rest of NSW") # Remove the state comparison aggregate from each LHD's data
alcohol_hospitalisations <- alcohol_hospitalisations %>%   
  filter (alcohol_hospitalisations$`State comparison` != "Total NSW")

# Remove the columns we don't want from alcohol hospitalisations data, and rename the remaining to be more friendly
names(alcohol_hospitalisations)
alcohol_hospitalisations <- select (alcohol_hospitalisations, -c("State comparison","LL 95% CI","UL 95% CI"))
alcohol_hospitalisations <- rename(alcohol_hospitalisations, hospitalisation_num = Number)
alcohol_hospitalisations <- rename(alcohol_hospitalisations, hospitalisation_rate = "Rate per 100,000 population")
alcohol_hospitalisations <- rename(alcohol_hospitalisations, LHD = "Local Health Districts")

########NEXT DATASET####################################

# Clean the "alcohol deaths" data
# Remove the NA's /blank data (from all the comments at the end of the csv file)
alcohol_deaths <- alcohol_deaths %>%
  filter (year !="") 
alcohol_deaths <- alcohol_deaths %>%
  filter (alcohol_deaths$`State comparison` != "Rest of NSW")  # Remove the state comparison aggregate from each LHD's data
alcohol_deaths <- alcohol_deaths %>%  
  filter (alcohol_deaths$`State comparison` != "Total NSW")

# Remove the columns we don't want from alcohol deaths data, and rename the remaining to be more friendly
names(alcohol_deaths)
alcohol_deaths <- select (alcohol_deaths, -c("State comparison","LL 95% CI","UL 95% CI"))
alcohol_deaths <- rename(alcohol_deaths, death_num = "Average number per year")
alcohol_deaths <- rename(alcohol_deaths, death_rate = "Rate per 100,000 population")
alcohol_deaths <- rename(alcohol_deaths, LHD = "Local Health Districts")

#####################################################################
names(alcohol_consumption)
# Clean the alcohol consumption data        
# Remove NA rows (from comment data at end of .csv files)        
alcohol_consumption <- alcohol_consumption %>%
  filter (year !=(is.na (year)))
#Consumption data is by year... the other two are by financial year... let's forget using consumption for now and use the freq instead
#####################################################################

########NEXT DATASET####################################

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
names(alcohol_deaths)

# Years are labelled differently in the two sets - we need to make them the same (i.e. frequency data has "2002-2003"; hospitalisations has "2002-03")
unique (alcohol_frequency$year)
unique (alcohol_hospitalisations$year)
unique (alcohol_deaths$year)
alcohol_hospitalisations <- alcohol_hospitalisations %>%
  mutate(left_year = substr(alcohol_hospitalisations$year,1,5), right_year = substr(alcohol_hospitalisations$year,6,7))

alcohol_hospitalisations <- alcohol_hospitalisations %>%
  unite ("year", left_year, right_year, sep = "20", remove = TRUE)

alcohol_hospitalisations

########NEXT DATASET####################################

# filter mapping data for NSW
mapping <- filter(mapping, State == 'New South Wales')

# I also need to split months and years into separate columns so it aligns - SHOULD I and HOW????

#filter postcode_data to keep data from Jan-08 to Dec-18 and then filter further for liquor offences
names(postcode_data)
subset = select(postcode_data, Postcode, Offence, "Jan-08" : "Dec-18")
alcohol_offences <- filter(subset, Offence == 'Liquor offences')
alcohol_offences

# Use "gather" to create a new variable / column for year - this will bring hospital and violence data into same format time wise
alcohol_offences <- alcohol_offences %>% 
  gather(key = year, value = violence_count, "Jan-08" : "Dec-18")
alcohol_offences

########NEXT DATASET####################################

#filter suburbdata to keep data from Jan-08 to Dec-18 and then filter further for liquor offences
names(suburbdata)
suburbdata <- rename(suburbdata, offence =  'Offence category')
unique (suburbdata$offence)
suburb_subset = select(suburbdata, Suburb, offence, "Jan 2008" : "Dec 2018")
alcoholoffences <- filter(suburb_subset, offence == 'Liquor offences')
alcoholoffences

# Use "gather" to create a new variable / column for year - this will bring hospital and violence data into same format time wise
alcoholoffences <- alcoholoffences %>% 
  gather(key = year, value = violence_count, "Jan 2008" : "Dec 2018")
alcoholoffences


########NEXT DATASET#########################################################
# Bring RCI data into same format as offences data
RCIdata <- rename(RCIdata, offence =  'Offence category') # rename the variable so it's more friendly.
RCIdata = select(RCIdata, LGA, offence, "Jan-08" : "Dec-18")
RCIdata <- filter(RCIdata, offence == 'Liquor offences')
RCIdata

RCIdata <- RCIdata %>% 
  gather(key = year, value = violence_count, "Jan-08" : "Dec-18")
RCIdata
##############################################################################

##################################################
## Merging Datasets
##################################################

## Now Join the Frequency and Hospitalisations and Deaths data - join by LHD, year, sex
alcohol_freq_hosp <- alcohol_hospitalisations %>%
  full_join(alcohol_frequency)

alcohol_freq_hosp_death <- alcohol_freq_hosp %>%
  full_join(alcohol_deaths)

summary(alcohol_freq_hosp_death)
names(alcohol_freq_hosp_death)

str(alcohol_freq_hosp_death)
alcohol_freq_hosp_death  <-   filter (alcohol_freq_hosp_death, !( year =="2001-2002" | year == "2002-2003"| year == "2003-2004"| year =="2004-2005" | year =="2005-2006" | year =="2006-2007" ))
# alcohol_freq_hosp_death  <-   filter (alcohol_freq_hosp_death, LHD == 'Sydney LHD')
alcohol_freq_hosp_death

########NEXT DATASET####################################

#Merge Postcode/LGA and LHD/LGA files
mapping <- rename(mapping, LGA =  'LGA region')
mergeCol.1 <- c("LGA")
basic_merge <- merge(LGA_LHD_Map, mapping, by = mergeCol.1)

########NEXT DATASET####################################

#Merge this merged file into Postcode violence data
mergeCol.2 <- c("Postcode")
offence_data <- merge(alcohol_offences, basic_merge, by = mergeCol.2)

########NEXT DATASET####################################

#Merge this merged file into hospital data
mergeCol.3 <- c("LHD")
hospital_data <- merge(alcohol_freq_hosp_death, basic_merge, by = mergeCol.3)

########NEXT DATASET####################################

#Merge this merged file into Suburb violence data
alcoholoffences <- rename(alcoholoffences, LGA =  'Suburb')
mergeCol.4 <- c("LGA")
offencedata <- merge(alcoholoffences, basic_merge, by = mergeCol.4)

########NEXT DATASET####################################

# Merge hospital data and offence data
mergeCols <- c("year","LGA", "LHD", "Postcode", "State") #year variable needs to be in the same format for it to work. atm, none of the merge codes below work
mergeCols <- c("LGA", "LHD", "Postcode", "State") #year variable needs to be in the same format for it to work

# just adiding some sample merging commands for now - 
# 
inner <- merge(offence_data, hospital_data, by = mergeCols)
# inner <- merge(offence_data, hospital_data, by = year)
# left  <- merge(offence_data, hospital_data, by = mergeCols, all.x = TRUE) #doesn't work for either codes above
# right <- merge(offence_data, hospital_data, by = mergeCols, all.y = TRUE) #doesn't work for either codes above
# 
cross <- merge(offence_data, hospital_data, by = NULL)
# natural <- merge(offence_data, hospital_data) #doesn't work

# Export to .csv Files 
write_csv(offence_data, path = "offence_data.csv")

###########################################################
##EDA
###########################################################


rm(list=ls())
setwd("C:/Users/mjg07/OneDrive/Documents/MDSI/36103 Statistical Thinking for Data Science/Assignment 2/STD_AT2")

library("tidyverse")

# alcohol hospitalisations data from http://www.healthstats.nsw.gov.au/Indicator/beh_alcafhos/beh_alcafhos_lhn_trend
alcohol_hosp_LGA <- read_csv("beh_alcafhos_lga_trend.csv")



# alcohol deaths data from http://www.healthstats.nsw.gov.au/Indicator/beh_alcafdth/beh_alcafdth_lhn_trend
alcohol_deaths_LGA <-  read_csv("beh_alcafdth_lga_trend.csv")



names(alcohol_hosp_LGA)

rate_col_numb <- grep("_rate",names(alcohol_hosp_LGA))


rate_only <- alcohol_hosp_LGA[,rate_col_numb]  
headers <- alcohol_hosp_LGA[,c(1:2)]
new_hosp <- cbind(headers,rate_only)


# There is no column data for Oberon LGA, so filter it out
# Take out North Sydney LGA as the grep function later gets both Sydney and North Sydney
 

new_hosp <-  new_hosp[,-74] 
new_hosp <-  new_hosp %>% filter (new_hosp$`Local Government Areas` != "North Sydney LGA")
new_hosp <-  new_hosp %>% filter (new_hosp$`Local Government Areas` != "Oberon")
names(new_hosp)



# loop to filter to each LGA, select it's data column, rename the column and bind all the rows together
hosp_clean = data.frame()
LGAs = unique(new_hosp$`Local Government Areas`)

for (LGA in LGAs) {
  
  new_hosp2 = new_hosp %>%filter(new_hosp$`Local Government Areas`==LGA)
  LGA_col = grep(LGA,names(new_hosp2)) # Find the column num of the LGA
  header = new_hosp2[,c(1:2)]
  One_LGA = data.frame (hosp_rate = new_hosp[,LGA_col])
  This_LGA = cbind(header,One_LGA)
  hosp_clean = rbind(hosp_clean,This_LGA)
}


# Filter out all the na rows
hosp_clean <- hosp_clean[complete.cases(hosp_clean),]


# Fix up the year to standard format
hosp_clean <- hosp_clean %>%
  mutate(right_year = substr(hosp_clean$year,6,7),
         year = paste0("20",right_year))
hosp_clean <- hosp_clean[,-4]




# Do the same for Deaths


names(alcohol_deaths_LGA)

rate_col_numb2 <- grep("_rate",names(alcohol_deaths_LGA))


rate_only <- alcohol_deaths_LGA[,rate_col_numb2]  
headers <- alcohol_deaths_LGA[,c(1:2)]
new_death <- cbind(headers,rate_only)
names(new_death)


grep("Sydney", names(new_death))
new_death <-  new_death[,-74] 
new_death <-  new_death %>% filter (new_death$`Local Government Areas` != "North Sydney LGA")
# new_hosp <-  new_hosp %>% filter (new_hosp$`Local Government Areas` != "Oberon")
names(new_death)




# programmatically apply the modelling process to all industries and locations
death_clean = data.frame()
LGAs = unique(new_death$`Local Government Areas`)

for (LGA in LGAs) {
  
  new_death2 = new_death %>%filter(new_death$`Local Government Areas`==LGA)
  LGA_col = grep(LGA,names(new_death2)) # Find the column num of the LGA
  header = new_death2[,c(1:2)]
  One_LGA = data.frame (death_rate = new_death2[,LGA_col])
  This_LGA = cbind(header,One_LGA)
  death_clean = rbind(death_clean,This_LGA)
}


death_clean <- death_clean[complete.cases(death_clean),]

death_clean <- death_clean %>%
  mutate(year= substring(death_clean$year,9,12))


alcohol_hsp_dth <- hosp_clean %>%
  left_join(death_clean)



alcohol_hsp_dth  <- rename(alcohol_hsp_dth, LGA =  "Local Government Areas")
names(alcohol_hsp_dth)

length <- length(alcohol_hsp_dth$LGA)

#Remove "LGA" tag at end of LGA Names
(alcohol_hsp_dth  <- alcohol_hsp_dth %>%
  rowwise()%>%
  mutate (LGA = substr(LGA,0,nchar(LGA)-4)))


setwd("C:/Users/mjg07/OneDrive/Documents/MDSI/36103 Statistical Thinking for Data Science/Assignment 2/STD_AT2")

alc_freq <- read_csv("alcohol_freq_hosp_death.csv")

alc_freq <- alc_freq %>%
  filter(Sex == "Persons") %>%
  mutate(year = substr(year,6,9))

alc_freq <-alc_freq[,c(1,5:9)]

setwd("./CLEAN DATA")
LGA_LHD_Map <- readxl::read_excel("LGAtoLHD.xlsx")
head(LGA_LHD_Map)

alc_freq_LGA <- LGA_LHD_Map %>%
  left_join(alc_freq)

alc_freq_LGA <- filter(alc_freq_LGA, LGA != "Albury")
alc_freq_LGA <- alc_freq_LGA[,-2]


library(readr)


getwd()

write.csv(alcohol_hsp_dth,"alcohol_hosp_death.csv")
write.csv(alc_freq_LGA,"alcohol_freq_LGA.csv")

library("tidyverse")

# Read in LGA and LHD data from other sources
lga_map <- read_csv("Australia_lga_postcode_mappings_2016.csv")
alcohol_hospitalisations <- read_csv("beh_alcafhos_lhn_trend.csv")
alcohol_hospitalisations <- alcohol_hospitalisations %>%
  filter (year !="")

lga_map <- filter(lga_map, State == "New South Wales")

# Get Unique Instances of LGAs and LHDs
LGAs <- unique(lga_map$`LGA region`)
LHDs <- unique(alcohol_hospitalisations$`Local Health Districts`)
library("data.table")
all_LGA <- data.table(LGA = LGAs)
all_LHDs <- data.table(LHD = LHDs)

library(readr)

# Export to .csv Files 
write_csv(all_LGA,path = "all_LGAs.csv")
write_csv(all_LHDs,path = "all_LHDs.csv")


# Now manually map the LGA to the LHD they belong to using info found on the NSW got health site https://www.health.nsw.gov.au/lhd/Pages/nbmlhd.aspx; particularly the pdf map at https://www.health.nsw.gov.au/lhd/Documents/lhd-wall-map.pdf.
# For future - find a way to do this by scraping the data somehow!!

# Read back in the csv file that was created manually
library(readxl)

LGA_LHD_Map <- read_excel("LGAtoLHD.xlsx") 

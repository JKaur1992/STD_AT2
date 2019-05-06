rm(list=ls())

library(tidyverse)
library(readxl)
library(Amelia)
library(dplyr)
library(tidyr)

###########################################################
##EDA
###########################################################
offence_data <- read_csv("offence_data.csv")
population <- read_csv("Population_Clean.csv")


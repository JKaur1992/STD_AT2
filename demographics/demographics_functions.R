# return an LGA demographics data from year 2011 to 2017 in a spread format
# filter by ERP, CAPGAINS, GIFTS, HELPS, INCOME, INSOLVE, PENSION as only them have almost complete data
get_LGA_demographics <- function(LGA_name){
  library(tidyverse)
  demographics_data <- read_csv(paste('STD_AT2/demographics/',LGA_name,'.csv', sep=''))
  demographics_data_spread <- (demographics_data %>% spread(Time, Value))
  return(demographics_data_spread %>% filter(str_detect(MEASURE, '^ERP') | 
                                               str_detect(MEASURE, '^CAPGAINS') |
                                               str_detect(MEASURE, '^GIFTS') |
                                               str_detect(MEASURE, '^HELPS') |
                                               str_detect(MEASURE, '^INCOME') |
                                               str_detect(MEASURE, '^INSOLVE') |
                                               str_detect(MEASURE, '^PENSION')))
}

#Sydney_demographics <- get_LGA_demographics('Sydney')

#View(Sydney_demographics)

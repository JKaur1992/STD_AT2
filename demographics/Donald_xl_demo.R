library(tidyverse)
library(dplyr)
library(stringr)
library(tau)
library(tm)
library(ggplot2)


Population <- read_excel("demographics/Population.xls")
PopulationXl <- Population


#Fixing Column names 
colnames(PopulationXl)[colnames(PopulationXl)=="LABEL"] <- "LGA"
colnames(PopulationXl)[colnames(PopulationXl)=="YEAR"] <- "Year"

names(PopulationXl) <- gsub(" ", "_", names(PopulationXl)) #Improve columns names aliminating spaces 

#Delete extra words in LGA names 
#PopulationXl$LGA<- gsub("\\s*\\([^\\)]+\\)","",as.character(PopulationXl$LGA))

# Add prefixes in coulmns 
colnames(PopulationXl)[4:12] <- paste("Person_Population_Rate", colnames(PopulationXl[,c(4:12)]), sep = "_") # Add a prefix to Person Population Rate 

colnames(PopulationXl)[13:31] <- paste("Male_Population_Number", colnames(PopulationXl[,c(13:31)]), sep = "_") # Add a prefix to Male Population Number 

colnames(PopulationXl)[32:50] <- paste("Female_Population_Number", colnames(PopulationXl[,c(32:50)]), sep = "_") # Add a prefix to Female Population Number 

colnames(PopulationXl)[51:69] <- paste("Person_Population_Number", colnames(PopulationXl[,c(51:69)]), sep = "_") # Add a prefix to Person Population Number 

# Looks like we have duplicated areas, but we have Councils and Areas
PopulationXl<- PopulationXl %>%
  separate(LGA, c("LGA", "Council_vs_Area"), "[()]")  #Separate Councils and Areas

PopulationXl$LGA <- str_trim(PopulationXl$LGA, side = c("both", "left", "right")) # Delete extra space in string


# Subsetting for EDA and Mrging
population_subset <- PopulationXl %>%
  filter(Year !=2011) %>%
  #filter(Council_vs_Area=="C") %>%
  select(LGA, Council_vs_Area ,Year,`Person_Population_Number_15-19_years..54`,`Person_Population_Number_20-24_years..55`, `Person_Population_Number_25-29_years..56`, `Person_Population_Number_30-34_years..57`, `Person_Population_Number_35-39_years..58`, `Person_Population_Number_40-44_years..59`, `Person_Population_Number_45-49_years..60`, `Person_Population_Number_50-54_years..61`,`Person_Population_Number_55-59_years..62` ,Person_Population_Number_Total..69 ,Population_density)


######## We need to transfor the str od the variables that are going to be used #######
population_subset$`Person_Population_Number_15-19_years..54` <-as.numeric(as.character(population_subset$`Person_Population_Number_15-19_years..54`))

population_subset$`Person_Population_Number_20-24_years..55`<- as.numeric(as.character(population_subset$`Person_Population_Number_20-24_years..55`))

population_subset$`Person_Population_Number_25-29_years..56`<- as.numeric(as.character(population_subset$`Person_Population_Number_25-29_years..56`))

population_subset$`Person_Population_Number_30-34_years..57`<- as.numeric(as.character(population_subset$`Person_Population_Number_30-34_years..57`))

population_subset$`Person_Population_Number_35-39_years..58`<- as.numeric(as.character(population_subset$`Person_Population_Number_35-39_years..58`))

population_subset$`Person_Population_Number_40-44_years..59` <- as.numeric(as.character(population_subset$`Person_Population_Number_40-44_years..59`))

population_subset$`Person_Population_Number_45-49_years..60`<- as.numeric(as.character(population_subset$`Person_Population_Number_45-49_years..60`))

population_subset$`Person_Population_Number_50-54_years..61`<-as.numeric(as.character(population_subset$`Person_Population_Number_50-54_years..61`))

population_subset$`Person_Population_Number_55-59_years..62` <- as.numeric(as.character(population_subset$`Person_Population_Number_55-59_years..62`))

population_subset$Person_Population_Number_Total..69 <- as.numeric(as.character(population_subset$Person_Population_Number_Total..69))

population_subset$Population_density <- as.numeric(as.character(population_subset$Population_density)) 


write.csv(PopulationXl,"Population_Clean_Xl.csv")




# Import Crime data set
offence_data_EDA <- read_csv("CLEAN DATA/offence_data_EDA.csv")
Crime <- offence_data_EDA
Crime<- Crime[,-c(4,5)]

# Merge
Pop_Crime_Merge_DT <- full_join(Crime, population_subset, by=c("Year","LGA"))

# Subset for representative areas
Pop_Crime_Merge_DT <- filter(Pop_Crime_Merge_DT, ( LGA == 'Sydney' | LGA == 'Parramatta'| LGA == 'North Sydney' | LGA == 'Inner West')) #LGA == 'Randwick' | LGA == 'Canterbury-Bankstown'  | LGA == 'North Sydney' |  LGA == 'Hornsby' | LGA == 'Strathfield' | LGA == 'Burwood' | LGA == 'Mosman'))   | LGA == 'Bayside'))| LGA == 'Central Darling'  ##Bayside has (C) and (A) creating duplicates 


#Create new variables 
#Adult drinkimg age band by LGA
Pop_Crime_Merge_DT <- Pop_Crime_Merge_DT %>%
  group_by(LGA, Year) %>%
  mutate(PopDrinkingAge = sum(`Person_Population_Number_15-19_years..54`,`Person_Population_Number_20-24_years..55`,`Person_Population_Number_25-29_years..56`, `Person_Population_Number_30-34_years..57`, `Person_Population_Number_35-39_years..58`, `Person_Population_Number_40-44_years..59`, `Person_Population_Number_45-49_years..60`, `Person_Population_Number_50-54_years..61`, `Person_Population_Number_55-59_years..62`))


# Add Pre and post lockout indicator
Pop_Crime_Merge_DT <- Pop_Crime_Merge_DT %>% 
  mutate(lockout = ifelse(Year<2014,"Pre","Post"))

#Violanece Popuation rate
Pop_Crime_Merge_DT <- Pop_Crime_Merge_DT %>%
  mutate(CrimePerPersonTot = violence_count/ Person_Population_Number_Total..69)

#Violenece per ADULT in the drinking Age
Pop_Crime_Merge_DT <- Pop_Crime_Merge_DT %>%
  mutate(CrimePerPerson_15_59 = violence_count/ PopDrinkingAge)



#missmap(Pop_Crime_Merge_DT, main = "Missing values vs observed") #checking for missing values.
#above code still has some missing values so to have data without missing values, try Inner join
#offence_data_EDA <- inner_join(offence_data_ag, population_subset, by = c("Year", "LGA"))

# No cental darling Pop density, WE HAVE LGA AREAS AND COUNCIL 

# Plots

#Population Density in time
ggplot(data = Pop_Crime_Merge_DT) + 
  geom_line(mapping = aes(x=Year ,y = Population_density, color=LGA))+
  xlab("Year") + ylab("Pop Density") +
  labs(title="Population Density")

ggplot(data = Pop_Crime_Merge_DT) + 
  geom_line(mapping = aes(x = Population_density, y = violence_count, color=LGA), show.legend = FALSE)+
  xlab("Population Density") + ylab("Violence Count") +
  facet_wrap(.~ LGA, scales = "free")+
  ggtitle("Population Density Vs Violence")


#Total crimes divided by total persons. All Ages 
ggplot(data = Pop_Crime_Merge_DT) + 
  geom_line(mapping = aes(x = Year, y = CrimePerPersonTot , color=LGA), show.legend = FALSE)+
  xlab("Year") + ylab("Crime Rate")+
  facet_wrap(.~ LGA, scales = "free") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "black", size=1.5)+
  labs(title="Crime Rate", subtitle = "Total Population")


#Total crimes divided by person between 15 and 59 Years 
ggplot(data = Pop_Crime_Merge_DT) + 
  geom_line(mapping = aes(x = Year, y = CrimePerPerson_15_59 , color=LGA), show.legend = FALSE)+
  xlab("Year") + ylab("Crime Rate") +
  facet_wrap(.~ LGA, scales = "free")+
  geom_vline(xintercept = (2014), linetype="dotted", color = "black", size=1.5) +
  labs(title="Crime Rate", subtitle = "Population between 15-59 Years") +
  theme_bw()

##LGA with same scale ;  Total crimes divided by person between 15 and 59 Years 
ggplot(data = Pop_Crime_Merge_DT) + 
  geom_line(mapping = aes(x = Year, y = CrimePerPerson_15_59 , color=LGA))+
  xlab("Year") + ylab("Crime Rate") +
  geom_vline(xintercept = (2014), linetype="dotted", color = "black", size=1.5) +
  labs(title="Crime Rate", subtitle = "Population between 15-59 Years") +
  theme_bw()




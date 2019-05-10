PopulationXl <- read_excel("demographics/Population.xls")


#Fixing Column names 
colnames(PopulationXl)[colnames(PopulationXl)=="LABEL"] <- "LGA"
colnames(PopulationXl)[colnames(PopulationXl)=="YEAR"] <- "Year"

names(PopulationXl) <- gsub(" ", "_", names(PopulationXl))


#Delete extra words in LGA names 
PopulationXl$LGA<- gsub("\\s*\\([^\\)]+\\)","",as.character(PopulationXl$LGA))





# Add prefixes in coulmns 
colnames(PopulationXl)[4:12] <- paste("Person_Population_Rate", colnames(PopulationXl[,c(4:12)]), sep = "_") # Add a prefix to Person Population Rate 

colnames(PopulationXl)[13:31] <- paste("Male_Population_Number", colnames(PopulationXl[,c(13:31)]), sep = "_") # Add a prefix to Male Population Number 

colnames(PopulationXl)[32:50] <- paste("Female_Population_Number", colnames(PopulationXl[,c(32:50)]), sep = "_") # Add a prefix to Female Population Number 

colnames(PopulationXl)[51:69] <- paste("Person_Population_Number", colnames(PopulationXl[,c(51:69)]), sep = "_") # Add a prefix to Person Population Number 


# Subsetting for EDA and Mrging
population_subset <- PopulationXl %>%
  filter(Year !=2011) %>%
  select(LGA, Year, Person_Population_Number_Total..69, Population_density)

population_subset$Person_Population_Number_Total..69 <- as.numeric(as.character(population_subset$Person_Population_Number_Total..69))

population_subset$Population_density <- as.numeric(as.character(population_subset$Population_density)) 

population_subset <-  round(population_subset, digits = 2)

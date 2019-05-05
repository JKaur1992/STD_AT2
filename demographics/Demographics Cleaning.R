library(stringr)

# DEMOGRAPHICS #
## Sydney Demographics ####
Sydney <- read_csv("demographics/Sydney.csv")
Sydney <- add_column(Sydney, Territory="Sydney", .before =1) #Identify territory for further merging (We should change to LGA or LHD)

colnames(Sydney)[colnames(Sydney)=="Time"]<- "Year" #Change column neame to year 



#Create a new data frame for Estimated Resident Population (ERP)
SydneyERP<- Sydney[1:396,] #Select the rows that contain Estimated Resident Population data

SydneyERP$Gender <- word(SydneyERP$Description, 1) #Column for gender

SydneyERP$Parameter <- word(SydneyERP$Description, 5) #we have percentage and total (should we delete percentages?)

SydneyERP$Age_Range <- word(SydneyERP$Description, 3) #Column for Age Range

SydneyERP$Age_Range <- ifelse (SydneyERP$Age_Range ==85,"85 or more",SydneyERP$Age_Range) #add rannge to 20 or more




#Create a new data frame for Births and Deaths (BD)
SydneyBD<- Sydney[421:442,] #Select the rows that contain Births and deaths

SydneyBD<- SydneyBD %>%
  separate(Description, c("Description", "Description_2"), "-")

SydneyBD$BirthOrDeath <- ifelse(SydneyBD$Description_2[grep("Births", SydneyBD$Description_2)]== SydneyBD$Description_2, 1,0) # Create dummy varibale for Births (1) and Deaths (0)




# Data frame for Median Age (MA)
SydneyMA<- Sydney[403:420,] #Select rows containing Median Age

SydneyMA <- SydneyMA %>%
  separate(Description, c("Description", "Description_2"), "-")  #Split Description

SydneyMA <- SydneyMA %>%
  separate(Description_2, c("x", "Gender"), " ")  # Create a colmn for gender 

colnames(SydneyMA)[colnames(SydneyMA)=="Value"]<- "Median_Age" #Values are median age (change name)

SydneyMA$x<-NULL #Delete extra column



# Industry Data frame (In)
SydneyIn<-Sydney[489:513,]

SydneyIn$Description <- substr(SydneyIn$Description,1,nchar(SydneyIn$Description) -5) #Delete (no.)

SydneyIn <- SydneyIn %>%
  separate(Description, c("Description", "Number_Employees"), ":")  #Split Description

SydneyIn$Number_Employees <- word(SydneyIn$Number_Employees, 2) # Delete "employees" 

SydneyIn$Number_Employees[is.na(SydneyIn$Number_Employees)]<- "0"  #Change NA to 0

SydneyIn$Number_Employees <- ifelse (SydneyIn$Number_Employees ==20,"20 or more",SydneyIn$Number_Employees) #add rannge to 20 or more

colnames(SydneyIn)[colnames(SydneyIn)=="Value"]<- "Number_Businesses" #Values are number of businesses



# Industry entries and exits (IEE)
SydneyIEE<-Sydney[514:553,]

SydneyIEE <- SydneyIEE %>%
  separate(Description, c("Description", "Number_Employees"), ":")  #Split Description

SydneyIEE$Number_Employees <- word(SydneyIEE$Number_Employees, 2) # Delete "employees"

SydneyIEE$Number_Employees[is.na(SydneyIEE$Number_Employees)]<- "0"  #Change NA to 0

SydneyIEE$Number_Employees <- ifelse (SydneyIEE$Number_Employees ==20,"20 or more",SydneyIEE$Number_Employees) #add rannge to 20 or more

colnames(SydneyIEE)[colnames(SydneyIEE)=="Value"]<- "Number_Businesses" #Values are number of businesses


SydneyIEE$EntryOrExit <- ifelse(SydneyIEE$Description[grep("entries", SydneyIEE$Description)]== SydneyIEE$Description, 1,0) # Create dummy varibale for entries (1) and exits (0)



# Businesses by Industry (BI)
SydneyBI<-Sydney[554:658,]

colnames(SydneyBI)[colnames(SydneyBI)=="Value"]<- "Number_Businesses" #Values are number of businesses

SydneyBI$Description <- substr(SydneyBI$Description,1,nchar(SydneyBI$Description) -5) #Delete (no.)





#Purpose:
#Downloading and FEMA HMA data

#
getwd()

#libraries####
library(data.table)
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
library(nnet)


#Reading FEMA Properties data ####
#note: extra steps were needed because strings like Rincon have an accent that was not reading in.
#properties <- GET("https://www.fema.gov/api/open/v4/HazardMitigationAssistanceMitigatedProperties.json") #note: V4 is latest release as of authorship
#raw_content <- rawToChar(properties$content)

# Step 1a: Inspect the raw content (optional, for debugging)
#print(substr(raw_content, 1, 1000))  # Print the first 1000 characters

# Step 1b: 1Clean the content if necessary
#cleaned_content <- iconv(raw_content, "latin1", "ASCII", sub="")

# Step 1c: Parse the JSON content
#properties <- fromJSON(cleaned_content)

#properties <- as.data.frame(properties)

#reading from archive ####
properties <- read.csv('C:/Users/lgero/Box/Research/FEMA_project/Data/Archive/HMA_Properties_07_03_24.csv')

#Cleaning Properties Data####

##Renaming columns####
head(properties)
colnames(properties) <- gsub(pattern="HazardMitigationAssistanceMitigatedProperties.",replacement="",x=colnames(properties))


##Exploring data ####
range(properties$programFy) #1989-2023
table(properties$propertyAction) 
table(properties$propertyAction, properties$programArea)
table(properties$typeOfResidency)
sum(properties$numberOfProperties) #214748
colSums(is.na(properties))

#Expanding properties dataset: 1 property per row####
properties_long <- properties[rep(row.names(properties), properties$numberOfProperties),] #214748 obs

#Subsetting to those with a DN####
properties_DN <- subset(properties_long, disasterNumber!="NA") #180034 obs
colSums(is.na(properties_DN))

## BREAK ################

#Reading in Projects data####
#projects <- GET("https://www.fema.gov/api/open/v4/HazardMitigationAssistanceProjects.json") #note: V4 is latest release as of authorship
#raw_content <- rawToChar(projects$content)

# Step 1a: Inspect the raw content (optional, for debugging)
#print(substr(raw_content, 1, 1000))  # Print the first 1000 characters

# Step 1b: 1Clean the content if necessary
#cleaned_content <- iconv(raw_content, "latin1", "ASCII", sub="")

# Step 1c: Parse the JSON content
#projects <- fromJSON(cleaned_content)

#projects <- as.data.frame(projects)

#Cleaning projects Data####
#colnames(projects) <- gsub(pattern="HazardMitigationAssistanceProjects.",replacement="",x=colnames(projects))

#adding prefix using the paste function in R to distinguish columns that came from the project file, except join field
#original_cols <- colnames(projects)
#print(original_cols)
#colnames(projects)<- paste("p2", original_cols, sep="")
#names(projects)[names(projects)=="p2projectIdentifier"]<-"projectIdentifier" #this is the join field



#reading from archive ####
projects <- read.csv('C:/Users/lgero/Box/Research/FEMA_project/Data/Archive/HMA_Projects_07_03_24.csv')


## BREAK ################

#Joining prop and proj data ####
myHMA <- left_join(properties_DN, projects, by="projectIdentifier", copy=F)
colSums(is.na(myHMA))

#Manage missing data on propertyActions (our Dependent variable of interest)####
colSums(is.na(myHMA))

#examine NAs on Property Action 
table(is.na(myHMA$propertyAction)) #13,931 missing

#select sample with missing data on property action, and examining if data is available on other columns 
propertyAction_NA <- myHMA[is.na(myHMA$propertyAction),]
colSums(is.na(propertyAction_NA ))

#examine the project types associated with the columns with missing property action data
unique_project_types <- unique(propertyAction_NA$p2projectType)
ProjectType_propertyAction_NA<- as.data.frame(table(unique_project_types))

# Create a new dataframe with unique p2projectType entries
names(myHMA)
project_types <- myHMA %>% 
  distinct(p2projectType) %>% 
  mutate(my_Elev = 0,
         my_Acqui = 0,
         my_Studies = 0,
         my_Relocation = 0,
         my_StormwaterManagement = 0,
         my_SafeRoom = 0,
         my_Floodproofing = 0,
         my_Other = 0,
         my_WindRetrofit = 0,
         my_Sanitary = 0,
         my_Wildfire = 0,
         my_Seismic = 0,
         my_FloodControl = 0)

# Define keywords and corresponding columns
keywords <- list(
  "Acquisition" = "my_Acqui",
  "Elevation" = "my_Elev",
  "Floodproofed" = "my_Floodproofed",
  "Mitigation Reconstruction" = "my_MitigationReconstruction",
  "Other" = "my_Other",
  "Safe Room" = "my_SafeRoom",
  "Seismic Retrofit" = "my_Seismic",
  "Wildfire Retrofit" = "my_Wildfire",
  "Wind Retrofit" = "my_WindRetrofit"
)

# Update the flags based on keywords
for (keyword in names(keywords)) {
  project_types[[keywords[[keyword]]]] <- str_detect(project_types$p2projectType, keyword) %>% as.integer()
}

# Sum the flags to check for multiple matches
project_types <- project_types %>% 
  mutate(Total = rowSums(select(., starts_with("my_"))))

# Filter based on the total flags
see_0s <- project_types %>% filter(Total == 0)
see_1s <- project_types %>% filter(Total == 1)
see_2s <- project_types %>% filter(Total == 2)

# Assign propertyAction based on the flags
project_types <- project_types %>%
  rowwise() %>%
  mutate(my_propertyAction = case_when(
    Total == 1 & my_Acqui == 1 ~ "Acquisition",
    Total == 1 & my_Elev == 1 ~ "Elevation",
    Total == 1 & my_Floodproofed == 1 ~ "Floodproofed",
    Total == 1 & my_MitigationReconstruction == 1 ~ "Mitigation Reconstruction",
    Total == 1 & my_Other == 1 ~ "Other",
    Total == 1 & my_SafeRoom == 1 ~ "Safe Room",
    Total == 1 & my_Seismic == 1 ~ "Seismic Retrofit",
    Total == 1 & my_Wildfire == 1 ~ "Wildfire Retrofit",
    Total == 1 & my_WindRetrofit == 1 ~ "Wind Retrofit",
    TRUE ~ "Mix"
  ))

# Merge the updated project_types with the original data
myHMA <- myHMA %>%
  left_join(project_types %>% select(p2projectType, my_propertyAction), by = "p2projectType")
names(myHMA)

# Update the propertyAction column with the assigned values
myHMA$propertyAction2 <- myHMA$propertyAction
myHMA$propertyAction2[is.na(myHMA$propertyAction)] <- myHMA$my_propertyAction[is.na(myHMA$propertyAction)]

#Simplifying propertyActions ####
myHMA$propertyAction3 <- myHMA$propertyAction2 

# Create a named vector for mapping the replacements
replacements <- c(
  "Acquisition of Vacant Land" = "Acquisition_VacantLand",
  "Acquisition/Demolition" = "Acquisition",
  "Acquisition/Relocation" = "Acquisition",
  "Mix" = "Other",
  "Other (Specify in Comments)" = "Other",
  "Retro (1/15/2008 - DSG)" = "Other",
  "Safe Room" = "Safe Room/Wind Shelter",
  "Wind Retrofit Basic (A-P804)" = "Wind Retrofit",
  "Wind Retrofit Basic (B-P804)" = "Wind Retrofit",
  "Wind Retrofit Estimated(ES-P804)" = "Wind Retrofit",
  "Wind Retrofit Intermediate(I-P804)" = "Wind Retrofit",
  "Wind Retrofit Advanced(A-P804)" = "Wind Retrofit"
)

# Apply the replacements
myHMA$propertyAction3 <- ifelse(myHMA$propertyAction2 %in% names(replacements), replacements[myHMA$propertyAction2], myHMA$propertyAction2)

# comparing and verifying new data against original
table(myHMA$propertyAction)
table(myHMA$propertyAction3)

#subset to Single Family ####
myHMA_SF <- subset(myHMA, structureType=="Single Family")

#subset to Owner Occ- Principal Residence ####
myHMA_SFOO <- subset(myHMA_SF, typeOfResidency=="Owner Occupied - Principal Residence")

myHMA <- myHMA_SFOO 

# Developing Hazards from projectType data ####

unique_hazards <- unique(myHMA$p2projectType)
Hazards_U <- data.frame(p2projectType = unique_hazards)

# Define the hazards
hazards <- c("Wind", "Wildfire", "Flood", "Erosion", "Riverine", "Coastal", "Stormwater", "Seismic", "Landslide", "Snow Avalanche", "Tsunami")

# Create binary columns for each hazard
for (hazard in hazards) {
  Hazards_U[[hazard]] <- as.integer(str_detect(Hazards_U$p2projectType, hazard))
}

# Check the tables for each hazard
sapply(hazards, function(hazard) table(Hazards_U[[hazard]]))

# Examining if there is more than one hazard to assign "Mix"
Hazards_U$Total <- rowSums(Hazards_U[, hazards])

# Classify hazards
Hazards_U$myHazard <- apply(Hazards_U, 1, function(row) {
  if (row["Total"] == 1) {
    return(hazards[which(row[hazards] == 1)])
  } else {
    return("MultiHazard")
  }
})

# Select necessary columns
Hazards_U2 <- Hazards_U[, c("p2projectType", "myHazard")]

# Joining Hazards data
myHMA <- left_join(myHMA, Hazards_U2, by = "p2projectType")

#examining time####
range(myHMA$programFy) #1998-2023


#Selecting Variables to Keep ####
#New fields created to keep: typeOfResidency2, propertyAction3, myHazard

colSums(is.na(myHMA))
names(myHMA)
myHMA2 <-myHMA %>%
  select(
    id,
    zip,
    city, 
    county, p2county, p2countyCode,
    stateNumberCode, p2state,
    region, p2region,
    projectIdentifier,
    propertyAction, propertyAction3,
    structureType,
    typeOfResidency, 
    foundationType,
    programArea,
    programFy,
    actualAmountPaid,
    disasterNumber,
    myHazard,
    damageCategory,
    p2projectType,
    p2status,
    p2subrecipient,
    p2dateApproved,
    p2dateClosed,
    p2projectAmount,
    p2federalShareObligated,
    p2subrecipientAdminCostAmt,
    p2recipientAdminCostAmt,
    p2costSharePercentage,
    p2benefitCostRatio,
    p2netValueBenefits,
    p2numberOfFinalProperties)

colSums(is.na(myHMA2))

# subset to relevant Acquisitions & Elevations###
table(myHMA2$propertyAction3)

myHMA3 <- subset(myHMA2, 
                 propertyAction3=="Acquisition" |
                   propertyAction3=="Elevation" 
                 )



#archiving raw HMA data for replication ####
#path1 <- ("C:/Users/lgero/Box/Research/FEMA_project/Data/Archive")
#write.csv(properties, file.path(path1, "HMA_Properties_07_03_24.csv"), row.names=TRUE)
#write.csv(projects, file.path(path1, "HMA_Projects_07_03_24.csv"), row.names=TRUE)

#writing out processed data ####
path1 <- ("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA_SFH")
write.csv(myHMA3, file.path(path1, "myHMA.csv"), row.names=TRUE)

# Setup
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)


# Importing data
myHMA <- read.csv('C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA_SFH/myHMA.csv')

#read in disaster data ####
#DisDataRaw <- GET("https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries.json")
#DisDataRaw2 <- fromJSON(rawToChar(DisDataRaw$content))
#Dis <- as.data.frame(DisDataRaw2)

#Reading from Archive
Dis <- read.csv("C:/Users/lgero/Box/Research/FEMA_project/Data/Archive/Dis_07_03_24.csv")

#EXploring Data
names(Dis)

#renaming columns####
colnames(Dis) <- gsub(pattern="DisasterDeclarationsSummaries.",replacement="",x=colnames(Dis))


#cleaning 
myHMA <- myHMA %>%
  select(-1) %>%
  mutate(
    GZCTA = str_c('G', str_pad(zip, 5, pad = "0")),
    DN = str_pad(disasterNumber, 4, pad = "0")
  )


Dis$DN <- str_pad(Dis$disasterNumber, 4, pad="0")

names(Dis)
Dis2 <- Dis %>%
  select(
    DN,
    declarationType,
    fyDeclared,
    incidentType,
    declarationTitle)

Dis3 <- distinct(Dis2)


#join 
myHMA2 <- left_join(myHMA, Dis3, by="DN")

#subset to relevant flood related disasters ####
table(myHMA2$incidentType)
myHMA3 <- subset(myHMA2, incidentType!="Biological" &
                 incidentType!="Chemical" &
                 incidentType!="Drought" &
                 incidentType!="Earthquake" &
                 incidentType!="Fire" &
                 incidentType!="Fishing Losses" &
                 incidentType!="Freezing" &
                 incidentType!="Human Cause" &
                 incidentType!="Other" &
                 incidentType!="Severe Ice Storm" &
                 incidentType!="Snowstorm" &
                 incidentType!="Terrorist" &
                 incidentType!="Tornado" &
                 incidentType!="Toxic Substances" &
                 incidentType!="Volcanic Eruption")

#cleaning disaster data and extracting additional information ####

#formating declarationTitle
#replace punctuation 
myHMA3$declarationTitle <- str_replace_all(myHMA3$declarationTitle, "[[:punct:]]", " ")

#change to lower case
myHMA3$declarationTitle <- tolower(myHMA3$declarationTitle)


#cleaning declarationTitle names ####
dis_names <- myHMA3 %>%
  select(disasterNumber, declarationTitle) %>%  # Replace 'id' with your actual identifier variable name
  distinct(declarationTitle, .keep_all = TRUE)

names(dis_names)[names(dis_names)=="unique(myHMA3$declarationTitle)"] <- "declarationTitle"


#extract storm names and hazard types
dis_names$Hurricane <- ifelse(dis_names$declarationTitle %like% "hurricane", 1,0)
dis_names$SevereStorm<- ifelse(dis_names$declarationTitle %like% "severe storm", 1,0)
dis_names$TropicalStorm<- ifelse(dis_names$declarationTitle %like% "tropical storm", 1,0)
dis_names$CoastalStorm<- ifelse(dis_names$declarationTitle %like% "coastal storm", 1,0)
dis_names$Tornado<- ifelse(dis_names$declarationTitle %like% "tornado", 1,0)
dis_names$Wind<- ifelse(dis_names$declarationTitle %like% "wind", 1,0)
dis_names$WinterStorm<- ifelse(dis_names$declarationTitle %like% "winter storm", 1,0)

dis_names$GroundSaturation<- ifelse(dis_names$declarationTitle %like% "ground saturation", 1,0)
dis_names$Flood<- ifelse(dis_names$declarationTitle %like% "flood", 1,0)
dis_names$Mudslide<- ifelse(dis_names$declarationTitle %like% "mudslide", 1,0)
dis_names$Landslide<- ifelse(dis_names$declarationTitle %like% "Landslide", 1,0)
dis_names$HighSurf<- ifelse(dis_names$declarationTitle %like% "high surf", 1,0)
dis_names$TorrentialRain<- ifelse(dis_names$declarationTitle %like% "torrential rain", 1,0)


#finding hurricane and tropical storm names####

#finding hurricane name

# Use a regular expression to extract the word following "hurricane"
extract_hurricane_name <- function(title) {
  match <- regexpr("hurricane\\s+(\\w+)", title)
  if (match[1] != -1) {
    return(regmatches(title, match)[1])
  } else {
    return(NA)
  }
}

# Apply the function to the declarationTitle column
dis_names$HurricaneName <- sapply(dis_names$declarationTitle, function(title) {
  match <- regexpr("hurricane\\s+(\\w+)", title)
  if (match[1] != -1) {
    regmatches(title, match)[1] %>%
      sub("hurricane\\s+", "", .) # Remove the "hurricane " part
  } else {
    NA
  }
})

#Quality check
table(dis_names$HurricaneName)

#finding tropical storm name

# Use a regular expression to extract the word following "hurricane"
extract_storm_name <- function(title) {
  match <- regexpr("tropical storm\\s+(\\w+)", title)
  if (match[1] != -1) {
    return(regmatches(title, match)[1])
  } else {
    return(NA)
  }
}

# Apply the function to the declarationTitle column
dis_names$TropicalStormName <- sapply(dis_names$declarationTitle, function(title) {
  match <- regexpr("tropical storm\\s+(\\w+)", title)
  if (match[1] != -1) {
    regmatches(title, match)[1] %>%
      sub("tropical storm\\s+", "", .) # Remove the "tropical storm" part
  } else {
    NA
  }
})

#joining newly created disaster data back into HMA data ####
dis_names <- dis_names %>%
  select(-declarationTitle)

myHMA4 <- left_join(myHMA3, dis_names, by="disasterNumber")
colSums(is.na(myHMA4))

#changing NAs to 0s on new disaster data
myHMA4 <- myHMA4 %>%
  mutate(across(c(Hurricane, SevereStorm, TropicalStorm, CoastalStorm,
                  Tornado, Wind, WinterStorm, GroundSaturation, Flood,
                  Mudslide, Landslide, HighSurf, TorrentialRain), ~
                  replace_na(.x,0)))

table(myHMA4$incidentType)

#archiving raw Disaster data for replication ####
#path1 <- ("C:/Users/lgero/Box/Research/FEMA_project/Data/Archive")
#write.csv(Dis, file.path(path1, "Dis_07_03_24.csv"), row.names=TRUE)


#writing out data####
path1 <- ("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA_SFH")
write.csv(myHMA6, file.path(path1, "myHMA6.csv"), row.names=TRUE)


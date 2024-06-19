#Purpose:
#Downloading and cleaning disaster data

##Setup####
getwd()

library(dplyr)
library(stringr)
library(Hmisc)
library(tidycensus)
library(data.table)

library(httr)
library(jsonlite)

#read in disaster data####
DisDataRaw <- GET("https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries.json")
DisDataRaw2 <- fromJSON(rawToChar(DisDataRaw$content))
Dis <- as.data.frame(DisDataRaw2)

#EXploring Data
names(Dis)

#renaming columns####
colnames(Dis) <- gsub(pattern="DisasterDeclarationsSummaries.",replacement="",x=colnames(Dis))

#creating county_DN####
range(Dis$disasterNumber)
names(Dis)[names(Dis)=="disasterNumber"] <- "DN"
Dis$DN <- str_pad(Dis$DN, 4, pad="0")
range(Dis$DN)

range(Dis$fipsStateCode)
Dis$fipsStateCode <- str_pad(Dis$fipsStateCode, 2, pad="0")

range(Dis$fipsCountyCode)
Dis$fipsCountyCode<- str_pad(Dis$fipsCountyCode, 3, pad="0")

Dis$cntyID <- str_c("C", Dis$fipsStateCode,Dis$fipsCountyCode)
head(Dis$cntyID)

Dis$county_DN <- str_c(Dis$DN, Dis$cntyID)
head(Dis$county_DN)
range(Dis$county_DN)

##county_YOL (Year of Loss)####
head(Dis$declarationDate)
Dis$DD <- substr(Dis$declarationDate, 1,4)
range(Dis$DD)

Dis$county_YOL <- str_c(Dis$DD,Dis$cntyID)
head(Dis$county_YOL)

#NOTE: YOLZ / DNZ - NOT AVAILABLE - NO ZCTA in Disaster data####

#examining storm names ####
dis_names <- as.data.frame(unique(Dis$declarationTitle))


#subsetting to disasters to 1989+ ####
range(Dis$declarationDate)
range(Dis$DD)

Dis2 <- subset(Dis, DD>=1989)

#Subsetting to flood related disasters####
table(Dis2$incidentType)

Dis3 <- subset(Dis2, incidentType!="Biological" &
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

table(Dis3$incidentType)


##Writing out files
path1 <- ("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/DisasterData")
write.csv(Dis, file.path(path1, "DisAll.csv"), row.names=TRUE)
write.csv(Dis3, file.path(path1, "DisFloodStorm.csv"), row.names=TRUE)

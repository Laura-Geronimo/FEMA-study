#Subsetting to ZCTAs and Counties in current geographies


getwd()
setwd('G:/My Drive/Academic/Research')

##Libraries
#install.packages(c("httr", "jsonlite"))
#install.packages("rjson")
#install.packages("httr")
#install.packages("data.table")
#install.packages("sqldf")
library(data.table)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(rjson)
library(httr)
library(sqldf)
library(tibble)
library(tidycensus)

##Importing data####
IHP_V6 <- read.csv('./Data/FEMA/IA/Produced/FS/IHP_V6_FS.csv', header=TRUE)
state <- read.csv('./Data/Census/StateNameAbbrCode.csv')
US_ZCTA <-get_acs("zcta",variables = "B25077_001", 
                  year=2019)
US_County <-get_acs("county",variables = "B25077_001", 
                    year=2019)

#cleaning
head(IHP_V6)
names(IHP_V6)
IHP_V6 <- IHP_V6[,c(-1)]
colSums(is.na(IHP_V6))

head(state)
names(state)[names(state)=="ï..State"]<- "stateName"
names(state)[names(state)=="FIPS"]<- "stateFIPS"
state$FIPS<- str_pad(state$FIPS,2, pad="0")
colSums(is.na(state))


names(US_County)[names(US_County)=="GEOID"]<- "countyFIPS"
names(US_County)[names(US_County)=="NAME"]<- "countyNAME"
names(US_County)
head(US_County)
US_County <- US_County[,c(1,2)]
US_County$stateName <- sub(".*, ","",US_County$countyNAME)
colSums(is.na(US_County))
USC_states <- US_County[,c(3)]
USC_states <- distinct(USC_states)

#creating My_CountyName in US_County
#pulling state acronyms to US_County to 
US_County2 <- left_join(US_County, state, by="stateName", copy=F)
head(US_County2)
colSums(is.na(US_County2))
US_County2$countyNAME <- sub("\\,.*","",US_County2$countyNAME)
US_County2$My_countyName <- str_c(US_County2$countyNAME, ", ", US_County2$stateAbr)
US_County2 <- US_County2[,c(1,3,4,5,6)]
US_County2$my_flag <- 1

#creating My_countyName in IHP file
head(IHP_V6)
names(IHP_V6)
IHP_V6$My_countyName <- sub("\\).*","",IHP_V6$county)
IHP_V6$My_countyName <- sub("\\(","",IHP_V6$My_countyName)
IHP_V6$My_countyName <- str_c(IHP_V6$My_countyName,", ",IHP_V6$damagedStateAbbreviation)
colSums(is.na(IHP_V6))
head(IHP_V6$My_countyName)

#examining unique countyNames
IHP_countyNames <- as.data.frame(IHP_V6[,c(25)])
IHP_countyNames <- distinct(IHP_countyNames) #2138 distinct


#Attempt Join1####
IHP_V7 <- left_join(IHP_V6, US_County2, by="My_countyName", copy=F)
colSums(is.na(IHP_V7)) #issue: 27459 did not match on My_countyName

#diagnosing issues1####
seeNAs <- IHP_V7[rowSums(is.na(IHP_V7)) > 0, ]
names(seeNAs)
seeNAs <- seeNAs[,c(25:29)]
seeNAs <- distinct(seeNAs) #87 - manageable

###matching the US_County2 file to the IHP file for now####
#not in US County2:
  #Alaska Gateway Regional Educational Attendance Area, AK
  #Big Cypress Indian Reservation, FL
  #Blackfeet Indian Reservation, MT
  #	Brighton Indian Reservation, FL
  #Chignik Lagoon ANV/ANVSA, AK
  #Dona Ana County, NM
  #Eastern District, AS
  #Fort Belknap Indian Reservation, MT
  #Fort Peck Indian Reservation, MT
  #Fort Pierce Indian Reservation, FL
  #Guam County-equivalent, GU
  #Immokalee Indian Reservation, FL
  #Kuspuk Regional Educational Attendance Area, AK
  #La Salle County, IL
  #La Salle Parish, LA
  #Lower Yukon Regional Educational Attendance Area, AK
  #Manu'a District, AS
  #Mashantucket Pequot Indian Reservation, CT
  #Mohegan Trust Lands, CT
  #Oglala Sioux Tribe of the Pine Ridge Reservation, SD
  #Rose Island Island, AS
  #Rota Municipality, MP
  #Saipan Municipality, MP
  #Santee Indian Reservation, NE
  #Spirit Lake Reservation, ND
  #St. Croix County, VI
  #St. John County, VI
  #St. Thomas County, VI
  #Standing Rock Sioux Tribe of North & South Dakota, ND
  #Tinian Municipality, MP
  #Turtle Mountain Indian Reservation, ND
  #Western District, AS
  #Yap County-equivalent, FM
  #Yukon Flats Regional Educational Attendance Area, AK
  #Yukon Koyukuk Regional Educational Attendance Area, AK
  #Yupiit Regional Educational Attendance Area, AK



#changed:
US_County2$My_countyName[US_County2$My_countyName=="Alexandria city, VA"] <- "Alexandria, VA"
US_County2$My_countyName[US_County2$My_countyName=="Añasco Municipio, PR"] <- "Anasco Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Baltimore city, MD"] <- "Baltimore, MD"
US_County2$My_countyName[US_County2$My_countyName=="Bayamón Municipio, PR"] <- "Bayamon Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Beaufort County, NC"] <- "Beaufort County, NC"
US_County2$My_countyName[US_County2$My_countyName=="Buena Vista city, VA"] <- "Buena Vista, VA"
US_County2$My_countyName[US_County2$My_countyName=="Canóvanas Municipio, PR"] <- "Canovanas Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Cataño Municipio, PR"] <- "Catano Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Charlottesville County, VA"] <- "Charlottesville city, VA"
US_County2$My_countyName[US_County2$My_countyName=="Chesapeake city, VA"] <- "Chesapeake, VA"
US_County2$My_countyName[US_County2$My_countyName=="Colonial Heights city, VA"] <- "Colonial Heights, VA"
US_County2$My_countyName[US_County2$My_countyName=="Comerío Municipio, PR"] <- "Comerio Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Danville County, VA"] <- "Danville city, VA"
US_County2$My_countyName[US_County2$My_countyName=="District of Columbia, DC"] <- "District of Columbia County-equivalent, DC"
US_County2$My_countyName[US_County2$My_countyName=="District of Columbia, DC"] <- ""
US_County2$My_countyName[US_County2$My_countyName=="Emporia city, VA"] <- "Emporia, VA"
US_County2$My_countyName[US_County2$My_countyName=="Fairfax County, VA"] <- "Fairfax, VA"
US_County2$My_countyName[US_County2$My_countyName=="Fairfax city, VA"] <- "Fairfax, VA"
US_County2$My_countyName[US_County2$My_countyName=="Falls Church County, VA"] <- "Falls Church, VA"
US_County2$My_countyName[US_County2$My_countyName=="Franklin County, VA"] <- "Franklin, VA"
US_County2$My_countyName[US_County2$My_countyName=="Fredericksburg city, VA"] <- "Fredericksburg, VA"
US_County2$My_countyName[US_County2$My_countyName=="Galax city, VA"] <- "Galax, VA"
US_County2$My_countyName[US_County2$My_countyName=="Guánica Municipio, PR"] <- "Guanica Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Hampton city, VA"] <- "Hampton, VA"
US_County2$My_countyName[US_County2$My_countyName=="Harrisonburg city, VA"] <- "Harrisonburg, VA"
US_County2$My_countyName[US_County2$My_countyName=="Hopewell city, VA"] <- "Hopewell, VA"
US_County2$My_countyName[US_County2$My_countyName=="Juana Díaz Municipio, PR"] <- "Juana Diaz Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Las Marías Municipio, PR"] <- "Las Marias Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Loíza Municipio, PR"] <- "Loiza Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Lynchburg city, VA"] <- "Lynchburg, VA"
US_County2$My_countyName[US_County2$My_countyName=="Manatí Municipio, PR"] <- "Manati Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Mayagüez Municipio, PR"] <- "Mayaguez Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Newport News city, VA"] <- "Newport News, VA"
US_County2$My_countyName[US_County2$My_countyName=="Norfolk city, VA"] <- "Norfolk, VA"
US_County2$My_countyName[US_County2$My_countyName=="Norton city, VA"] <- "Norton, VA"
US_County2$My_countyName[US_County2$My_countyName=="Peñuelas Municipio, PR"] <- "Penuelas Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Petersburg city, VA"] <- "Petersburg, VA"
US_County2$My_countyName[US_County2$My_countyName=="Poquoson city, VA"] <- "Poquoson, VA"
US_County2$My_countyName[US_County2$My_countyName=="Portsmouth city, VA"] <- "Portsmouth, VA"
US_County2$My_countyName[US_County2$My_countyName=="Richmond city, VA"] <- "Richmond, VA"
US_County2$My_countyName[US_County2$My_countyName=="Rincón Municipio, PR"] <- "Rincon Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Río Grande Municipio, PR"] <- "Rio Grande Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="Roanoke city, VA"] <- "Roanoke, VA"
US_County2$My_countyName[US_County2$My_countyName=="Salem city, VA"] <- "Salem, VA"
US_County2$My_countyName[US_County2$My_countyName=="San Germán Municipio, PR"] <- "San German Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="San Sebastián Municipio, PR"] <- "San Sebastian Municipio, PR"
US_County2$My_countyName[US_County2$My_countyName=="St. Louis city, MO"] <- "St. Louis, MO"
US_County2$My_countyName[US_County2$My_countyName=="Staunton city, VA"] <- "Staunton, VA"
US_County2$My_countyName[US_County2$My_countyName=="Suffolk city, VA"] <- "Suffolk, VA"
US_County2$My_countyName[US_County2$My_countyName=="Virginia Beach city, VA"] <- "Virginia Beach, VA"
US_County2$My_countyName[US_County2$My_countyName=="Waynesboro city, VA"] <- "Waynesboro, VA"
US_County2$My_countyName[US_County2$My_countyName=="Williamsburg city, VA"] <- "Williamsburg, VA"
US_County2$My_countyName[US_County2$My_countyName=="Wyoming County, WV"] <- "Wyoming County, WV"

#Attempt Join 2####
IHP_V8 <- left_join(IHP_V6, US_County2, by="My_countyName", copy=F)
colSums(is.na(IHP_V8)) #brought down from 27459 to 7260  that did not match

#diagnosing issues####
seeNAs <- IHP_V8[rowSums(is.na(IHP_V8)) > 0, ]
names(seeNAs)
seeNAs <- seeNAs[,c(25:29)]
seeNAs <- distinct(seeNAs) #42 - now 42 distinct counties--> mostly islands, aslaska, indian reservations...

#examining data####
colSums(is.na(IHP_V8))

#examining missing data on GZCTA####
head(US_ZCTA)
US_ZCTA <- as.data.frame(US_ZCTA[,c(1)])
range(US_ZCTA$GEOID)
US_ZCTA$GZCTA <- str_c("G", US_ZCTA$GEOID)
US_ZCTA$my_Z_flag <- 1
US_ZCTA <- US_ZCTA[, c(2,3)]

#Attempting ZCTA join1 ####
IHP_V9 <- left_join(IHP_V8, US_ZCTA, by="GZCTA", copy= F)

##examining missing data####
colSums(is.na(IHP_V9))
seeNAs <- IHP_V9[rowSums(is.na(IHP_V9)) > 0, ] #14003 observations missing data on zcta or county
names(seeNAs)

#comparing what would be lost####

QCnum1_floodDamageAmount <- sum(seeNAs$floodDamageAmountAdj)/ sum(IHP_V9$floodDamageAmountAdj) #0.005 less than 1percent
QCnum2_ihpAmount <- sum(seeNAs$ihpAmountAdj) / sum(IHP_V9$ihpAmountAdj) #0.006 less than 1percent
QCnum3_OtherDamages <- sum(seeNAs$rpfvlAdj + seeNAs$ppfvlAdj) / sum(IHP_V9$rpfvlAdj + IHP_V9$ppfvlAdj) #0.005 less than 1 percent
QCnum4_OtherAssistance <- sum(seeNAs$rentalAssistanceAmountAdj + seeNAs$repairAmountAdj + seeNAs$replacementAmountAdj)/
                          sum(IHP_V9$rentalAssistanceAmountAdj + IHP_V9$repairAmountAdj + IHP_V9$replacementAmountAdj) #0.0057 less than 1 percent

##dropping NAs####
IHP_V10 <- na.omit(IHP_V9)

#cleaning and exporting###
colSums(is.na(IHP_V10))
names(IHP_V10)

IHP_V10 <- IHP_V10[,c(-14,-30,-31,-32)]

#writing out files#####
path2 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/PropLevel'
write.csv(IHP_V10, file.path(path2, "IHP_V10_FS.csv"), row.names=TRUE)


  

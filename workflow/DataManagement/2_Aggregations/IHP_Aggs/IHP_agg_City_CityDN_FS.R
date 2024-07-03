#Aggregating IHP data by State and cityDN


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



##Importing data####
IHP_V10 <- read.csv('G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/PropLevel/IHP_V10_FS.csv', header=TRUE)


##Examining Data####
names(IHP_V10)
IHP_V10 <- IHP_V10[,c(-1)]



###Aggregating Vars of interest by City_DN####
##Creating unique City_DN####
names(IHP_V10)
head(IHP_V10)
colSums(is.na(IHP_V10))
table(IHP_V10$damagedCity)
IHP_V10$cityID <- str_c(IHP_V10$damagedCity, ", ", IHP_V10$damagedStateAbbreviation)
head(IHP_V10$cityID)

range(IHP_V10$disasterNumber)
IHP_V10$cityDN <- str_c(IHP_V10$cityID, " ", IHP_V10$disasterNumber)
head(IHP_V10$cityDN)



IHP_V10$cityDN <- str_c(IHP_V10$disasterNumber,IHP_V10$stateID)
head(IHP_V10$cityDN)
cityDN <- data.frame(unique(IHP_V10$cityDN)) #362
names(cityDN)
names(cityDN)[names(cityDN)=="unique.IHP_V10.cityDN."] <- "cityDN"


#Aggregating potential vars of interest by cityDN####
#reference
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_V10$replacementAmountAdj) #331523281

#Aggregating sums by cityDN
names(IHP_V10)
IHP_AmountAdjcityDN <- aggregate(IHP_V10$ihpAmountAdj~IHP_V10$cityDN, FUN=sum) 
IHP_haAmountAdjcityDN <- aggregate(IHP_V10$haAmountAdj~IHP_V10$cityDN, FUN=sum)
IHP_onaAmountAdjcityDN <- aggregate(IHP_V10$onaAmountAdj~IHP_V10$cityDN, FUN=sum)
IHP_fldDamAmountAdjcityDN <- aggregate(IHP_V10$floodDamageAmountAdj~IHP_V10$cityDN, FUN=sum) 
IHP_rpfvlAdjcityDN <- aggregate(IHP_V10$rpfvlAdj~IHP_V10$cityDN, FUN=sum) 
IHP_ppfvlAdjcityDN <- aggregate(IHP_V10$ppfvlAdj~IHP_V10$cityDN, FUN=sum) 
IHP_rentalAssistanceAmountAdjcityDN <- aggregate(IHP_V10$rentalAssistanceAmountAdj~IHP_V10$cityDN, FUN=sum) 
IHP_repairAmountAdjcityDN <- aggregate(IHP_V10$repairAmountAdj~IHP_V10$cityDN, FUN=sum) 
IHP_replacementAmountAdjcityDN <- aggregate(IHP_V10$replacementAmountAdj~IHP_V10$cityDN, FUN=sum) 


#IHP_AmountAdjcityDN####
head(IHP_AmountAdjcityDN)
names(IHP_AmountAdjcityDN)[names(IHP_AmountAdjcityDN)=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_AmountAdjcityDN)[names(IHP_AmountAdjcityDN)=="IHP_V10$ihpAmountAdj"] <- "IHP_AmountAdjcityDN"
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_AmountAdjcityDN$IHP_AmountAdjcityDN)#13925313982 good

#IHP_haAmountAdjcityDN####
head(IHP_haAmountAdjcityDN)
names(IHP_haAmountAdjcityDN)[names(IHP_haAmountAdjcityDN)=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_haAmountAdjcityDN)[names(IHP_haAmountAdjcityDN)=="IHP_V10$haAmountAdj"] <- "IHP_haAmountAdjcityDN"
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_haAmountAdjcityDN$IHP_haAmountAdjcityDN)#10775699067 good

#IHP_onaAmountAdjcityDN####
head(IHP_onaAmountAdjcityDN)
names(IHP_onaAmountAdjcityDN)[names(IHP_onaAmountAdjcityDN)=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_onaAmountAdjcityDN)[names(IHP_onaAmountAdjcityDN)=="IHP_V10$onaAmountAdj"] <- "IHP_onaAmountAdjcityDN"
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_onaAmountAdjcityDN$IHP_onaAmountAdjcityDN)#3149614917good

#IHP_fldDamAmountAdjcityDN####
head(IHP_fldDamAmountAdjcityDN)
names(IHP_fldDamAmountAdjcityDN)[names(IHP_fldDamAmountAdjcityDN)=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_fldDamAmountAdjcityDN)[names(IHP_fldDamAmountAdjcityDN)=="IHP_V10$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjcityDN"
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_fldDamAmountAdjcityDN$IHP_fldDamAmountAdjcityDN) #29262312328 good


#IHP_rpfvlAdjcityDN####
head(IHP_rpfvlAdjcityDN)
names(IHP_rpfvlAdjcityDN)[names(IHP_rpfvlAdjcityDN)=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_rpfvlAdjcityDN)[names(IHP_rpfvlAdjcityDN)=="IHP_V10$rpfvlAdj"] <- "IHP_rpfvlAdjcityDN"
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_rpfvlAdjcityDN$IHP_rpfvlAdjcityDN) #24635761202 good


#IHP_ppfvlAdjcityDN####
head(IHP_ppfvlAdjcityDN)
names(IHP_ppfvlAdjcityDN)[names(IHP_ppfvlAdjcityDN)=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_ppfvlAdjcityDN)[names(IHP_ppfvlAdjcityDN)=="IHP_V10$ppfvlAdj"] <- "IHP_ppfvlAdjcityDN"
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_ppfvlAdjcityDN$IHP_ppfvlAdjcityDN) #5902773648 good


#IHP_rentalAssistanceAmountAdjcityDN####
head(IHP_rentalAssistanceAmountAdjcityDN)
names(IHP_rentalAssistanceAmountAdjcityDN)[names(IHP_rentalAssistanceAmountAdjcityDN)=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_rentalAssistanceAmountAdjcityDN)[names(IHP_rentalAssistanceAmountAdjcityDN)=="IHP_V10$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjcityDN"
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_rentalAssistanceAmountAdjcityDN$IHP_rentalAssistanceAmountAdjcityDN) #3276879805 good

#IHP_repairAmountAdjcityDN ####
head(IHP_repairAmountAdjcityDN )
names(IHP_repairAmountAdjcityDN )[names(IHP_repairAmountAdjcityDN )=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_repairAmountAdjcityDN )[names(IHP_repairAmountAdjcityDN )=="IHP_V10$repairAmountAdj"] <- "IHP_repairAmountAdjcityDN"
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_repairAmountAdjcityDN $IHP_repairAmountAdjcityDN ) #6633263233 good

#IHP_replacementAmountAdjcityDN ####
head(IHP_replacementAmountAdjcityDN)
names(IHP_replacementAmountAdjcityDN )[names(IHP_replacementAmountAdjcityDN)=="IHP_V10$cityDN"] <- "cityDN"
names(IHP_replacementAmountAdjcityDN )[names(IHP_replacementAmountAdjcityDN)=="IHP_V10$replacementAmountAdj"] <- "IHP_replacementAmountAdjcityDN"
sum(IHP_V10$replacementAmountAdj) #331523281
sum(IHP_replacementAmountAdjcityDN$IHP_replacementAmountAdjcityDN) #331523281 good


##Joining####
IHP_cityDN1 <- left_join(cityDN, IHP_AmountAdjcityDN, by= "cityDN", copy= F)
IHP_cityDN2 <- left_join(IHP_cityDN1, IHP_haAmountAdjcityDN, by="cityDN", copy=FALSE)
IHP_cityDN3 <- left_join(IHP_cityDN2,IHP_onaAmountAdjcityDN, by="cityDN", copy=FALSE)
IHP_cityDN4 <- left_join(IHP_cityDN3, IHP_fldDamAmountAdjcityDN, by="cityDN", copy=FALSE)
IHP_cityDN5 <- left_join(IHP_cityDN4, IHP_rpfvlAdjcityDN, by="cityDN", copy=FALSE)
IHP_cityDN6 <- left_join(IHP_cityDN5, IHP_ppfvlAdjcityDN , by="cityDN", copy=FALSE)
IHP_cityDN7 <- left_join(IHP_cityDN6, IHP_rentalAssistanceAmountAdjcityDN, by="cityDN", copy=FALSE)
IHP_cityDN8 <- left_join(IHP_cityDN7, IHP_repairAmountAdjcityDN, by="cityDN", copy=FALSE)
IHP_cityDN9 <- left_join(IHP_cityDN8, IHP_replacementAmountAdjcityDN, by="cityDN", copy=FALSE)

names(IHP_cityDN9)
colSums(is.na(IHP_cityDN9))
head(IHP_cityDN9) 

#QC
sum(IHP_V10$ihpAmountAdj)
sum(IHP_cityDN9$IHP_AmountAdjcityDN)

sum(IHP_V10$floodDamageAmountAdj)
sum(IHP_cityDN9$IHP_fldDamAmountAdjcityDN)



#############################################
##Aggregating  by cityID
#reference
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_V10$replacementAmountAdj) #331523281

#creating cityID
names(IHP_V10)
head(IHP_V10$cityID)

#Creating unique cityID
cityID <- data.frame(unique(IHP_V10$cityID)) #49 - 
names(cityID)
names(cityID)[names(cityID)=="unique.IHP_V10.cityID."] <- "cityID"

#Aggregating sums by cityID
names(IHP_V10)
IHP_AmountAdjcityID <- aggregate(IHP_V10$ihpAmountAdj~IHP_V10$cityID, FUN=sum) 
IHP_haAmountAdjcityID <- aggregate(IHP_V10$haAmountAdj~IHP_V10$cityID, FUN=sum)
IHP_onaAmountAdjcityID <- aggregate(IHP_V10$onaAmountAdj~IHP_V10$cityID, FUN=sum)
IHP_fldDamAmountAdjcityID <- aggregate(IHP_V10$floodDamageAmountAdj~IHP_V10$cityID, FUN=sum) #14111 cityIDs
IHP_rpfvlAdjcityID <- aggregate(IHP_V10$rpfvlAdj~IHP_V10$cityID, FUN=sum) 
IHP_ppfvlAdjcityID <- aggregate(IHP_V10$ppfvlAdj~IHP_V10$cityID, FUN=sum) 
IHP_rentalAssistanceAmountAdjcityID <- aggregate(IHP_V10$rentalAssistanceAmountAdj~IHP_V10$cityID, FUN=sum) 
IHP_repairAmountAdjcityID <- aggregate(IHP_V10$repairAmountAdj~IHP_V10$cityID, FUN=sum) 
IHP_replacementAmountAdjcityID <- aggregate(IHP_V10$replacementAmountAdj~IHP_V10$cityID, FUN=sum) 


#IHP_AmountAdjcityID####
head(IHP_AmountAdjcityID)
names(IHP_AmountAdjcityID)[names(IHP_AmountAdjcityID)=="IHP_V10$cityID"] <- "cityID"
names(IHP_AmountAdjcityID)[names(IHP_AmountAdjcityID)=="IHP_V10$ihpAmountAdj"] <- "IHP_AmountAdjcityID"
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_AmountAdjcityID$IHP_AmountAdjcityID)#13925313982 good

#IHP_haAmountAdjcityID####
head(IHP_haAmountAdjcityID)
names(IHP_haAmountAdjcityID)[names(IHP_haAmountAdjcityID)=="IHP_V10$cityID"] <- "cityID"
names(IHP_haAmountAdjcityID)[names(IHP_haAmountAdjcityID)=="IHP_V10$haAmountAdj"] <- "IHP_haAmountAdjcityID"
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_haAmountAdjcityID$IHP_haAmountAdjcityID)#10775699067 good

#IHP_onaAmountAdjcityID####
head(IHP_onaAmountAdjcityID)
names(IHP_onaAmountAdjcityID)[names(IHP_onaAmountAdjcityID)=="IHP_V10$cityID"] <- "cityID"
names(IHP_onaAmountAdjcityID)[names(IHP_onaAmountAdjcityID)=="IHP_V10$onaAmountAdj"] <- "IHP_onaAmountAdjcityID"
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_onaAmountAdjcityID$IHP_onaAmountAdjcityID)#3149614917good

#IHP_fldDamAmountAdjcityID####
head(IHP_fldDamAmountAdjcityID)
names(IHP_fldDamAmountAdjcityID)[names(IHP_fldDamAmountAdjcityID)=="IHP_V10$cityID"] <- "cityID"
names(IHP_fldDamAmountAdjcityID)[names(IHP_fldDamAmountAdjcityID)=="IHP_V10$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjcityID"
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_fldDamAmountAdjcityID$IHP_fldDamAmountAdjcityID) #29262312328 good


#IHP_rpfvlAdjcityID####
head(IHP_rpfvlAdjcityID)
names(IHP_rpfvlAdjcityID)[names(IHP_rpfvlAdjcityID)=="IHP_V10$cityID"] <- "cityID"
names(IHP_rpfvlAdjcityID)[names(IHP_rpfvlAdjcityID)=="IHP_V10$rpfvlAdj"] <- "IHP_rpfvlAdjcityID"
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_rpfvlAdjcityID$IHP_rpfvlAdjcityID) #24635761202 good


#IHP_ppfvlAdjcityID####
head(IHP_ppfvlAdjcityID)
names(IHP_ppfvlAdjcityID)[names(IHP_ppfvlAdjcityID)=="IHP_V10$cityID"] <- "cityID"
names(IHP_ppfvlAdjcityID)[names(IHP_ppfvlAdjcityID)=="IHP_V10$ppfvlAdj"] <- "IHP_ppfvlAdjcityID"
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_ppfvlAdjcityID$IHP_ppfvlAdjcityID) #5902773648 good


#IHP_rentalAssistanceAmountAdjcityID####
head(IHP_rentalAssistanceAmountAdjcityID)
names(IHP_rentalAssistanceAmountAdjcityID)[names(IHP_rentalAssistanceAmountAdjcityID)=="IHP_V10$cityID"] <- "cityID"
names(IHP_rentalAssistanceAmountAdjcityID)[names(IHP_rentalAssistanceAmountAdjcityID)=="IHP_V10$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjcityID"
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_rentalAssistanceAmountAdjcityID$IHP_rentalAssistanceAmountAdjcityID) #3276879805 good

#IHP_repairAmountAdjcityID ####
head(IHP_repairAmountAdjcityID )
names(IHP_repairAmountAdjcityID )[names(IHP_repairAmountAdjcityID )=="IHP_V10$cityID"] <- "cityID"
names(IHP_repairAmountAdjcityID )[names(IHP_repairAmountAdjcityID )=="IHP_V10$repairAmountAdj"] <- "IHP_repairAmountAdjcityID"
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_repairAmountAdjcityID $IHP_repairAmountAdjcityID ) #6633263233 good

#IHP_replacementAmountAdjcityID ####
head(IHP_replacementAmountAdjcityID)
names(IHP_replacementAmountAdjcityID )[names(IHP_replacementAmountAdjcityID)=="IHP_V10$cityID"] <- "cityID"
names(IHP_replacementAmountAdjcityID )[names(IHP_replacementAmountAdjcityID)=="IHP_V10$replacementAmountAdj"] <- "IHP_replacementAmountAdjcityID"
sum(IHP_V10$replacementAmountAdj) #331523281
sum(IHP_replacementAmountAdjcityID$IHP_replacementAmountAdjcityID) #331523281 good


##Joining####
IHP_cityID1 <- left_join(cityID, IHP_AmountAdjcityID, by= "cityID", copy= F)
IHP_cityID2 <- left_join(IHP_cityID1, IHP_haAmountAdjcityID, by="cityID", copy=FALSE)
IHP_cityID3 <- left_join(IHP_cityID2,IHP_onaAmountAdjcityID, by="cityID", copy=FALSE)
IHP_cityID4 <- left_join(IHP_cityID3, IHP_fldDamAmountAdjcityID, by="cityID", copy=FALSE)
IHP_cityID5 <- left_join(IHP_cityID4, IHP_rpfvlAdjcityID, by="cityID", copy=FALSE)
IHP_cityID6 <- left_join(IHP_cityID5, IHP_ppfvlAdjcityID , by="cityID", copy=FALSE)
IHP_cityID7 <- left_join(IHP_cityID6, IHP_rentalAssistanceAmountAdjcityID, by="cityID", copy=FALSE)
IHP_cityID8 <- left_join(IHP_cityID7, IHP_repairAmountAdjcityID, by="cityID", copy=FALSE)
IHP_cityID9 <- left_join(IHP_cityID8, IHP_replacementAmountAdjcityID, by="cityID", copy=FALSE)

names(IHP_cityID9)
colSums(is.na(IHP_cityID9))
head(IHP_cityID9) 

#QC
sum(IHP_V10$ihpAmountAdj)
sum(IHP_cityID9$IHP_AmountAdjcityID)

sum(IHP_V10$floodDamageAmountAdj)
sum(IHP_cityID9$IHP_fldDamAmountAdjcityID)

#writing out files#####
path1 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/CityLevel'
write.csv(IHP_cityID9, file.path(path1, "City_FS_IHP.csv"), row.names=TRUE)

path2 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/cityDNLevel'
write.csv(IHP_cityDN9, file.path(path2, "cityDN_FS_IHP.csv"), row.names=TRUE)


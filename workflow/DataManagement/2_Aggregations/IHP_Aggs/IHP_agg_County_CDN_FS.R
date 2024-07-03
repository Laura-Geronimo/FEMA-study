#Aggregating IHP data by County and CDN


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



###Aggregating Vars of interest by CDN####
##Creating unique CDN####
names(IHP_V10)
colSums(is.na(IHP_V10))
range(IHP_V10$countyFIPS)
IHP_V10$countyFIPS <- str_pad(IHP_V10$countyFIPS, 5, pad="0")
IHP_V10$countyID <- str_c("C",IHP_V10$countyFIPS)
head(IHP_V10$countyID)

IHP_V10$CDN <- str_c(IHP_V10$disasterNumber,IHP_V10$countyID)
head(IHP_V10$CDN)
CDN <- data.frame(unique(IHP_V10$CDN)) #5131
names(CDN)
names(CDN)[names(CDN)=="unique.IHP_V10.CDN."] <- "CDN"


#Aggregating potential vars of interest by CDN####
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

#Aggregating sums by CDN
names(IHP_V10)
IHP_AmountAdjCDN <- aggregate(IHP_V10$ihpAmountAdj~IHP_V10$CDN, FUN=sum) 
IHP_haAmountAdjCDN <- aggregate(IHP_V10$haAmountAdj~IHP_V10$CDN, FUN=sum)
IHP_onaAmountAdjCDN <- aggregate(IHP_V10$onaAmountAdj~IHP_V10$CDN, FUN=sum)
IHP_fldDamAmountAdjCDN <- aggregate(IHP_V10$floodDamageAmountAdj~IHP_V10$CDN, FUN=sum) 
IHP_rpfvlAdjCDN <- aggregate(IHP_V10$rpfvlAdj~IHP_V10$CDN, FUN=sum) 
IHP_ppfvlAdjCDN <- aggregate(IHP_V10$ppfvlAdj~IHP_V10$CDN, FUN=sum) 
IHP_rentalAssistanceAmountAdjCDN <- aggregate(IHP_V10$rentalAssistanceAmountAdj~IHP_V10$CDN, FUN=sum) 
IHP_repairAmountAdjCDN <- aggregate(IHP_V10$repairAmountAdj~IHP_V10$CDN, FUN=sum) 
IHP_replacementAmountAdjCDN <- aggregate(IHP_V10$replacementAmountAdj~IHP_V10$CDN, FUN=sum) 


#IHP_AmountAdjCDN####
head(IHP_AmountAdjCDN)
names(IHP_AmountAdjCDN)[names(IHP_AmountAdjCDN)=="IHP_V10$CDN"] <- "CDN"
names(IHP_AmountAdjCDN)[names(IHP_AmountAdjCDN)=="IHP_V10$ihpAmountAdj"] <- "IHP_AmountAdjCDN"
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_AmountAdjCDN$IHP_AmountAdjCDN)#13925313982 good

#IHP_haAmountAdjCDN####
head(IHP_haAmountAdjCDN)
names(IHP_haAmountAdjCDN)[names(IHP_haAmountAdjCDN)=="IHP_V10$CDN"] <- "CDN"
names(IHP_haAmountAdjCDN)[names(IHP_haAmountAdjCDN)=="IHP_V10$haAmountAdj"] <- "IHP_haAmountAdjCDN"
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_haAmountAdjCDN$IHP_haAmountAdjCDN)#10775699067 good

#IHP_onaAmountAdjCDN####
head(IHP_onaAmountAdjCDN)
names(IHP_onaAmountAdjCDN)[names(IHP_onaAmountAdjCDN)=="IHP_V10$CDN"] <- "CDN"
names(IHP_onaAmountAdjCDN)[names(IHP_onaAmountAdjCDN)=="IHP_V10$onaAmountAdj"] <- "IHP_onaAmountAdjCDN"
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_onaAmountAdjCDN$IHP_onaAmountAdjCDN)#3149614917good

#IHP_fldDamAmountAdjCDN####
head(IHP_fldDamAmountAdjCDN)
names(IHP_fldDamAmountAdjCDN)[names(IHP_fldDamAmountAdjCDN)=="IHP_V10$CDN"] <- "CDN"
names(IHP_fldDamAmountAdjCDN)[names(IHP_fldDamAmountAdjCDN)=="IHP_V10$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjCDN"
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_fldDamAmountAdjCDN$IHP_fldDamAmountAdjCDN) #29262312328 good


#IHP_rpfvlAdjCDN####
head(IHP_rpfvlAdjCDN)
names(IHP_rpfvlAdjCDN)[names(IHP_rpfvlAdjCDN)=="IHP_V10$CDN"] <- "CDN"
names(IHP_rpfvlAdjCDN)[names(IHP_rpfvlAdjCDN)=="IHP_V10$rpfvlAdj"] <- "IHP_rpfvlAdjCDN"
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_rpfvlAdjCDN$IHP_rpfvlAdjCDN) #24635761202 good


#IHP_ppfvlAdjCDN####
head(IHP_ppfvlAdjCDN)
names(IHP_ppfvlAdjCDN)[names(IHP_ppfvlAdjCDN)=="IHP_V10$CDN"] <- "CDN"
names(IHP_ppfvlAdjCDN)[names(IHP_ppfvlAdjCDN)=="IHP_V10$ppfvlAdj"] <- "IHP_ppfvlAdjCDN"
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_ppfvlAdjCDN$IHP_ppfvlAdjCDN) #5902773648 good


#IHP_rentalAssistanceAmountAdjCDN####
head(IHP_rentalAssistanceAmountAdjCDN)
names(IHP_rentalAssistanceAmountAdjCDN)[names(IHP_rentalAssistanceAmountAdjCDN)=="IHP_V10$CDN"] <- "CDN"
names(IHP_rentalAssistanceAmountAdjCDN)[names(IHP_rentalAssistanceAmountAdjCDN)=="IHP_V10$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjCDN"
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_rentalAssistanceAmountAdjCDN$IHP_rentalAssistanceAmountAdjCDN) #3276879805 good

#IHP_repairAmountAdjCDN ####
head(IHP_repairAmountAdjCDN )
names(IHP_repairAmountAdjCDN )[names(IHP_repairAmountAdjCDN )=="IHP_V10$CDN"] <- "CDN"
names(IHP_repairAmountAdjCDN )[names(IHP_repairAmountAdjCDN )=="IHP_V10$repairAmountAdj"] <- "IHP_repairAmountAdjCDN"
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_repairAmountAdjCDN $IHP_repairAmountAdjCDN ) #6633263233 good

#IHP_replacementAmountAdjCDN ####
head(IHP_replacementAmountAdjCDN)
names(IHP_replacementAmountAdjCDN )[names(IHP_replacementAmountAdjCDN)=="IHP_V10$CDN"] <- "CDN"
names(IHP_replacementAmountAdjCDN )[names(IHP_replacementAmountAdjCDN)=="IHP_V10$replacementAmountAdj"] <- "IHP_replacementAmountAdjCDN"
sum(IHP_V10$replacementAmountAdj) #331523281
sum(IHP_replacementAmountAdjCDN$IHP_replacementAmountAdjCDN) #331523281 good


##Joining####
IHP_CDN1 <- left_join(CDN, IHP_AmountAdjCDN, by= "CDN", copy= F)
IHP_CDN2 <- left_join(IHP_CDN1, IHP_haAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN3 <- left_join(IHP_CDN2,IHP_onaAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN4 <- left_join(IHP_CDN3, IHP_fldDamAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN5 <- left_join(IHP_CDN4, IHP_rpfvlAdjCDN, by="CDN", copy=FALSE)
IHP_CDN6 <- left_join(IHP_CDN5, IHP_ppfvlAdjCDN , by="CDN", copy=FALSE)
IHP_CDN7 <- left_join(IHP_CDN6, IHP_rentalAssistanceAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN8 <- left_join(IHP_CDN7, IHP_repairAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN9 <- left_join(IHP_CDN8, IHP_replacementAmountAdjCDN, by="CDN", copy=FALSE)

names(IHP_CDN9)
colSums(is.na(IHP_CDN9))
head(IHP_CDN9) 



#############################################
##Aggregating  by County
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

#creating countyID
names(IHP_V10)
head(IHP_V10$countyID)

#Creating unique countyID
countyID <- data.frame(unique(IHP_V10$countyID)) #18372 - 
names(countyID)
names(countyID)[names(countyID)=="unique.IHP_V10.countyID."] <- "countyID"

#Aggregating sums by countyID
names(IHP_V10)
IHP_AmountAdjcountyID <- aggregate(IHP_V10$ihpAmountAdj~IHP_V10$countyID, FUN=sum) 
IHP_haAmountAdjcountyID <- aggregate(IHP_V10$haAmountAdj~IHP_V10$countyID, FUN=sum)
IHP_onaAmountAdjcountyID <- aggregate(IHP_V10$onaAmountAdj~IHP_V10$countyID, FUN=sum)
IHP_fldDamAmountAdjcountyID <- aggregate(IHP_V10$floodDamageAmountAdj~IHP_V10$countyID, FUN=sum) #14111 countyIDs
IHP_rpfvlAdjcountyID <- aggregate(IHP_V10$rpfvlAdj~IHP_V10$countyID, FUN=sum) 
IHP_ppfvlAdjcountyID <- aggregate(IHP_V10$ppfvlAdj~IHP_V10$countyID, FUN=sum) 
IHP_rentalAssistanceAmountAdjcountyID <- aggregate(IHP_V10$rentalAssistanceAmountAdj~IHP_V10$countyID, FUN=sum) 
IHP_repairAmountAdjcountyID <- aggregate(IHP_V10$repairAmountAdj~IHP_V10$countyID, FUN=sum) 
IHP_replacementAmountAdjcountyID <- aggregate(IHP_V10$replacementAmountAdj~IHP_V10$countyID, FUN=sum) 


#IHP_AmountAdjcountyID####
head(IHP_AmountAdjcountyID)
names(IHP_AmountAdjcountyID)[names(IHP_AmountAdjcountyID)=="IHP_V10$countyID"] <- "countyID"
names(IHP_AmountAdjcountyID)[names(IHP_AmountAdjcountyID)=="IHP_V10$ihpAmountAdj"] <- "IHP_AmountAdjcountyID"
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_AmountAdjcountyID$IHP_AmountAdjcountyID)#13925313982 good

#IHP_haAmountAdjcountyID####
head(IHP_haAmountAdjcountyID)
names(IHP_haAmountAdjcountyID)[names(IHP_haAmountAdjcountyID)=="IHP_V10$countyID"] <- "countyID"
names(IHP_haAmountAdjcountyID)[names(IHP_haAmountAdjcountyID)=="IHP_V10$haAmountAdj"] <- "IHP_haAmountAdjcountyID"
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_haAmountAdjcountyID$IHP_haAmountAdjcountyID)#10775699067 good

#IHP_onaAmountAdjcountyID####
head(IHP_onaAmountAdjcountyID)
names(IHP_onaAmountAdjcountyID)[names(IHP_onaAmountAdjcountyID)=="IHP_V10$countyID"] <- "countyID"
names(IHP_onaAmountAdjcountyID)[names(IHP_onaAmountAdjcountyID)=="IHP_V10$onaAmountAdj"] <- "IHP_onaAmountAdjcountyID"
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_onaAmountAdjcountyID$IHP_onaAmountAdjcountyID)#3149614917good

#IHP_fldDamAmountAdjcountyID####
head(IHP_fldDamAmountAdjcountyID)
names(IHP_fldDamAmountAdjcountyID)[names(IHP_fldDamAmountAdjcountyID)=="IHP_V10$countyID"] <- "countyID"
names(IHP_fldDamAmountAdjcountyID)[names(IHP_fldDamAmountAdjcountyID)=="IHP_V10$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjcountyID"
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_fldDamAmountAdjcountyID$IHP_fldDamAmountAdjcountyID) #29262312328 good


#IHP_rpfvlAdjcountyID####
head(IHP_rpfvlAdjcountyID)
names(IHP_rpfvlAdjcountyID)[names(IHP_rpfvlAdjcountyID)=="IHP_V10$countyID"] <- "countyID"
names(IHP_rpfvlAdjcountyID)[names(IHP_rpfvlAdjcountyID)=="IHP_V10$rpfvlAdj"] <- "IHP_rpfvlAdjcountyID"
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_rpfvlAdjcountyID$IHP_rpfvlAdjcountyID) #24635761202 good


#IHP_ppfvlAdjcountyID####
head(IHP_ppfvlAdjcountyID)
names(IHP_ppfvlAdjcountyID)[names(IHP_ppfvlAdjcountyID)=="IHP_V10$countyID"] <- "countyID"
names(IHP_ppfvlAdjcountyID)[names(IHP_ppfvlAdjcountyID)=="IHP_V10$ppfvlAdj"] <- "IHP_ppfvlAdjcountyID"
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_ppfvlAdjcountyID$IHP_ppfvlAdjcountyID) #5902773648 good


#IHP_rentalAssistanceAmountAdjcountyID####
head(IHP_rentalAssistanceAmountAdjcountyID)
names(IHP_rentalAssistanceAmountAdjcountyID)[names(IHP_rentalAssistanceAmountAdjcountyID)=="IHP_V10$countyID"] <- "countyID"
names(IHP_rentalAssistanceAmountAdjcountyID)[names(IHP_rentalAssistanceAmountAdjcountyID)=="IHP_V10$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjcountyID"
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_rentalAssistanceAmountAdjcountyID$IHP_rentalAssistanceAmountAdjcountyID) #3276879805 good

#IHP_repairAmountAdjcountyID ####
head(IHP_repairAmountAdjcountyID )
names(IHP_repairAmountAdjcountyID )[names(IHP_repairAmountAdjcountyID )=="IHP_V10$countyID"] <- "countyID"
names(IHP_repairAmountAdjcountyID )[names(IHP_repairAmountAdjcountyID )=="IHP_V10$repairAmountAdj"] <- "IHP_repairAmountAdjcountyID"
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_repairAmountAdjcountyID $IHP_repairAmountAdjcountyID ) #6633263233 good

#IHP_replacementAmountAdjcountyID ####
head(IHP_replacementAmountAdjcountyID)
names(IHP_replacementAmountAdjcountyID )[names(IHP_replacementAmountAdjcountyID)=="IHP_V10$countyID"] <- "countyID"
names(IHP_replacementAmountAdjcountyID )[names(IHP_replacementAmountAdjcountyID)=="IHP_V10$replacementAmountAdj"] <- "IHP_replacementAmountAdjcountyID"
sum(IHP_V10$replacementAmountAdj) #331523281
sum(IHP_replacementAmountAdjcountyID$IHP_replacementAmountAdjcountyID) #331523281 good


##Joining####
IHP_countyID1 <- left_join(countyID, IHP_AmountAdjcountyID, by= "countyID", copy= F)
IHP_countyID2 <- left_join(IHP_countyID1, IHP_haAmountAdjcountyID, by="countyID", copy=FALSE)
IHP_countyID3 <- left_join(IHP_countyID2,IHP_onaAmountAdjcountyID, by="countyID", copy=FALSE)
IHP_countyID4 <- left_join(IHP_countyID3, IHP_fldDamAmountAdjcountyID, by="countyID", copy=FALSE)
IHP_countyID5 <- left_join(IHP_countyID4, IHP_rpfvlAdjcountyID, by="countyID", copy=FALSE)
IHP_countyID6 <- left_join(IHP_countyID5, IHP_ppfvlAdjcountyID , by="countyID", copy=FALSE)
IHP_countyID7 <- left_join(IHP_countyID6, IHP_rentalAssistanceAmountAdjcountyID, by="countyID", copy=FALSE)
IHP_countyID8 <- left_join(IHP_countyID7, IHP_repairAmountAdjcountyID, by="countyID", copy=FALSE)
IHP_countyID9 <- left_join(IHP_countyID8, IHP_replacementAmountAdjcountyID, by="countyID", copy=FALSE)

names(IHP_countyID9)
colSums(is.na(IHP_countyID9))
head(IHP_countyID9) 


#writing out files#####
path1 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/CountyLevel'
write.csv(IHP_countyID9, file.path(path1, "County_FS_IHP.csv"), row.names=TRUE)

path2 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/CDNLevel'
write.csv(IHP_CDN9, file.path(path2, "CDN_FS_IHP.csv"), row.names=TRUE)


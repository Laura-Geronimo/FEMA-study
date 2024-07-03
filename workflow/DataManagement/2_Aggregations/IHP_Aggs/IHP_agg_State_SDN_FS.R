#Aggregating IHP data by State and SDN


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



###Aggregating Vars of interest by SDN####
##Creating unique SDN####
names(IHP_V10)
colSums(is.na(IHP_V10))
range(IHP_V10$stateFIPS)
IHP_V10$stateFIPS <- str_pad(IHP_V10$stateFIPS, 2, pad="0")
IHP_V10$stateID <- str_c("S",IHP_V10$stateFIPS)
head(IHP_V10$stateID)
range(IHP_V10$stateID)

IHP_V10$SDN <- str_c(IHP_V10$disasterNumber,IHP_V10$stateID)
head(IHP_V10$SDN)
SDN <- data.frame(unique(IHP_V10$SDN)) #362
names(SDN)
names(SDN)[names(SDN)=="unique.IHP_V10.SDN."] <- "SDN"


#Aggregating potential vars of interest by SDN####
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

#Aggregating sums by SDN
names(IHP_V10)
IHP_AmountAdjSDN <- aggregate(IHP_V10$ihpAmountAdj~IHP_V10$SDN, FUN=sum) 
IHP_haAmountAdjSDN <- aggregate(IHP_V10$haAmountAdj~IHP_V10$SDN, FUN=sum)
IHP_onaAmountAdjSDN <- aggregate(IHP_V10$onaAmountAdj~IHP_V10$SDN, FUN=sum)
IHP_fldDamAmountAdjSDN <- aggregate(IHP_V10$floodDamageAmountAdj~IHP_V10$SDN, FUN=sum) 
IHP_rpfvlAdjSDN <- aggregate(IHP_V10$rpfvlAdj~IHP_V10$SDN, FUN=sum) 
IHP_ppfvlAdjSDN <- aggregate(IHP_V10$ppfvlAdj~IHP_V10$SDN, FUN=sum) 
IHP_rentalAssistanceAmountAdjSDN <- aggregate(IHP_V10$rentalAssistanceAmountAdj~IHP_V10$SDN, FUN=sum) 
IHP_repairAmountAdjSDN <- aggregate(IHP_V10$repairAmountAdj~IHP_V10$SDN, FUN=sum) 
IHP_replacementAmountAdjSDN <- aggregate(IHP_V10$replacementAmountAdj~IHP_V10$SDN, FUN=sum) 


#IHP_AmountAdjSDN####
head(IHP_AmountAdjSDN)
names(IHP_AmountAdjSDN)[names(IHP_AmountAdjSDN)=="IHP_V10$SDN"] <- "SDN"
names(IHP_AmountAdjSDN)[names(IHP_AmountAdjSDN)=="IHP_V10$ihpAmountAdj"] <- "IHP_AmountAdjSDN"
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_AmountAdjSDN$IHP_AmountAdjSDN)#13925313982 good

#IHP_haAmountAdjSDN####
head(IHP_haAmountAdjSDN)
names(IHP_haAmountAdjSDN)[names(IHP_haAmountAdjSDN)=="IHP_V10$SDN"] <- "SDN"
names(IHP_haAmountAdjSDN)[names(IHP_haAmountAdjSDN)=="IHP_V10$haAmountAdj"] <- "IHP_haAmountAdjSDN"
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_haAmountAdjSDN$IHP_haAmountAdjSDN)#10775699067 good

#IHP_onaAmountAdjSDN####
head(IHP_onaAmountAdjSDN)
names(IHP_onaAmountAdjSDN)[names(IHP_onaAmountAdjSDN)=="IHP_V10$SDN"] <- "SDN"
names(IHP_onaAmountAdjSDN)[names(IHP_onaAmountAdjSDN)=="IHP_V10$onaAmountAdj"] <- "IHP_onaAmountAdjSDN"
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_onaAmountAdjSDN$IHP_onaAmountAdjSDN)#3149614917good

#IHP_fldDamAmountAdjSDN####
head(IHP_fldDamAmountAdjSDN)
names(IHP_fldDamAmountAdjSDN)[names(IHP_fldDamAmountAdjSDN)=="IHP_V10$SDN"] <- "SDN"
names(IHP_fldDamAmountAdjSDN)[names(IHP_fldDamAmountAdjSDN)=="IHP_V10$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjSDN"
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_fldDamAmountAdjSDN$IHP_fldDamAmountAdjSDN) #29262312328 good


#IHP_rpfvlAdjSDN####
head(IHP_rpfvlAdjSDN)
names(IHP_rpfvlAdjSDN)[names(IHP_rpfvlAdjSDN)=="IHP_V10$SDN"] <- "SDN"
names(IHP_rpfvlAdjSDN)[names(IHP_rpfvlAdjSDN)=="IHP_V10$rpfvlAdj"] <- "IHP_rpfvlAdjSDN"
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_rpfvlAdjSDN$IHP_rpfvlAdjSDN) #24635761202 good


#IHP_ppfvlAdjSDN####
head(IHP_ppfvlAdjSDN)
names(IHP_ppfvlAdjSDN)[names(IHP_ppfvlAdjSDN)=="IHP_V10$SDN"] <- "SDN"
names(IHP_ppfvlAdjSDN)[names(IHP_ppfvlAdjSDN)=="IHP_V10$ppfvlAdj"] <- "IHP_ppfvlAdjSDN"
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_ppfvlAdjSDN$IHP_ppfvlAdjSDN) #5902773648 good


#IHP_rentalAssistanceAmountAdjSDN####
head(IHP_rentalAssistanceAmountAdjSDN)
names(IHP_rentalAssistanceAmountAdjSDN)[names(IHP_rentalAssistanceAmountAdjSDN)=="IHP_V10$SDN"] <- "SDN"
names(IHP_rentalAssistanceAmountAdjSDN)[names(IHP_rentalAssistanceAmountAdjSDN)=="IHP_V10$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjSDN"
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_rentalAssistanceAmountAdjSDN$IHP_rentalAssistanceAmountAdjSDN) #3276879805 good

#IHP_repairAmountAdjSDN ####
head(IHP_repairAmountAdjSDN )
names(IHP_repairAmountAdjSDN )[names(IHP_repairAmountAdjSDN )=="IHP_V10$SDN"] <- "SDN"
names(IHP_repairAmountAdjSDN )[names(IHP_repairAmountAdjSDN )=="IHP_V10$repairAmountAdj"] <- "IHP_repairAmountAdjSDN"
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_repairAmountAdjSDN $IHP_repairAmountAdjSDN ) #6633263233 good

#IHP_replacementAmountAdjSDN ####
head(IHP_replacementAmountAdjSDN)
names(IHP_replacementAmountAdjSDN )[names(IHP_replacementAmountAdjSDN)=="IHP_V10$SDN"] <- "SDN"
names(IHP_replacementAmountAdjSDN )[names(IHP_replacementAmountAdjSDN)=="IHP_V10$replacementAmountAdj"] <- "IHP_replacementAmountAdjSDN"
sum(IHP_V10$replacementAmountAdj) #331523281
sum(IHP_replacementAmountAdjSDN$IHP_replacementAmountAdjSDN) #331523281 good


##Joining####
IHP_SDN1 <- left_join(SDN, IHP_AmountAdjSDN, by= "SDN", copy= F)
IHP_SDN2 <- left_join(IHP_SDN1, IHP_haAmountAdjSDN, by="SDN", copy=FALSE)
IHP_SDN3 <- left_join(IHP_SDN2,IHP_onaAmountAdjSDN, by="SDN", copy=FALSE)
IHP_SDN4 <- left_join(IHP_SDN3, IHP_fldDamAmountAdjSDN, by="SDN", copy=FALSE)
IHP_SDN5 <- left_join(IHP_SDN4, IHP_rpfvlAdjSDN, by="SDN", copy=FALSE)
IHP_SDN6 <- left_join(IHP_SDN5, IHP_ppfvlAdjSDN , by="SDN", copy=FALSE)
IHP_SDN7 <- left_join(IHP_SDN6, IHP_rentalAssistanceAmountAdjSDN, by="SDN", copy=FALSE)
IHP_SDN8 <- left_join(IHP_SDN7, IHP_repairAmountAdjSDN, by="SDN", copy=FALSE)
IHP_SDN9 <- left_join(IHP_SDN8, IHP_replacementAmountAdjSDN, by="SDN", copy=FALSE)

names(IHP_SDN9)
colSums(is.na(IHP_SDN9))
head(IHP_SDN9) 

#QC
sum(IHP_V10$ihpAmountAdj)
sum(IHP_SDN9$IHP_AmountAdjSDN)



#############################################
##Aggregating  by state
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

#creating stateID
names(IHP_V10)
head(IHP_V10$stateID)

#Creating unique stateID
stateID <- data.frame(unique(IHP_V10$stateID)) #49 - 
names(stateID)
names(stateID)[names(stateID)=="unique.IHP_V10.stateID."] <- "stateID"

#Aggregating sums by stateID
names(IHP_V10)
IHP_AmountAdjstateID <- aggregate(IHP_V10$ihpAmountAdj~IHP_V10$stateID, FUN=sum) 
IHP_haAmountAdjstateID <- aggregate(IHP_V10$haAmountAdj~IHP_V10$stateID, FUN=sum)
IHP_onaAmountAdjstateID <- aggregate(IHP_V10$onaAmountAdj~IHP_V10$stateID, FUN=sum)
IHP_fldDamAmountAdjstateID <- aggregate(IHP_V10$floodDamageAmountAdj~IHP_V10$stateID, FUN=sum) #14111 stateIDs
IHP_rpfvlAdjstateID <- aggregate(IHP_V10$rpfvlAdj~IHP_V10$stateID, FUN=sum) 
IHP_ppfvlAdjstateID <- aggregate(IHP_V10$ppfvlAdj~IHP_V10$stateID, FUN=sum) 
IHP_rentalAssistanceAmountAdjstateID <- aggregate(IHP_V10$rentalAssistanceAmountAdj~IHP_V10$stateID, FUN=sum) 
IHP_repairAmountAdjstateID <- aggregate(IHP_V10$repairAmountAdj~IHP_V10$stateID, FUN=sum) 
IHP_replacementAmountAdjstateID <- aggregate(IHP_V10$replacementAmountAdj~IHP_V10$stateID, FUN=sum) 


#IHP_AmountAdjstateID####
head(IHP_AmountAdjstateID)
names(IHP_AmountAdjstateID)[names(IHP_AmountAdjstateID)=="IHP_V10$stateID"] <- "stateID"
names(IHP_AmountAdjstateID)[names(IHP_AmountAdjstateID)=="IHP_V10$ihpAmountAdj"] <- "IHP_AmountAdjstateID"
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_AmountAdjstateID$IHP_AmountAdjstateID)#13925313982 good

#IHP_haAmountAdjstateID####
head(IHP_haAmountAdjstateID)
names(IHP_haAmountAdjstateID)[names(IHP_haAmountAdjstateID)=="IHP_V10$stateID"] <- "stateID"
names(IHP_haAmountAdjstateID)[names(IHP_haAmountAdjstateID)=="IHP_V10$haAmountAdj"] <- "IHP_haAmountAdjstateID"
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_haAmountAdjstateID$IHP_haAmountAdjstateID)#10775699067 good

#IHP_onaAmountAdjstateID####
head(IHP_onaAmountAdjstateID)
names(IHP_onaAmountAdjstateID)[names(IHP_onaAmountAdjstateID)=="IHP_V10$stateID"] <- "stateID"
names(IHP_onaAmountAdjstateID)[names(IHP_onaAmountAdjstateID)=="IHP_V10$onaAmountAdj"] <- "IHP_onaAmountAdjstateID"
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_onaAmountAdjstateID$IHP_onaAmountAdjstateID)#3149614917good

#IHP_fldDamAmountAdjstateID####
head(IHP_fldDamAmountAdjstateID)
names(IHP_fldDamAmountAdjstateID)[names(IHP_fldDamAmountAdjstateID)=="IHP_V10$stateID"] <- "stateID"
names(IHP_fldDamAmountAdjstateID)[names(IHP_fldDamAmountAdjstateID)=="IHP_V10$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjstateID"
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_fldDamAmountAdjstateID$IHP_fldDamAmountAdjstateID) #29262312328 good


#IHP_rpfvlAdjstateID####
head(IHP_rpfvlAdjstateID)
names(IHP_rpfvlAdjstateID)[names(IHP_rpfvlAdjstateID)=="IHP_V10$stateID"] <- "stateID"
names(IHP_rpfvlAdjstateID)[names(IHP_rpfvlAdjstateID)=="IHP_V10$rpfvlAdj"] <- "IHP_rpfvlAdjstateID"
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_rpfvlAdjstateID$IHP_rpfvlAdjstateID) #24635761202 good


#IHP_ppfvlAdjstateID####
head(IHP_ppfvlAdjstateID)
names(IHP_ppfvlAdjstateID)[names(IHP_ppfvlAdjstateID)=="IHP_V10$stateID"] <- "stateID"
names(IHP_ppfvlAdjstateID)[names(IHP_ppfvlAdjstateID)=="IHP_V10$ppfvlAdj"] <- "IHP_ppfvlAdjstateID"
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_ppfvlAdjstateID$IHP_ppfvlAdjstateID) #5902773648 good


#IHP_rentalAssistanceAmountAdjstateID####
head(IHP_rentalAssistanceAmountAdjstateID)
names(IHP_rentalAssistanceAmountAdjstateID)[names(IHP_rentalAssistanceAmountAdjstateID)=="IHP_V10$stateID"] <- "stateID"
names(IHP_rentalAssistanceAmountAdjstateID)[names(IHP_rentalAssistanceAmountAdjstateID)=="IHP_V10$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjstateID"
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_rentalAssistanceAmountAdjstateID$IHP_rentalAssistanceAmountAdjstateID) #3276879805 good

#IHP_repairAmountAdjstateID ####
head(IHP_repairAmountAdjstateID )
names(IHP_repairAmountAdjstateID )[names(IHP_repairAmountAdjstateID )=="IHP_V10$stateID"] <- "stateID"
names(IHP_repairAmountAdjstateID )[names(IHP_repairAmountAdjstateID )=="IHP_V10$repairAmountAdj"] <- "IHP_repairAmountAdjstateID"
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_repairAmountAdjstateID $IHP_repairAmountAdjstateID ) #6633263233 good

#IHP_replacementAmountAdjstateID ####
head(IHP_replacementAmountAdjstateID)
names(IHP_replacementAmountAdjstateID )[names(IHP_replacementAmountAdjstateID)=="IHP_V10$stateID"] <- "stateID"
names(IHP_replacementAmountAdjstateID )[names(IHP_replacementAmountAdjstateID)=="IHP_V10$replacementAmountAdj"] <- "IHP_replacementAmountAdjstateID"
sum(IHP_V10$replacementAmountAdj) #331523281
sum(IHP_replacementAmountAdjstateID$IHP_replacementAmountAdjstateID) #331523281 good


##Joining####
IHP_stateID1 <- left_join(stateID, IHP_AmountAdjstateID, by= "stateID", copy= F)
IHP_stateID2 <- left_join(IHP_stateID1, IHP_haAmountAdjstateID, by="stateID", copy=FALSE)
IHP_stateID3 <- left_join(IHP_stateID2,IHP_onaAmountAdjstateID, by="stateID", copy=FALSE)
IHP_stateID4 <- left_join(IHP_stateID3, IHP_fldDamAmountAdjstateID, by="stateID", copy=FALSE)
IHP_stateID5 <- left_join(IHP_stateID4, IHP_rpfvlAdjstateID, by="stateID", copy=FALSE)
IHP_stateID6 <- left_join(IHP_stateID5, IHP_ppfvlAdjstateID , by="stateID", copy=FALSE)
IHP_stateID7 <- left_join(IHP_stateID6, IHP_rentalAssistanceAmountAdjstateID, by="stateID", copy=FALSE)
IHP_stateID8 <- left_join(IHP_stateID7, IHP_repairAmountAdjstateID, by="stateID", copy=FALSE)
IHP_stateID9 <- left_join(IHP_stateID8, IHP_replacementAmountAdjstateID, by="stateID", copy=FALSE)

names(IHP_stateID9)
colSums(is.na(IHP_stateID9))
head(IHP_stateID9) 

#QC
sum(IHP_V10$ihpAmountAdj)
sum(IHP_stateID9$IHP_AmountAdjstateID)

sum(IHP_V10$floodDamageAmountAdj)
sum(IHP_stateID9$IHP_fldDamAmountAdjstateID)

#writing out files#####
path1 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/StateLevel'
write.csv(IHP_stateID9, file.path(path1, "State_FS_IHP.csv"), row.names=TRUE)

path2 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/SDNLevel'
write.csv(IHP_SDN9, file.path(path2, "SDN_FS_IHP.csv"), row.names=TRUE)


#Aggregating IHP data by GZCTA and DNZ


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



###Aggregating Vars of interest by DNZ####
##Creating unique DNZ####

IHP_V10$DNZ <- str_c(IHP_V10$disasterNumber,IHP_V10$GZCTA)
head(IHP_V10$DNZ)
DNZ <- data.frame(unique(IHP_V10$DNZ)) #40442 - this is going to be your new universe -ALL zip codes impacted by flood events
names(DNZ)
names(DNZ)[names(DNZ)=="unique.IHP_V10.DNZ."] <- "DNZ"


#Aggregating potential vars of interest by DNZ####
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

#Aggregating sums by DNZ
names(IHP_V10)
IHP_AmountAdjDNZ <- aggregate(IHP_V10$ihpAmountAdj~IHP_V10$DNZ, FUN=sum) 
IHP_haAmountAdjDNZ <- aggregate(IHP_V10$haAmountAdj~IHP_V10$DNZ, FUN=sum)
IHP_onaAmountAdjDNZ <- aggregate(IHP_V10$onaAmountAdj~IHP_V10$DNZ, FUN=sum)
IHP_fldDamAmountAdjDNZ <- aggregate(IHP_V10$floodDamageAmountAdj~IHP_V10$DNZ, FUN=sum) 
IHP_rpfvlAdjDNZ <- aggregate(IHP_V10$rpfvlAdj~IHP_V10$DNZ, FUN=sum) 
IHP_ppfvlAdjDNZ <- aggregate(IHP_V10$ppfvlAdj~IHP_V10$DNZ, FUN=sum) 
IHP_rentalAssistanceAmountAdjDNZ <- aggregate(IHP_V10$rentalAssistanceAmountAdj~IHP_V10$DNZ, FUN=sum) 
IHP_repairAmountAdjDNZ <- aggregate(IHP_V10$repairAmountAdj~IHP_V10$DNZ, FUN=sum) 
IHP_replacementAmountAdjDNZ <- aggregate(IHP_V10$replacementAmountAdj~IHP_V10$DNZ, FUN=sum) 


#IHP_AmountAdjDNZ####
head(IHP_AmountAdjDNZ)
names(IHP_AmountAdjDNZ)[names(IHP_AmountAdjDNZ)=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_AmountAdjDNZ)[names(IHP_AmountAdjDNZ)=="IHP_V10$ihpAmountAdj"] <- "IHP_AmountAdjDNZ"
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_AmountAdjDNZ$IHP_AmountAdjDNZ)#13925313982 good

#IHP_haAmountAdjDNZ####
head(IHP_haAmountAdjDNZ)
names(IHP_haAmountAdjDNZ)[names(IHP_haAmountAdjDNZ)=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_haAmountAdjDNZ)[names(IHP_haAmountAdjDNZ)=="IHP_V10$haAmountAdj"] <- "IHP_haAmountAdjDNZ"
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_haAmountAdjDNZ$IHP_haAmountAdjDNZ)#10775699067 good

#IHP_onaAmountAdjDNZ####
head(IHP_onaAmountAdjDNZ)
names(IHP_onaAmountAdjDNZ)[names(IHP_onaAmountAdjDNZ)=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_onaAmountAdjDNZ)[names(IHP_onaAmountAdjDNZ)=="IHP_V10$onaAmountAdj"] <- "IHP_onaAmountAdjDNZ"
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_onaAmountAdjDNZ$IHP_onaAmountAdjDNZ)#3149614917good

#IHP_fldDamAmountAdjDNZ####
head(IHP_fldDamAmountAdjDNZ)
names(IHP_fldDamAmountAdjDNZ)[names(IHP_fldDamAmountAdjDNZ)=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_fldDamAmountAdjDNZ)[names(IHP_fldDamAmountAdjDNZ)=="IHP_V10$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjDNZ"
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_fldDamAmountAdjDNZ$IHP_fldDamAmountAdjDNZ) #29262312328 good


#IHP_rpfvlAdjDNZ####
head(IHP_rpfvlAdjDNZ)
names(IHP_rpfvlAdjDNZ)[names(IHP_rpfvlAdjDNZ)=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_rpfvlAdjDNZ)[names(IHP_rpfvlAdjDNZ)=="IHP_V10$rpfvlAdj"] <- "IHP_rpfvlAdjDNZ"
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_rpfvlAdjDNZ$IHP_rpfvlAdjDNZ) #24635761202 good


#IHP_ppfvlAdjDNZ####
head(IHP_ppfvlAdjDNZ)
names(IHP_ppfvlAdjDNZ)[names(IHP_ppfvlAdjDNZ)=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_ppfvlAdjDNZ)[names(IHP_ppfvlAdjDNZ)=="IHP_V10$ppfvlAdj"] <- "IHP_ppfvlAdjDNZ"
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_ppfvlAdjDNZ$IHP_ppfvlAdjDNZ) #5902773648 good


#IHP_rentalAssistanceAmountAdjDNZ####
head(IHP_rentalAssistanceAmountAdjDNZ)
names(IHP_rentalAssistanceAmountAdjDNZ)[names(IHP_rentalAssistanceAmountAdjDNZ)=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_rentalAssistanceAmountAdjDNZ)[names(IHP_rentalAssistanceAmountAdjDNZ)=="IHP_V10$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjDNZ"
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_rentalAssistanceAmountAdjDNZ$IHP_rentalAssistanceAmountAdjDNZ) #3276879805 good

#IHP_repairAmountAdjDNZ ####
head(IHP_repairAmountAdjDNZ )
names(IHP_repairAmountAdjDNZ )[names(IHP_repairAmountAdjDNZ )=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_repairAmountAdjDNZ )[names(IHP_repairAmountAdjDNZ )=="IHP_V10$repairAmountAdj"] <- "IHP_repairAmountAdjDNZ"
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_repairAmountAdjDNZ $IHP_repairAmountAdjDNZ ) #6633263233 good

#IHP_replacementAmountAdjDNZ ####
head(IHP_replacementAmountAdjDNZ)
names(IHP_replacementAmountAdjDNZ )[names(IHP_replacementAmountAdjDNZ)=="IHP_V10$DNZ"] <- "DNZ"
names(IHP_replacementAmountAdjDNZ )[names(IHP_replacementAmountAdjDNZ)=="IHP_V10$replacementAmountAdj"] <- "IHP_replacementAmountAdjDNZ"
sum(IHP_V10$replacementAmountAdj) #331523281
sum(IHP_replacementAmountAdjDNZ$IHP_replacementAmountAdjDNZ) #331523281 good


##Joining####
IHP_DNZ1 <- left_join(DNZ, IHP_AmountAdjDNZ, by= "DNZ", copy= F)
IHP_DNZ2 <- left_join(IHP_DNZ1, IHP_haAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_DNZ3 <- left_join(IHP_DNZ2,IHP_onaAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_DNZ4 <- left_join(IHP_DNZ3, IHP_fldDamAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_DNZ5 <- left_join(IHP_DNZ4, IHP_rpfvlAdjDNZ, by="DNZ", copy=FALSE)
IHP_DNZ6 <- left_join(IHP_DNZ5, IHP_ppfvlAdjDNZ , by="DNZ", copy=FALSE)
IHP_DNZ7 <- left_join(IHP_DNZ6, IHP_rentalAssistanceAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_DNZ8 <- left_join(IHP_DNZ7, IHP_repairAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_DNZ9 <- left_join(IHP_DNZ8, IHP_replacementAmountAdjDNZ, by="DNZ", copy=FALSE)

names(IHP_DNZ9)
colSums(is.na(IHP_DNZ9))
head(IHP_DNZ9) #40442 DNZ



#############################################
##Aggregating  by GZCTA
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

#creating GZCTA
names(IHP_V10)
head(IHP_V10$GZCTA)

#Creating unique GZCTA
GZCTA <- data.frame(unique(IHP_V10$GZCTA)) #18372 - 
names(GZCTA)
names(GZCTA)[names(GZCTA)=="unique.IHP_V10.GZCTA."] <- "GZCTA"

#Aggregating sums by GZCTA
names(IHP_V10)
IHP_AmountAdjGZCTA <- aggregate(IHP_V10$ihpAmountAdj~IHP_V10$GZCTA, FUN=sum) 
IHP_haAmountAdjGZCTA <- aggregate(IHP_V10$haAmountAdj~IHP_V10$GZCTA, FUN=sum)
IHP_onaAmountAdjGZCTA <- aggregate(IHP_V10$onaAmountAdj~IHP_V10$GZCTA, FUN=sum)
IHP_fldDamAmountAdjGZCTA <- aggregate(IHP_V10$floodDamageAmountAdj~IHP_V10$GZCTA, FUN=sum) #14111 GZCTAs
IHP_rpfvlAdjGZCTA <- aggregate(IHP_V10$rpfvlAdj~IHP_V10$GZCTA, FUN=sum) 
IHP_ppfvlAdjGZCTA <- aggregate(IHP_V10$ppfvlAdj~IHP_V10$GZCTA, FUN=sum) 
IHP_rentalAssistanceAmountAdjGZCTA <- aggregate(IHP_V10$rentalAssistanceAmountAdj~IHP_V10$GZCTA, FUN=sum) 
IHP_repairAmountAdjGZCTA <- aggregate(IHP_V10$repairAmountAdj~IHP_V10$GZCTA, FUN=sum) 
IHP_replacementAmountAdjGZCTA <- aggregate(IHP_V10$replacementAmountAdj~IHP_V10$GZCTA, FUN=sum) 


#IHP_AmountAdjGZCTA####
head(IHP_AmountAdjGZCTA)
names(IHP_AmountAdjGZCTA)[names(IHP_AmountAdjGZCTA)=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_AmountAdjGZCTA)[names(IHP_AmountAdjGZCTA)=="IHP_V10$ihpAmountAdj"] <- "IHP_AmountAdjGZCTA"
sum(IHP_V10$ihpAmountAdj)#13925313982
sum(IHP_AmountAdjGZCTA$IHP_AmountAdjGZCTA)#13925313982 good

#IHP_haAmountAdjGZCTA####
head(IHP_haAmountAdjGZCTA)
names(IHP_haAmountAdjGZCTA)[names(IHP_haAmountAdjGZCTA)=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_haAmountAdjGZCTA)[names(IHP_haAmountAdjGZCTA)=="IHP_V10$haAmountAdj"] <- "IHP_haAmountAdjGZCTA"
sum(IHP_V10$haAmountAdj) #10775699067
sum(IHP_haAmountAdjGZCTA$IHP_haAmountAdjGZCTA)#10775699067 good

#IHP_onaAmountAdjGZCTA####
head(IHP_onaAmountAdjGZCTA)
names(IHP_onaAmountAdjGZCTA)[names(IHP_onaAmountAdjGZCTA)=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_onaAmountAdjGZCTA)[names(IHP_onaAmountAdjGZCTA)=="IHP_V10$onaAmountAdj"] <- "IHP_onaAmountAdjGZCTA"
sum(IHP_V10$onaAmountAdj) #3149614917
sum(IHP_onaAmountAdjGZCTA$IHP_onaAmountAdjGZCTA)#3149614917good

#IHP_fldDamAmountAdjGZCTA####
head(IHP_fldDamAmountAdjGZCTA)
names(IHP_fldDamAmountAdjGZCTA)[names(IHP_fldDamAmountAdjGZCTA)=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_fldDamAmountAdjGZCTA)[names(IHP_fldDamAmountAdjGZCTA)=="IHP_V10$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjGZCTA"
sum(IHP_V10$floodDamageAmountAdj) #29262312328
sum(IHP_fldDamAmountAdjGZCTA$IHP_fldDamAmountAdjGZCTA) #29262312328 good


#IHP_rpfvlAdjGZCTA####
head(IHP_rpfvlAdjGZCTA)
names(IHP_rpfvlAdjGZCTA)[names(IHP_rpfvlAdjGZCTA)=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_rpfvlAdjGZCTA)[names(IHP_rpfvlAdjGZCTA)=="IHP_V10$rpfvlAdj"] <- "IHP_rpfvlAdjGZCTA"
sum(IHP_V10$rpfvlAdj) #24635761202
sum(IHP_rpfvlAdjGZCTA$IHP_rpfvlAdjGZCTA) #24635761202 good


#IHP_ppfvlAdjGZCTA####
head(IHP_ppfvlAdjGZCTA)
names(IHP_ppfvlAdjGZCTA)[names(IHP_ppfvlAdjGZCTA)=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_ppfvlAdjGZCTA)[names(IHP_ppfvlAdjGZCTA)=="IHP_V10$ppfvlAdj"] <- "IHP_ppfvlAdjGZCTA"
sum(IHP_V10$ppfvlAdj) #5902773648
sum(IHP_ppfvlAdjGZCTA$IHP_ppfvlAdjGZCTA) #5902773648 good


#IHP_rentalAssistanceAmountAdjGZCTA####
head(IHP_rentalAssistanceAmountAdjGZCTA)
names(IHP_rentalAssistanceAmountAdjGZCTA)[names(IHP_rentalAssistanceAmountAdjGZCTA)=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_rentalAssistanceAmountAdjGZCTA)[names(IHP_rentalAssistanceAmountAdjGZCTA)=="IHP_V10$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjGZCTA"
sum(IHP_V10$rentalAssistanceAmountAdj) #3276879805
sum(IHP_rentalAssistanceAmountAdjGZCTA$IHP_rentalAssistanceAmountAdjGZCTA) #3276879805 good

#IHP_repairAmountAdjGZCTA ####
head(IHP_repairAmountAdjGZCTA )
names(IHP_repairAmountAdjGZCTA )[names(IHP_repairAmountAdjGZCTA )=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_repairAmountAdjGZCTA )[names(IHP_repairAmountAdjGZCTA )=="IHP_V10$repairAmountAdj"] <- "IHP_repairAmountAdjGZCTA"
sum(IHP_V10$repairAmountAdj) #6633263233
sum(IHP_repairAmountAdjGZCTA $IHP_repairAmountAdjGZCTA ) #6633263233 good

#IHP_replacementAmountAdjGZCTA ####
head(IHP_replacementAmountAdjGZCTA)
names(IHP_replacementAmountAdjGZCTA )[names(IHP_replacementAmountAdjGZCTA)=="IHP_V10$GZCTA"] <- "GZCTA"
names(IHP_replacementAmountAdjGZCTA )[names(IHP_replacementAmountAdjGZCTA)=="IHP_V10$replacementAmountAdj"] <- "IHP_replacementAmountAdjGZCTA"
sum(IHP_V10$replacementAmountAdj) #331523281
sum(IHP_replacementAmountAdjGZCTA$IHP_replacementAmountAdjGZCTA) #331523281 good


##Joining####
IHP_GZCTA1 <- left_join(GZCTA, IHP_AmountAdjGZCTA, by= "GZCTA", copy= F)
IHP_GZCTA2 <- left_join(IHP_GZCTA1, IHP_haAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_GZCTA3 <- left_join(IHP_GZCTA2,IHP_onaAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_GZCTA4 <- left_join(IHP_GZCTA3, IHP_fldDamAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_GZCTA5 <- left_join(IHP_GZCTA4, IHP_rpfvlAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_GZCTA6 <- left_join(IHP_GZCTA5, IHP_ppfvlAdjGZCTA , by="GZCTA", copy=FALSE)
IHP_GZCTA7 <- left_join(IHP_GZCTA6, IHP_rentalAssistanceAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_GZCTA8 <- left_join(IHP_GZCTA7, IHP_repairAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_GZCTA9 <- left_join(IHP_GZCTA8, IHP_replacementAmountAdjGZCTA, by="GZCTA", copy=FALSE)

names(IHP_GZCTA9)
colSums(is.na(IHP_GZCTA9))
head(IHP_GZCTA9) 


#writing out files#####
path1 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/ZCTALevel'
write.csv(IHP_GZCTA9, file.path(path1, "ZCTA_FS_IHP.csv"), row.names=TRUE)

path2 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS/DNZLevel'
write.csv(IHP_DNZ9, file.path(path2, "DNZ_FS_IHP.csv"), row.names=TRUE)


#Exploring IA
#Consider coming back and grabbing more data by DNZ
#NOTE: countyNum does not exist, so aggregating by GZCTA and DNZ

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


###Aggregating Vars of interest by DNZ####
##Creating unique DNZ####

IHP_V6$DNZ <- str_c(IHP_V6$disasterNumber,IHP_V6$GZCTA)
head(IHP_V6$DNZ)
DNZ <- data.frame(unique(IHP_V6$DNZ)) #42248 - this is going to be your new universe - zip codes impacted by flood events
names(DNZ)
names(DNZ)[names(DNZ)=="unique.IHP_V6.DNZ."] <- "DNZ"


#Aggregating potential vars of interest by DNZ####
sum(IHP_V6$ihpAmountAdj)#14018466822
sum(IHP_V6$haAmountAdj) #10838650752
sum(IHP_V6$onaAmountAdj) #3179816071
sum(IHP_V6$floodDamageAmountAdj) #29419330736
sum(IHP_V6$rpfvlAdj) #24760661306
sum(IHP_V6$ppfvlAdj) #5947500567
sum(IHP_V6$rentalAssistanceAmountAdj) #3289467883
sum(IHP_V6$repairAmountAdj) #6674197323
sum(IHP_V6$replacementAmountAdj) #336952640

#Aggregating sums by DNZ
names(IHP_V6)
IHP_AmountAdjDNZ <- aggregate(IHP_V6$ihpAmountAdj~IHP_V6$DNZ, FUN=sum) 
IHP_haAmountAdjDNZ <- aggregate(IHP_V6$haAmountAdj~IHP_V6$DNZ, FUN=sum)
IHP_onaAmountAdjDNZ <- aggregate(IHP_V6$onaAmountAdj~IHP_V6$DNZ, FUN=sum)
IHP_fldDamAmountAdjDNZ <- aggregate(IHP_V6$floodDamageAmountAdj~IHP_V6$DNZ, FUN=sum) #14111 DNZs
IHP_rpfvlAdjDNZ <- aggregate(IHP_V6$rpfvlAdj~IHP_V6$DNZ, FUN=sum) 
IHP_ppfvlAdjDNZ <- aggregate(IHP_V6$ppfvlAdj~IHP_V6$DNZ, FUN=sum) 
IHP_rentalAssistanceAmountAdjDNZ <- aggregate(IHP_V6$rentalAssistanceAmountAdj~IHP_V6$DNZ, FUN=sum) 
IHP_repairAmountAdjDNZ <- aggregate(IHP_V6$repairAmountAdj~IHP_V6$DNZ, FUN=sum) 
IHP_replacementAmountAdjDNZ <- aggregate(IHP_V6$replacementAmountAdj~IHP_V6$DNZ, FUN=sum) 


#IHP_AmountAdjDNZ####
head(IHP_AmountAdjDNZ)
names(IHP_AmountAdjDNZ)[names(IHP_AmountAdjDNZ)=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_AmountAdjDNZ)[names(IHP_AmountAdjDNZ)=="IHP_V6$ihpAmountAdj"] <- "IHP_AmountAdjDNZ"
sum(IHP_V6$ihpAmountAdj)#14018466822
sum(IHP_AmountAdjDNZ$IHP_AmountAdjDNZ)#14018466822 good

#IHP_haAmountAdjDNZ####
head(IHP_haAmountAdjDNZ)
names(IHP_haAmountAdjDNZ)[names(IHP_haAmountAdjDNZ)=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_haAmountAdjDNZ)[names(IHP_haAmountAdjDNZ)=="IHP_V6$haAmountAdj"] <- "IHP_haAmountAdjDNZ"
sum(IHP_V6$haAmountAdj) #10838650752
sum(IHP_haAmountAdjDNZ$IHP_haAmountAdjDNZ)#10838650752 good

#IHP_onaAmountAdjDNZ####
head(IHP_onaAmountAdjDNZ)
names(IHP_onaAmountAdjDNZ)[names(IHP_onaAmountAdjDNZ)=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_onaAmountAdjDNZ)[names(IHP_onaAmountAdjDNZ)=="IHP_V6$onaAmountAdj"] <- "IHP_onaAmountAdjDNZ"
sum(IHP_V6$onaAmountAdj) #3179816071
sum(IHP_onaAmountAdjDNZ$IHP_onaAmountAdjDNZ)#3179816071 good

#IHP_fldDamAmountAdjDNZ####
head(IHP_fldDamAmountAdjDNZ)
names(IHP_fldDamAmountAdjDNZ)[names(IHP_fldDamAmountAdjDNZ)=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_fldDamAmountAdjDNZ)[names(IHP_fldDamAmountAdjDNZ)=="IHP_V6$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjDNZ"
sum(IHP_V6$floodDamageAmountAdj) #29419330736
sum(IHP_fldDamAmountAdjDNZ$IHP_fldDamAmountAdjDNZ) #29419330736 good


#IHP_rpfvlAdjDNZ####
head(IHP_rpfvlAdjDNZ)
names(IHP_rpfvlAdjDNZ)[names(IHP_rpfvlAdjDNZ)=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_rpfvlAdjDNZ)[names(IHP_rpfvlAdjDNZ)=="IHP_V6$rpfvlAdj"] <- "IHP_rpfvlAdjDNZ"
sum(IHP_V6$rpfvlAdj) #24760661306
sum(IHP_rpfvlAdjDNZ$IHP_rpfvlAdjDNZ) #24760661306 good


#IHP_ppfvlAdjDNZ####
head(IHP_ppfvlAdjDNZ)
names(IHP_ppfvlAdjDNZ)[names(IHP_ppfvlAdjDNZ)=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_ppfvlAdjDNZ)[names(IHP_ppfvlAdjDNZ)=="IHP_V6$ppfvlAdj"] <- "IHP_ppfvlAdjDNZ"
sum(IHP_V6$ppfvlAdj) #5947500567
sum(IHP_ppfvlAdjDNZ$IHP_ppfvlAdjDNZ) #5947500567 good


#IHP_rentalAssistanceAmountAdjDNZ####
head(IHP_rentalAssistanceAmountAdjDNZ)
names(IHP_rentalAssistanceAmountAdjDNZ)[names(IHP_rentalAssistanceAmountAdjDNZ)=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_rentalAssistanceAmountAdjDNZ)[names(IHP_rentalAssistanceAmountAdjDNZ)=="IHP_V6$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjDNZ"
sum(IHP_V6$rentalAssistanceAmountAdj) #3289467883
sum(IHP_rentalAssistanceAmountAdjDNZ$IHP_rentalAssistanceAmountAdjDNZ) #3289467883 good

#IHP_repairAmountAdjDNZ ####
head(IHP_repairAmountAdjDNZ )
names(IHP_repairAmountAdjDNZ )[names(IHP_repairAmountAdjDNZ )=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_repairAmountAdjDNZ )[names(IHP_repairAmountAdjDNZ )=="IHP_V6$repairAmountAdj"] <- "IHP_repairAmountAdjDNZ"
sum(IHP_V6$repairAmountAdj) #6674197323
sum(IHP_repairAmountAdjDNZ $IHP_repairAmountAdjDNZ ) #6674197323 good

#IHP_replacementAmountAdjDNZ ####
head(IHP_replacementAmountAdjDNZ )
names(IHP_replacementAmountAdjDNZ )[names(IHP_replacementAmountAdjDNZ )=="IHP_V6$DNZ"] <- "DNZ"
names(IHP_replacementAmountAdjDNZ )[names(IHP_replacementAmountAdjDNZ )=="IHP_V6$replacementAmountAdj"] <- "IHP_replacementAmountAdjDNZ"
sum(IHP_V6$replacementAmountAdj) #336952640
sum(IHP_replacementAmountAdjDNZ $IHP_replacementAmountAdjDNZ ) #336952640good


##Joining####
IHP_V9a <- left_join(IHP_AmountAdjDNZ, IHP_haAmountAdjDNZ, by="DNZ", copy=FALSE)
names(IHP_V6)
IHP_V9 <- left_join(IHP_V9a,IHP_onaAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_V10 <- left_join(IHP_V9, IHP_fldDamAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_V11 <- left_join(IHP_V10, IHP_rpfvlAdjDNZ, by="DNZ", copy=FALSE)
IHP_V12 <- left_join(IHP_V11, IHP_ppfvlAdjDNZ , by="DNZ", copy=FALSE)
IHP_V13 <- left_join(IHP_V12, IHP_rentalAssistanceAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_V14 <- left_join(IHP_V13, IHP_repairAmountAdjDNZ, by="DNZ", copy=FALSE)
IHP_V15 <- left_join(IHP_V14, IHP_replacementAmountAdjDNZ, by="DNZ", copy=FALSE)

names(IHP_V15)
colSums(is.na(IHP_V15))
head(IHP_V15) #42248 DNZ
dim(IHP_V15)
dim(IHP_V6)


#############################################
##Aggregating  by GZCTA

#creating GZCTA
range(IHP_V6$GZCTA)

#QC
sum(IHP_V6$ihpAmountAdj)#14018466822
sum(IHP_V6$haAmountAdj) #10838650752
sum(IHP_V6$onaAmountAdj) #3179816071
sum(IHP_V6$floodDamageAmountAdj) #29419330736
sum(IHP_V6$rpfvlAdj) #24760661306
sum(IHP_V6$ppfvlAdj) #5947500567
sum(IHP_V6$rentalAssistanceAmountAdj) #3289467883
sum(IHP_V6$repairAmountAdj) #6674197323
sum(IHP_V6$replacementAmountAdj) #336952640

#Aggregating sums by GZCTA
names(IHP_V6)
IHP_AmountAdjGZCTA <- aggregate(IHP_V6$ihpAmountAdj~IHP_V6$GZCTA, FUN=sum) 
IHP_haAmountAdjGZCTA <- aggregate(IHP_V6$haAmountAdj~IHP_V6$GZCTA, FUN=sum)
IHP_onaAmountAdjGZCTA <- aggregate(IHP_V6$onaAmountAdj~IHP_V6$GZCTA, FUN=sum)
IHP_fldDamAmountAdjGZCTA <- aggregate(IHP_V6$floodDamageAmountAdj~IHP_V6$GZCTA, FUN=sum) #14111 GZCTAs
IHP_rpfvlAdjGZCTA <- aggregate(IHP_V6$rpfvlAdj~IHP_V6$GZCTA, FUN=sum) 
IHP_ppfvlAdjGZCTA <- aggregate(IHP_V6$ppfvlAdj~IHP_V6$GZCTA, FUN=sum) 
IHP_rentalAssistanceAmountAdjGZCTA <- aggregate(IHP_V6$rentalAssistanceAmountAdj~IHP_V6$GZCTA, FUN=sum) 
IHP_repairAmountAdjGZCTA <- aggregate(IHP_V6$repairAmountAdj~IHP_V6$GZCTA, FUN=sum) 
IHP_replacementAmountAdjGZCTA <- aggregate(IHP_V6$replacementAmountAdj~IHP_V6$GZCTA, FUN=sum) 


#IHP_AmountAdjGZCTA####
head(IHP_AmountAdjGZCTA)
names(IHP_AmountAdjGZCTA)[names(IHP_AmountAdjGZCTA)=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_AmountAdjGZCTA)[names(IHP_AmountAdjGZCTA)=="IHP_V6$ihpAmountAdj"] <- "IHP_AmountAdjGZCTA"
sum(IHP_V6$ihpAmountAdj)#14018466822
sum(IHP_AmountAdjGZCTA$IHP_AmountAdjGZCTA)#14018466822 good

#IHP_haAmountAdjGZCTA####
head(IHP_haAmountAdjGZCTA)
names(IHP_haAmountAdjGZCTA)[names(IHP_haAmountAdjGZCTA)=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_haAmountAdjGZCTA)[names(IHP_haAmountAdjGZCTA)=="IHP_V6$haAmountAdj"] <- "IHP_haAmountAdjGZCTA"
sum(IHP_V6$haAmountAdj) #10838650752
sum(IHP_haAmountAdjGZCTA$IHP_haAmountAdjGZCTA)#10838650752 good

#IHP_onaAmountAdjGZCTA####
head(IHP_onaAmountAdjGZCTA)
names(IHP_onaAmountAdjGZCTA)[names(IHP_onaAmountAdjGZCTA)=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_onaAmountAdjGZCTA)[names(IHP_onaAmountAdjGZCTA)=="IHP_V6$onaAmountAdj"] <- "IHP_onaAmountAdjGZCTA"
sum(IHP_V6$onaAmountAdj) #3179816071
sum(IHP_onaAmountAdjGZCTA$IHP_onaAmountAdjGZCTA)#3179816071 good

#IHP_fldDamAmountAdjGZCTA####
head(IHP_fldDamAmountAdjGZCTA)
names(IHP_fldDamAmountAdjGZCTA)[names(IHP_fldDamAmountAdjGZCTA)=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_fldDamAmountAdjGZCTA)[names(IHP_fldDamAmountAdjGZCTA)=="IHP_V6$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjGZCTA"
sum(IHP_V6$floodDamageAmountAdj) #29419330736
sum(IHP_fldDamAmountAdjGZCTA$IHP_fldDamAmountAdjGZCTA) #29419330736 good


#IHP_rpfvlAdjGZCTA####
head(IHP_rpfvlAdjGZCTA)
names(IHP_rpfvlAdjGZCTA)[names(IHP_rpfvlAdjGZCTA)=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_rpfvlAdjGZCTA)[names(IHP_rpfvlAdjGZCTA)=="IHP_V6$rpfvlAdj"] <- "IHP_rpfvlAdjGZCTA"
sum(IHP_V6$rpfvlAdj) #24760661306
sum(IHP_rpfvlAdjGZCTA$IHP_rpfvlAdjGZCTA) #24760661306 good


#IHP_ppfvlAdjGZCTA####
head(IHP_ppfvlAdjGZCTA)
names(IHP_ppfvlAdjGZCTA)[names(IHP_ppfvlAdjGZCTA)=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_ppfvlAdjGZCTA)[names(IHP_ppfvlAdjGZCTA)=="IHP_V6$ppfvlAdj"] <- "IHP_ppfvlAdjGZCTA"
sum(IHP_V6$ppfvlAdj) #5947500567
sum(IHP_ppfvlAdjGZCTA$IHP_ppfvlAdjGZCTA) #5947500567 good


#IHP_rentalAssistanceAmountAdjGZCTA####
head(IHP_rentalAssistanceAmountAdjGZCTA)
names(IHP_rentalAssistanceAmountAdjGZCTA)[names(IHP_rentalAssistanceAmountAdjGZCTA)=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_rentalAssistanceAmountAdjGZCTA)[names(IHP_rentalAssistanceAmountAdjGZCTA)=="IHP_V6$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjGZCTA"
sum(IHP_V6$rentalAssistanceAmountAdj) #3289467883
sum(IHP_rentalAssistanceAmountAdjGZCTA$IHP_rentalAssistanceAmountAdjGZCTA) #3289467883 good

#IHP_repairAmountAdjGZCTA ####
head(IHP_repairAmountAdjGZCTA )
names(IHP_repairAmountAdjGZCTA )[names(IHP_repairAmountAdjGZCTA )=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_repairAmountAdjGZCTA )[names(IHP_repairAmountAdjGZCTA )=="IHP_V6$repairAmountAdj"] <- "IHP_repairAmountAdjGZCTA"
sum(IHP_V6$repairAmountAdj) #6674197323
sum(IHP_repairAmountAdjGZCTA $IHP_repairAmountAdjGZCTA ) #6674197323 good

#IHP_replacementAmountAdjGZCTA ####
head(IHP_replacementAmountAdjGZCTA)
names(IHP_replacementAmountAdjGZCTA )[names(IHP_replacementAmountAdjGZCTA)=="IHP_V6$GZCTA"] <- "GZCTA"
names(IHP_replacementAmountAdjGZCTA )[names(IHP_replacementAmountAdjGZCTA)=="IHP_V6$replacementAmountAdj"] <- "IHP_replacementAmountAdjGZCTA"
sum(IHP_V6$replacementAmountAdj) #336952640
sum(IHP_replacementAmountAdjGZCTA$IHP_replacementAmountAdjGZCTA) #336952640 good



##Joining####
IHP_G1 <- left_join(IHP_AmountAdjGZCTA, IHP_haAmountAdjGZCTA, by="GZCTA", copy=FALSE)
names(IHP_G1)
IHP_G2 <- left_join(IHP_G1,IHP_onaAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_G3 <- left_join(IHP_G2, IHP_fldDamAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_G4 <- left_join(IHP_G3, IHP_rpfvlAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_G5 <- left_join(IHP_G4, IHP_ppfvlAdjGZCTA , by="GZCTA", copy=FALSE)
IHP_G6 <- left_join(IHP_G5, IHP_rentalAssistanceAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_G7 <- left_join(IHP_G6, IHP_repairAmountAdjGZCTA, by="GZCTA", copy=FALSE)
IHP_G8 <- left_join(IHP_G7, IHP_replacementAmountAdjGZCTA, by="GZCTA", copy=FALSE)

names(IHP_G8)
colSums(is.na(IHP_G8))
head(IHP_G8) #19660 GZCTAs
dim(IHP_G8)


######################
#checking####
names(IHP_V6)
IHP_V6b <- IHP_V6[,c(-11:-16,-18:-21)]
names(IHP_V6b)

names(IHP_V15)

#writing out files#####
path2 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced/FS'
write.csv(IHP_V6b, file.path(path2, "IHP_V6_FS.csv"), row.names=TRUE)
write.csv(IHP_V15, file.path(path2, "IHP_DNZ_FS.csv"), row.names=TRUE)
write.csv(IHP_G8, file.path(path2, "IHP_GZCTA_FS.csv"), row.names=TRUE)


###aggregating HMGP Elevations and acquisitions###

#NOTE to self: Should not create rates and norms here until i have final subsets. Numbers change.



##Setup####
getwd()
setwd('G:/My Drive/Academic/Research')

#Libraries####
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)

#reading data ####
myHMA10 <- read.csv("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA/myHMA10.csv")

# Create binary columns for property actions 
myHMA10 <- myHMA10 %>%
  mutate(
    Acqui = ifelse(propertyAction3=="Acquisition", 1, 0),
    Elev = ifelse(propertyAction3=="Elevation", 1, 0)
  )

#ZCTA Agg ####
# Aggregate the sums by GZCTA 
ZCTA_HMGP <- myHMA10 %>%
  group_by(GZCTA) %>%
  summarize(
    Acqui= sum(Acqui, na.rm=TRUE),
    Elev = sum(Elev, na.rm=TRUE)
  )

#DNZ Agg ####
# Create DNZ ID ####
head(myHMA10)
range(myHMA10$DN)
myHMA10$DN <- str_pad(myHMA10$DN, pad="0",4)

myHMA10$DNZ <- str_c(myHMA10$DN,myHMA10$GZCTA)

# Aggregate the sums by DNZ
DNZ_HMGP <- myHMA10 %>%
  group_by(DNZ) %>%
  summarize(
    Acqui= sum(Acqui, na.rm=TRUE),
    Elev = sum(Elev, na.rm=TRUE)
  )

#County Agg ####
# Aggregate the sums by County 
myHMA10 <- myHMA10 %>%
  mutate(CntyID = str_c("C", str_pad(CntyID, width = 5, pad = "0")))

County_HMGP <- myHMA10 %>%
  group_by(CntyID) %>%
  summarize(
    Acqui= sum(Acqui, na.rm=TRUE),
    Elev = sum(Elev, na.rm=TRUE)
  )

#DNC (DN_County) Agg ####
# Create DNC ID ####
head(myHMA10)
range(myHMA10$DN)
myHMA10$DN <- str_pad(myHMA10$DN, pad="0",4)

myHMA10$DNC <- str_c(myHMA10$DN,myHMA10$CntyID)

# Aggregate the sums by DN_County
DNC_HMGP <- myHMA10 %>%
  group_by(DNC) %>%
  summarize(
    Acqui= sum(Acqui, na.rm=TRUE),
    Elev = sum(Elev, na.rm=TRUE)
  )


#State Agg ####
# Aggregate the sums by state 
colSums(is.na(myHMA10))
table(myHMA10$p2state)

State_HMGP <- myHMA10 %>%
  group_by(p2state) %>%
  summarize(
    Acqui= sum(Acqui, na.rm=TRUE),
    Elev = sum(Elev, na.rm=TRUE)
  )

#DNS (DN_State) Agg ####
# Create DNS ID ####
head(myHMA10)
range(myHMA10$DN)

myHMA10$DNS <- str_c(myHMA10$DN,myHMA10$p2state)

# Aggregate the sums by DN_County
DNS_HMGP <- myHMA10 %>%
  group_by(DNS) %>%
  summarize(
    Acqui= sum(Acqui, na.rm=TRUE),
    Elev = sum(Elev, na.rm=TRUE)
  )

# Writing out files ####
path1 <- ("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA/HMGP_Agg")
write.csv(ZCTA_HMGP, file.path(path1, "ZCTA_HMGP.csv"), row.names=TRUE)
write.csv(DNZ_HMGP, file.path(path1, "DNZ_HMGP.csv"), row.names=TRUE)

write.csv(County_HMGP, file.path(path1, "County_HMGP.csv"), row.names=TRUE)
write.csv(DNC_HMGP, file.path(path1, "DNC_HMGP.csv"), row.names=TRUE)

write.csv(State_HMGP, file.path(path1, "State_HMGP.csv"), row.names=TRUE)
write.csv(DNS_HMGP, file.path(path1, "DNS_HMGP.csv"), row.names=TRUE)
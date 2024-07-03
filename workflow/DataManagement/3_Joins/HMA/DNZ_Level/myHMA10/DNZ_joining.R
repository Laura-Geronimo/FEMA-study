#Purpose: DNZ fusing

#note: need to rework the paths to the data files

##Setup####
getwd()
setwd('C:/Users/lgero/Box/Research/FEMA_project')

##Libraries
library(dplyr)
library(stringr)
library(Hmisc)
library(tidycensus)
library(data.table)

#importing data####
myHMA10 <- read.csv('./Data/Edited/HMA/myHMA10.csv')
DNZ_FS_HMGP <- read.csv('./Data/Edited/HMA/HMGP_Agg/DNZ_HMGP.csv')

YOLZ_FS_NFIP <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/FEMA/NFIP/Claims/Produced/FS/YOLZ_FS_NFIP.csv')
DNZ_FS_IHP <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/FEMA/IA/Produced/FS/DNZLevel/DNZ_FS_IHP.csv')

#Dis4 <- read.csv('./Data/FEMA/DisasterData/Edited/Dis_01_23/DisSome.csv')

ZCTA_FirstStreet <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/Firststreet/Aggregated/V2/fsf_flood_zcta_summary.csv')
ZCTA_Census<- read.csv('./Data/Edited/Census/TS_ZCTA_Tidy.csv')

ZCTA_CSC <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/NOAA/Produced/Old/ZCTA_CSC.csv')
ZCTA_shore <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/NOAA/GIS/Produced/ZCTA_shoreline.csv')

RepRate <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/Politics/County Presidential Election Returns 2000-2020/Produced/RepRate.csv')
AIG <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/Politics/HarvardMIT/American_Ideology_Project/aip_zips_ideology_v2022.csv')



#states <- read.csv('./Data/Fused/V3/CSC/ZCTALevel/Old/StatesByZCTA_FS.csv')


#cleaning, reducing, basic management####
names(myHMA10)
myHMA10<- myHMA10[,c(-1)]

names(DNZ_FS_HMGP)
DNZ_FS_HMGP<- DNZ_FS_HMGP[,c(-1)]

#names(Dis4)
#Dis4 <- Dis4[,c(-1)]

names(ZCTA_Census)
ZCTA_Census <- ZCTA_Census[,c(-1)]
head(ZCTA_Census$GZCTA)

names(YOLZ_FS_NFIP)
YOLZ_FS_NFIP <- YOLZ_FS_NFIP[,c(-1)]

names(DNZ_FS_IHP)
DNZ_FS_IHP <- DNZ_FS_IHP[,c(-1)]

names(ZCTA_CSC)
head(ZCTA_CSC)
names(ZCTA_CSC)[names(ZCTA_CSC)=="ZCTA"] <- "GZCTA"
ZCTA_CSC$GZCTA <- str_pad(ZCTA_CSC$GZCTA,5,pad="0")
ZCTA_CSC$GZCTA <- str_c("G",ZCTA_CSC$GZCTA)
range(ZCTA_CSC$GZCTA)

names(ZCTA_shore)
head(ZCTA_shore)

names(AIG)
AIG<- AIG[,c(-1)]
AIG$GZCTA <- str_pad(AIG$zip,5,pad="0")
AIG$GZCTA <- str_c("G",AIG$GZCTA)
range(AIG$GZCTA)
AIG <-AIG[,c(3,2,11,15)]


#Creating DNZ in myHMA10####
names(myHMA10)
colSums(is.na(myHMA10))
range(myHMA10$DN)
myHMA10$DN <- str_pad(myHMA10$DN, pad="0", 4)
range(myHMA10$GZCTA)

myHMA10$DNZ <- str_c(myHMA10$DN, myHMA10$GZCTA)
range(myHMA10$DNZ)



#splitting out GZCTA and DN####
DNZ <- DNZ_FS_HMGP
head(DNZ1)
DNZ$DN <- substr(DNZ$DNZ, 1,4)
range(DNZ$DN)
DNZ$GZCTA <- substr(DNZ$DNZ, 5,11)
range(DNZ$GZCTA)


#joining disaster data####
#taking from the HMGP dataset I already combined with disaster info
names(myHMA10)
HMGPDis <- myHMA10 %>%
  select(c(DNZ, declarationType, fyDeclared, incidentType, declarationTitle, Hurricane, 
           SevereStorm, TropicalStorm, CoastalStorm, Tornado, Wind, WinterStorm,
           GroundSaturation, Flood, Mudslide, Landslide, HighSurf, TorrentialRain, HurricaneName, 
           TropicalStormName))
HMGPDis <- distinct(HMGPDis) #


##joining disaster data 
range(DNZ$DNZ)
range(HMGPDis$DNZ)
DNZ2 <- left_join(DNZ, HMGPDis, by="DNZ", copy=F)
colSums(is.na(DNZ2)) 
table(DNZ2$HurricaneName)



#joining FSFM to Base####
#ISSUE: was GENERATING DUPLICATS
names(ZCTA_FirstStreet)
ZCTA_FirstStreet1 <- ZCTA_FirstStreet[,c(1,3,4,9,10,13,14)]
head(ZCTA_FirstStreet1)
ZCTA_FirstStreet1 <- ZCTA_FirstStreet1 %>%
  rename(GZCTA = fips) %>%
  mutate(GZCTA = str_c("G", str_pad(GZCTA, 5, pad = "0")))
names(ZCTA_FirstStreet1)
ZCTA_FirstStreet2 <- distinct(ZCTA_FirstStreet1)
head(DNZ2$GZCTA)

DNZ3 <- left_join(DNZ2, ZCTA_FirstStreet2, by="GZCTA", copy=F)
colSums(is.na(DNZ3)) #20 missing FSFM data

#Examining for duplicates####
DNZ4 <-DNZ3
n_occur <- data.frame(table(DNZ4$DNZ))
n_occur[n_occur$Freq>1,]
duplicates<-DNZ4[DNZ4$DNZ %in% n_occur$Var1[n_occur$Freq > 1],]

##Removing dublicates
DNZ4b<-DNZ4[!duplicated(DNZ4$DNZ), ]
colSums(is.na(DNZ4b))

#joining NFIP to Base####
#need to create YOLZ using declarationDate --> DD
names(DNZ4b)
head(DNZ4b$fyDeclared)

range(DNZ4b$GZCTA)
DNZ4b$YOLZ <- str_c(DNZ4b$fyDeclared,DNZ4b$GZCTA)
head(DNZ4b$YOLZ)
range(DNZ4b$YOLZ)
head(YOLZ_FS_NFIP$YOLZ)
range(YOLZ_FS_NFIP$YOLZ)

DNZ5 <- left_join(DNZ4b, YOLZ_FS_NFIP, by="YOLZ", copy=F)
colSums(is.na(DNZ5)) #1939

#QC
Q1 <- sum(YOLZ_FS_NFIP$NFIP_AllClaimsAdj_YOLZ)
Q2 <- sum(DNZ5$NFIP_AllClaimsAdj_YOLZ, na.rm=T)
Q3 <- Q1 - Q2 
Q4 <- Q3/Q1 #NOTE - loosing 38 % of NFIP data. Likely NFIP claims in counties where no HMGP actions taken

#joining IHP to Base####
names(DNZ_FS_IHP)
head(DNZ_FS_IHP$DNZ)
head(DNZ5$DNZ)

DNZ6 <- left_join(DNZ5, DNZ_FS_IHP, by="DNZ", copy=F)
colSums(is.na(DNZ6)) #3850 without IHP data - not a great control, but what we got

Q1 <- sum(DNZ_FS_IHP$IHP_fldDamAmountAdjDNZ)
Q2 <- sum(DNZ6$IHP_fldDamAmountAdjDNZ, na.rm=T)
Q3 <- Q1 - Q2 
Q4 <- Q3/Q1 #note - loosing 38% of IHP data. Likely IHP in counties where not HMGP actIons



#Joining Census data####
ZCTA_Census <- distinct(ZCTA_Census)

names(ZCTA_Census)
range(ZCTA_Census$GZCTA, na.rm=T)
range(DNZ6$GZCTA)

DNZ7 <- left_join(DNZ6, ZCTA_Census, by="GZCTA", copy=FALSE)


## Creating sociodemographic data based on DD-1####
class(DNZ7$fyDeclared)
DNZ7$fyDeclared <- as.numeric(DNZ7$fyDeclared)
DNZ7$fyDeclaredLess1 <- DNZ7$fyDeclared - 1
table(DNZ7$fyDeclaredLess1)


##TotPop####
names(DNZ7)
head(DNZ7)
DNZ7$TotPop <- NA
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_TotPop, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_TotPop, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_TotPop, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2022, DNZ7$y22_TotPopE, DNZ7$TotPop)
DNZ7$TotPop <- ifelse(DNZ7$fyDeclaredLess1 == 2023, DNZ7$y22_TotPopE, DNZ7$TotPop)

names(DNZ7)
table(is.na(DNZ7$TotPop)) #4441
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, TotPop, y90_TotPop, y00_TotPop, y10_TotPop, y11_TotPopE, y12_TotPopE, y13_TotPopE, y14_TotPopE,
         y15_TotPopE,y16_TotPopE, y17_TotPopE, y18_TotPopE, y19_TotPopE, y20_TotPopE, y21_TotPopE, y22_TotPopE)


##WhitePct####
DNZ7$WhitePct <- NA
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_WhitePct, DNZ7$WhitePct)
DNZ7$WhitePct <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_WhitePct, DNZ7$WhitePct)

table(is.na(DNZ7$WhitePct)) #4443
names(DNZ7)
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, WhitePct, y90_WhitePct, y00_WhitePct, y10_WhitePct, y11_WhitePct, y12_WhitePct, y13_WhitePct, y14_WhitePct,
         y15_WhitePct,y16_WhitePct, y17_WhitePct, y18_WhitePct, y19_WhitePct, y20_WhitePct, y21_WhitePct, y22_WhitePct)


##BlackPct####
DNZ7$BlackPct <- NA
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_BlackPct, DNZ7$BlackPct)
DNZ7$BlackPct <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_BlackPct, DNZ7$BlackPct)

table(is.na(DNZ7$BlackPct)) #4443
names(DNZ7)
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, BlackPct, y90_BlackPct, y00_BlackPct, y10_BlackPct, y11_BlackPct, y12_BlackPct, y13_BlackPct, y14_BlackPct,
         y15_BlackPct,y16_BlackPct, y17_BlackPct, y18_BlackPct, y19_BlackPct, y20_BlackPct, y21_BlackPct, y22_BlackPct)



##HispPct####
DNZ7$HispPct <- NA
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_HispPct, DNZ7$HispPct)
DNZ7$HispPct <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_HispPct, DNZ7$HispPct)

table(is.na(DNZ7$HispPct)) #4443
names(DNZ7)
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, HispPct, y90_HispPct, y00_HispPct, y10_HispPct, y11_HispPct, y12_HispPct, y13_HispPct, y14_HispPct,
         y15_HispPct,y16_HispPct, y17_HispPct, y18_HispPct, y19_HispPct, y20_HispPct, y21_HispPct, y22_HispPct)



##MHIadj####
## notes####
#no 1990 data so using 2000 as proxy
#no 2010 data, so using 2011 as proxy
names(DNZ7)
DNZ7$MHIadj <- NA
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y11_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_MHIE, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_MHIadj, DNZ7$MHIadj)
DNZ7$MHIadj <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y21_MHIadj, DNZ7$MHIadj)

table(is.na(DNZ7$MHIadj)) #4211
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, MHIadj, y00_MHIadj,  y11_MHIadj, y12_MHIadj, y13_MHIadj, y14_MHIadj,
         y15_MHIadj,y16_MHIadj, y17_MHIadj, y18_MHIadj, y19_MHIadj, y20_MHIE, y21_MHIadj, y22_MHIadj)


##MHVadj####
##notes####
#no 1990 data so using 2000 as proxy
#no 2010 data, so using 2011 as proxy
names(DNZ7)
DNZ7$MHVadj <- NA
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y11_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_MHVE, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_MHVadj, DNZ7$MHVadj)
DNZ7$MHVadj <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y21_MHVadj, DNZ7$MHVadj)

table(is.na(DNZ7$MHVadj)) #4211
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, MHVadj, y00_MHVadj,  y11_MHVadj, y12_MHVadj, y13_MHVadj, y14_MHVadj,
         y15_MHVadj,y16_MHVadj, y17_MHVadj, y18_MHVadj, y19_MHVadj, y20_MHVE, y21_MHVadj, y22_MHVadj)


##PopDenseSqMile####
#no 90 data so using 2000 as proxy
names(DNZ7)
colSums(is.na(DNZ7))
DNZ7$PopDenseSqMile <- NA
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_PopDenseSqMile, DNZ7$PopDenseSqMile)
DNZ7$PopDenseSqMile <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_PopDenseSqMile, DNZ7$PopDenseSqMile)

table(is.na(DNZ7$PopDenseSqMile)) #4032


##TotHU####
names(DNZ7)
DNZ7$TotHU <- NA
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_TotHU, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_TotHU, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_TotHU, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_TotHUE, DNZ7$TotHU)
DNZ7$TotHU <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_TotHUE, DNZ7$TotHU)

table(is.na(DNZ7$TotHU)) #4441
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, TotHU, y90_TotHU, y00_TotHU,  y11_TotHUE, y12_TotHUE, y13_TotHUE, y14_TotHUE,
         y15_TotHUE,y16_TotHUE, y17_TotHUE, y18_TotHUE, y19_TotHUE, y20_TotHUE, y21_TotHUE, y22_TotHUE)



##TotOccHU####
names(DNZ7)
DNZ7$TotOccHU <- NA
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_TotOcc_HU, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_TotOccHU, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_TotOccHU, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_TotOccHUE, DNZ7$TotOccHU)
DNZ7$TotOccHU <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_TotOccHUE, DNZ7$TotOccHU)

table(is.na(DNZ7$TotOccHU)) #4441
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, TotOccHU, y90_TotOcc_HU, y00_TotOccHU,  y11_TotOccHUE, y12_TotOccHUE, y13_TotOccHUE, y14_TotOccHUE,
         y15_TotOccHUE,y16_TotOccHUE, y17_TotOccHUE, y18_TotOccHUE, y19_TotOccHUE, y20_TotOccHUE, y21_TotOccHUE, y22_TotOccHUE)


##OwnOccPct####
DNZ7$OwnOccPct <- NA
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_OwnOccPct, DNZ7$OwnOccPct)
DNZ7$OwnOccPct <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_OwnOccPct, DNZ7$OwnOccPct)

table(is.na(DNZ7$OwnOccPct)) #4444
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, OwnOccPct, y90_OwnOccPct, y00_OwnOccPct,  y11_OwnOccPct, y12_OwnOccPct, y13_OwnOccPct, y14_OwnOccPct,
         y15_OwnOccPct,y16_OwnOccPct, y17_OwnOccPct, y18_OwnOccPct, y19_OwnOccPct, y20_OwnOccPct, y21_OwnOccPct, y22_OwnOccPct)

##RentOccPct####
DNZ7$RentOccPct <- NA
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_RentOccPct, DNZ7$RentOccPct)
DNZ7$RentOccPct <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_RentOccPct, DNZ7$RentOccPct)

table(is.na(DNZ7$RentOccPct)) #4444
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, RentOccPct, y90_RentOccPct, y00_RentOccPct,  y11_RentOccPct, y12_RentOccPct, y13_RentOccPct, y14_RentOccPct,
         y15_RentOccPct,y16_RentOccPct, y17_RentOccPct, y18_RentOccPct, y19_RentOccPct, y20_RentOccPct, y21_RentOccPct, y22_RentOccPct)


##2ndHomePct####
DNZ7$S2ndHomePct <- NA
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 <= 1999, DNZ7$y90_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 >= 2000 & DNZ7$fyDeclaredLess1 <= 2009, DNZ7$y00_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2010, DNZ7$y10_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2011, DNZ7$y11_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2012, DNZ7$y12_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2013, DNZ7$y13_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2014, DNZ7$y14_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2015, DNZ7$y15_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2016, DNZ7$y16_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2017, DNZ7$y17_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2018, DNZ7$y18_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2019, DNZ7$y19_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2020, DNZ7$y20_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 == 2021, DNZ7$y21_2ndHomePct, DNZ7$S2ndHomePct)
DNZ7$S2ndHomePct <- ifelse(DNZ7$fyDeclaredLess1 >= 2022, DNZ7$y22_2ndHomePct, DNZ7$S2ndHomePct)

table(is.na(DNZ7$S2ndHomePct)) #4444
test <- DNZ7 %>%
  select(GZCTA, fyDeclaredLess1, S2ndHomePct, y90_2ndHomePct, y00_2ndHomePct,  y11_2ndHomePct, y12_2ndHomePct, y13_2ndHomePct, y14_2ndHomePct,
         y15_2ndHomePct,y16_2ndHomePct, y17_2ndHomePct, y18_2ndHomePct, y19_2ndHomePct, y20_2ndHomePct, y21_2ndHomePct, y22_2ndHomePct)



##Joining States####
names(myHMA10)
states1 <- myHMA10[,c("DNZ","stateNumberCode","p2state")]
states2 <- distinct(states1)

DNZ8<- left_join(DNZ7, states2, by="DNZ", copy=FALSE)
DNZ9<- distinct(DNZ8, DNZ, .keep_all = T) #getting rid of duplicated rows


table(is.na(DNZ9$p2state))  #0
table(DNZ9$p2state)

##Creating cntyID ####
names(myHMA10)
colSums(is.na(myHMA10))
cntyID <- myHMA10[,c("DNZ", "CntyID")] #
cntyID <- distinct(cntyID)

DNZ9b <- left_join(DNZ9, cntyID, by="DNZ", copy=F) 
##removing duplicates
DNZ9b<-DNZ9b[!duplicated(DNZ9b$DNZ), ]
##formating
DNZ9b <- DNZ9b %>%
  mutate(CntyID = str_c("C", str_pad(CntyID, 5, pad = "0")))
range(DNZ9b$CntyID)

## Joining CSC####
names(DNZ9b)
names(ZCTA_CSC)
DNZ9c <- left_join(DNZ9b, ZCTA_CSC, by="GZCTA", copy=F)
names(DNZ9c)
table(DNZ9c$ZCTA_CSC)
DNZ9c$ZCTA_CSC[is.na(DNZ9c$ZCTA_CSC)]<-0
names(DNZ9c)[names(DNZ9c)=="ZCTA_CSC"]<-"CSC"

## Joining Shore####
names(DNZ9c)
names(ZCTA_shore)
ZCTA_shore$ZCTA_shore <- 1
DNZ9c <- left_join(DNZ9c, ZCTA_shore, by="GZCTA", copy=F)
names(DNZ9c)
table(DNZ9c$ZCTA_shore)
DNZ9c$ZCTA_shore[is.na(DNZ9c$ZCTA_shore)]<-0
names(DNZ9c)


##Joining RepRate####
#creating CntyVoteID ####
names(DNZ9c)
head(DNZ9c$CntyID)
range(DNZ9c$CntyID)


DNZ9c$VoteYear <- 0
DNZ9c$VoteYear[DNZ9c$fyDeclared < 2004]<- 2000  #this would be making a big assumption that 2004 vote represents sentiment throughout 1990s
DNZ9c$VoteYear[DNZ9c$fyDeclared >= 2004 & DNZ9c$fyDeclared < 2008]<-2004
DNZ9c$VoteYear[DNZ9c$fyDeclared >= 2008 & DNZ9c$fyDeclared < 2012]<-2008
DNZ9c$VoteYear[DNZ9c$fyDeclared >= 2012 & DNZ9c$fyDeclared < 2016]<-2012
DNZ9c$VoteYear[DNZ9c$fyDeclared >= 2016 & DNZ9c$fyDeclared < 2020]<-2020
DNZ9c$VoteYear[DNZ9c$fyDeclared >= 2020]<-2020
table(DNZ9c$VoteYear)
DNZ9c$CntyVoteID <- str_c(DNZ9c$VoteYear, DNZ9c$CntyID)
head(DNZ9c$CntyVoteID)
class(DNZ9c$CntyVoteID)
names(DNZ9c)

#joining RepRate
DNZ9d <- left_join(DNZ9c, RepRate, by="CntyVoteID", copy=FALSE)
DNZ9d<- distinct(DNZ9d, DNZ, .keep_all = T) #getting rid of duplicated rows

hist(DNZ9d$RepRate)
colSums(is.na(DNZ9d))

#joining AIG data ####
#creating an AIG_id to join
table(AIG$presidential_year)
table(DNZ9d$fyDeclared)

DNZ9d$VoteYear <- 0
DNZ9d$VoteYear[DNZ9d$fyDeclared < 2008]<- 2008  #this would be making a big assumption that 2008 vote represents sentiment throughout 1990s
DNZ9d$VoteYear[DNZ9d$fyDeclared >= 2008 & DNZ9d$fyDeclared < 2016]<-2008
DNZ9d$VoteYear[DNZ9d$fyDeclared >= 2016 & DNZ9d$fyDeclared < 2020]<-2016
DNZ9d$VoteYear[DNZ9d$fyDeclared >= 2020]<-2020
table(DNZ9d$VoteYear)

DNZ9d$AIG_id <- str_c(DNZ9d$VoteYear, DNZ9d$GZCTA)
head(DNZ9d$AIG_id)
class(DNZ9d$AIG_id)

names(AIG)
AIG$AIG_id <- str_c(AIG$presidential_year, AIG$GZCTA)
head(AIG$AIG_id)
class(AIG$AIG_id)

DNZ9e <- left_join(DNZ9d,AIG, by="AIG_id", copy=F)
colSums(is.na(DNZ9e))

DNZ9f<- distinct(DNZ9e, DNZ, .keep_all = T)

##examining
names(DNZ9f)

##Writing out files
path1 <- 'C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Joined/DNZLevel/myHMA10'
write.csv(DNZ9f, file.path(path1, "DNZ9f.csv"), row.names=TRUE)






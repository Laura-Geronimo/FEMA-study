#Purpose: DNZ Rates and Norms

##Setup####
getwd()
setwd('C:/Users/lgero/Box/Research')

##Libraries
library(dplyr)
library(stringr)
library(Hmisc)
library(tidycensus)

options (scipen=999)


#importing data####
DNZ9 <- read.csv('C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Joined/DNZLevel/myHMA10/DNZ9f.csv')

#cleaning, reducing, basic management####
names(DNZ9)
table(DNZ9$ZCTA_shore)
sum(DNZ9$Elev)
sum(DNZ9$Acqui)
DNZ9<- DNZ9[,c(-1)]
QC <- sum(DNZ9$Acqui) + sum(DNZ9$Elev)
names(DNZ9)[names(DNZ9)=="GZCTA.x"] <- "GZCTA"

sum(DNZ9$Acqui) # 50559
sum(DNZ9$Elev) #11,062


#Data Reduction 1####
#keeping vars of immediate interest####
#REMEMBER- need to do rates and norms still - but after you get to a sample with complete data on vars of interest
names(DNZ9) 
DNZ9$GZC
DNZ10 <- DNZ9[,c("DNZ", "DN",
                "fyDeclared", 
                "YOLZ",
                "GZCTA", 
                "CntyID",
                "stateNumberCode","p2state",
                "Acqui", "Elev" ,
                "incidentType","declarationTitle",
                "Hurricane", "SevereStorm", "TropicalStorm","CoastalStorm",
                "Tornado","Wind","WinterStorm","GroundSaturation","Flood",
                "Mudslide","Landslide","HighSurf","TorrentialRain",
                "HurricaneName" , "TropicalStormName",
               # "Coastal", "Riverine",
                "CSC","ZCTA_shore",  
                "count_fema_sfha", "pct_fema_sfha", "count_fs_risk_100_year00","pct_fs_risk_100_year00","count_fs_risk_500_year00","pct_fs_risk_500_year00",
                "NFIP_AllClaimsAdj_YOLZ", "NFIP_BuildClaimAdj_YOLZ", "NFIP_ContClaimAdj_YOLZ","NFIP_ICCadj_YOLZ",
                "IHP_fldDamAmountAdjDNZ",
                "IHP_rentalAssistanceAmountAdjDNZ",
                "TotPop",
                "WhitePct",
                "BlackPct",
                "HispPct",
                "MHIadj",
                "MHVadj", 
                "PopDenseSqMile",
                "TotHU", 
                "TotOccHU",
                "OwnOccPct",
                "RentOccPct",
                "S2ndHomePct", 
                "CntyVoteID",
                "RepRate",
                "mrp_ideology")]
names(DNZ10)


#QC####
names(DNZ10)
colSums(is.na(DNZ10))
sum(DNZ10$Elev)
sum(DNZ10$Acqui)

#converting NAs to 0s where appropriate####
colSums(is.na(DNZ10))

#NAs for NFIP and IHP data should be 0s####
table(is.na(DNZ10$IHP_fldDamAmountAdjDNZ))
range(DNZ10$IHP_fldDamAmountAdjDNZ,na.rm=T)

DNZ10$IHP_fldDamAmountAdjDNZ[is.na(DNZ10$IHP_fldDamAmountAdj)]<-0
DNZ10$IHP_rentalAssistanceAmountAdjDNZ[is.na(DNZ10$IHP_rentalAssistanceAmountAdjDNZ)]<-0
DNZ10$NFIP_AllClaimsAdj_YOLZ[is.na(DNZ10$NFIP_AllClaimsAdj_YOLZ)]<-0
DNZ10$NFIP_BuildClaimAdj_YOLZ[is.na(DNZ10$NFIP_BuildClaimAdj_YOLZ)]<-0
DNZ10$NFIP_ContClaimAdj_YOLZ[is.na(DNZ10$NFIP_ContClaimAdj_YOLZ)]<-0
DNZ10$NFIP_ICCadj_YOLZ[is.na(DNZ10$NFIP_ICCadj_YOLZ)]<-0

DNZ10$HurricaneName[is.na(DNZ10$HurricaneName)]<-0
DNZ10$TropicalStormName[is.na(DNZ10$TropicalStormName)]<-0


#Examining missing data
colSums(is.na(DNZ10))
seeNAs <-  DNZ10[rowSums(is.na(DNZ10)) > 0, ] #264
sum(seeNAs$Acqui) #loosing 1707 buyouts
sum(seeNAs$Elev) #loosing 423 elevs

##Now subset to sample with all data####

DNZ11 <- na.omit(DNZ10)
names(DNZ11)

#Now create sums####
DNZ11$TotalProps <- DNZ11$Elev + DNZ11$Acqui
sum(DNZ11$TotalProps)


#Now fractions: facqui, felev########
names(DNZ11)
DNZ11$facqui <- DNZ11$Acqui / DNZ11$TotalProps
range(DNZ11$facqui, na.rm=T)

DNZ11$felev <- DNZ11$Elev / DNZ11$TotalProps
range(DNZ11$felev, na.rm=T)



#Combine_NFIP_IHPAdj####
names(DNZ11)
DNZ11$Combine_NFIP_IHPAdj <- DNZ11$NFIP_AllClaimsAdj_YOLZ + DNZ11$IHP_fldDamAmountAdjDNZ
table(is.na(DNZ11$Combine_NFIP_IHPAdj))
range(DNZ11$Combine_NFIP_IHPAdj)

#creating rates & norms####

#property Action Rates by counts of homes in SFHA####

#Acqui_RateBy_count_sfha_pct####
class(DNZ11$Acqui)
range(DNZ11$Acqui, na.rm=T)
DNZ11$Acqui_RateBy_count_sfha_pct <- DNZ11$Acqui / DNZ11$count_fema_sfha *100
range(DNZ11$Acqui_RateBy_count_sfha_pct, na.rm=T)
plot(DNZ11$Acqui_RateBy_count_sfha_pct)  #NOTE OUTLIERS
hist(DNZ11$Acqui_RateBy_count_sfha_pc)
table(is.na(DNZ11$Acqui_RateBy_count_sfha_pct))
table(is.na(DNZ11$Acqui))

#Elev_RateBy_count_sfha_pct####
DNZ11$Elev_RateBy_count_sfha_pct <- DNZ11$Elev / DNZ11$count_fema_sfha *100
range(DNZ11$Elev_RateBy_count_sfha_pct, na.rm=T)
plot(DNZ11$Elev_RateBy_count_sfha_pct)  #NOTE OUTLIERS
hist(DNZ11$Elev_RateBy_count_sfha_pct)

#All
DNZ11$Total_RateBy_count_sfha_pct <- DNZ11$TotalProps / DNZ11$count_fema_sfha *100
range(DNZ11$Total_RateBy_count_sfha_pct, na.rm=T)
plot(DNZ11$Total_RateBy_count_sfha_pct)  #NOTE OUTLIERS
hist(DNZ11$Total_RateBy_count_sfha_pct)


###Checking inf values####
#caused because there are places where count_fema_sfha = 0
#chaning to NaN
table(is.infinite(DNZ11$Acqui_RateBy_count_sfha_pct)) #62
table(is.infinite(DNZ11$Elev_RateBy_count_sfha_pct)) #6
table(is.infinite(DNZ11$Total_RateBy_count_sfha_pct)) #66

#Managing infinite NAN####
DNZ11[sapply(DNZ11, is.infinite)]<- NaN


##Creating new vars based on national medians####
#Left out this time but can go back and recreate if needed


##Managing NAs, outliers / winsorizing / normalizing####


##Examining Data#
names(DNZ11)
plot(DNZ11$felev, DNZ11$facqui)
plot(DNZ11$felev, DNZ11$TotPop) 
plot(DNZ11$felev, DNZ11$WhitePct) 
plot(DNZ11$felev, DNZ11$BlackPct)
plot(DNZ11$felev, DNZ11$HispPct)
plot(DNZ11$felev, DNZ11$MHIadj) #divide by 10K
plot(DNZ11$felev, DNZ11$MHVadj) #Divide by 100K / winsorize
plot(DNZ11$felev, DNZ11$OwnOccPct) #
plot(DNZ11$felev, DNZ11$PopDenseSqMile) # normalize (divide by 1000)
plot(DNZ11$felev, DNZ11$pct_fema_sfha) #fraction and deciles
#plot(DNZ11$felev, DNZ11$fCoastal)  # Consider deciles?
#plot(DNZ11$fCoastal)
#plot(DNZ11$felev, DNZ11$fRiverine)
plot(DNZ11$felev, DNZ11$TotOccHU) # norm


range(DNZ11$IHP_fldDamAmountAdjDNZ) #win, 1M

range(DNZ11$IHP_rentalAssistanceAmountAdjDNZ, na.rm=T) 


#Processing data#
#TotPop_W_10k ####
range(DNZ11$TotPop)
plot(DNZ11$TotPop)

quantile(DNZ11$TotPop, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$TotPop_W <- DNZ11$TotPop
DNZ11$TotPop_W[DNZ11$TotPop_W >= 57134.7600   ]<- 57134.7600 
DNZ11$TotPop_W[DNZ11$TotPop_W <= 240.4832 ]<-  240.4832
range(DNZ11$TotPop_W,na.rm=T)
plot(DNZ11$TotPop_W)

DNZ11$TotPop_W_10k <- DNZ11$TotPop_W/10000
range(DNZ11$TotPop_W_10k,na.rm=T)
plot(DNZ11$TotPop_W_10k)

#PopDense_W_1k ####
range(DNZ11$PopDenseSqMile)
plot(DNZ11$PopDenseSqMile)

quantile(DNZ11$PopDense, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$PopDense_W <- DNZ11$PopDense
DNZ11$PopDense_W[DNZ11$PopDense_W >=7202.691480   ]<- 7202.691480
DNZ11$PopDense_W[DNZ11$PopDense_W <=4.053464   ]<- 4.053464   
range(DNZ11$PopDense_W,na.rm=T)
plot(DNZ11$PopDense_W)

DNZ11$PopDense_W_1k <- DNZ11$PopDense_W/1000
range(DNZ11$PopDense_W_1k,na.rm=T)
plot(DNZ11$PopDense_W_1k)


#fWhite####
range(DNZ11$WhitePct, na.rm=T)

#fHisp####
range(DNZ11$HispPct, na.rm=T)

#fBlack####
range(DNZ11$BlackPct, na.rm=T)


#MHIadj_W_10k####
#Win & divided by 10,000
range(DNZ11$MHIadj)
plot(DNZ11$MHIadj)

quantile(DNZ11$MHIadj, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$MHIadj_W <- DNZ11$MHIadj
DNZ11$MHIadj_W[DNZ11$MHIadj_W >=135171.61 ]<- 135171.61 
DNZ11$MHIadj_W[DNZ11$MHIadj_W <=25398.08 ]<- 25398.08 
range(DNZ11$MHIadj_W,na.rm=T)
plot(DNZ11$MHIadj_W)

DNZ11$MHIadj_W_10k <- DNZ11$MHIadj_W/10000
range(DNZ11$MHIadj_W_10k,na.rm=T)
plot(DNZ11$MHIadj_W_10k)

#MHVadj_W_100k####
#Win & divided by 100,000
range(DNZ11$MHVadj)
plot(DNZ11$MHVadj)

quantile(DNZ11$MHVadj, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$MHVadj_W <- DNZ11$MHVadj
DNZ11$MHVadj_W[DNZ11$MHVadj_W >= 689748.48   ]<- 689748.48 
DNZ11$MHVadj_W[DNZ11$MHVadj_W <= 44665.12  ]<-  44665.12
range(DNZ11$MHVadj_W,na.rm=T)
plot(DNZ11$MHVadj_W)

DNZ11$MHVadj_W_100k <- DNZ11$MHVadj_W/100000
range(DNZ11$MHVadj_W_100k, na.rm=T)
plot(DNZ11$MHVadj_W_100k)


#fOwnOcc####
range(DNZ11$OwnOccPct, na.rm=T)



#f_fema_sfha####
range(DNZ11$pct_fema_sfha, na.rm=T)
DNZ11$f_fema_sfha <- DNZ11$pct_fema_sfha / 100
range(DNZ11$f_fema_sfha, na.rm=T)

#count_fema_sfha_W_1k####
range(DNZ11$count_fema_sfha, na.rm=T)
hist(DNZ11$count_fema_sfha)

quantile(DNZ11$count_fema_sfha, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$count_fema_sfha_W <- DNZ11$count_fema_sfha
DNZ11$count_fema_sfha_W[DNZ11$count_fema_sfha_W >=8864   ]<- 8864
DNZ11$count_fema_sfha_W[DNZ11$count_fema_sfha_W <=0  ]<- 0 
range(DNZ11$count_fema_sfha_W, na.rm=T)
hist(DNZ11$count_fema_sfha_W)

DNZ11$count_fema_sfha_W_1k <- DNZ11$count_fema_sfha_W / 1000
range(DNZ11$count_fema_sfha_W_1k, na.rm=T)


#count_fs_risk_100_year00_W_1k####
range(DNZ11$count_fs_risk_100_year00, na.rm=T)
hist(DNZ11$count_fs_risk_100_year00)

quantile(DNZ11$count_fs_risk_100_year00, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$count_fs_risk_100_year00_W <- DNZ11$count_fs_risk_100_year00
DNZ11$count_fs_risk_100_year00_W[DNZ11$count_fs_risk_100_year00_W >=8988.44   ]<- 8988.44 
DNZ11$count_fs_risk_100_year00_W[DNZ11$count_fs_risk_100_year00_W <=41.00  ]<- 41.00 
range(DNZ11$count_fs_risk_100_year00_W, na.rm=T)
hist(DNZ11$count_fs_risk_100_year00_W)

DNZ11$count_fs_risk_100_year00_W_1k <- DNZ11$count_fs_risk_100_year00_W / 1000
range(DNZ11$count_fs_risk_100_year00_W_1k, na.rm=T)


#f_fs_risk_100_year00####
range(DNZ11$pct_fs_risk_100_year00, na.rm=T)
DNZ11$f_fs_risk_100_year00 <- DNZ11$pct_fs_risk_100_year00 / 100
range(DNZ11$f_fs_risk_100_year00, na.rm=T)
plot(DNZ11$f_fs_risk_100_year00)

#count_fs_risk_500_year00_W_1k####
range(DNZ11$count_fs_risk_500_year00, na.rm=T)
hist(DNZ11$count_fs_risk_500_year00)

quantile(DNZ11$count_fs_risk_500_year00, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$count_fs_risk_500_year00_W <- DNZ11$count_fs_risk_500_year00
DNZ11$count_fs_risk_500_year00_W[DNZ11$count_fs_risk_500_year00_W >=14858.72  ]<- 14858.72
DNZ11$count_fs_risk_500_year00_W[DNZ11$count_fs_risk_500_year00_W <=55.32  ]<- 55.32
range(DNZ11$count_fs_risk_500_year00_W, na.rm=T)
hist(DNZ11$count_fs_risk_500_year00_W)

DNZ11$count_fs_risk_500_year00_W_1k <- DNZ11$count_fs_risk_500_year00_W / 1000
range(DNZ11$count_fs_risk_500_year00_W_1k, na.rm=T)


#f_fs_risk_500_year00####
range(DNZ11$pct_fs_risk_500_year00, na.rm=T)
DNZ11$f_fs_risk_500_year00 <- DNZ11$pct_fs_risk_500_year00 / 100
range(DNZ11$f_fs_risk_500_year00, na.rm=T)
plot(DNZ11$f_fs_risk_500_year00)


##TotOccHU_W_10k####
range(DNZ11$TotOccHU, na.rm=T)
plot(DNZ11$TotOccHU)
plot(DNZ11$TotOccHU, DNZ11$WhitePct)
plot(DNZ11$TotOccHU, DNZ11$Elev)
plot(DNZ11$TotOccHU, DNZ11$Acqui)

quantile(DNZ11$TotOccHU, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$TotOccHU_W <- DNZ11$TotOccHU
DNZ11$TotOccHU_W[DNZ11$TotOccHU_W >=20929.60 ]<- 20929.60   
DNZ11$TotOccHU_W[DNZ11$TotOccHU_W <= 103.64 ]<-  103.64
range(DNZ11$TotOccHU_W, na.rm=T)
plot(DNZ11$TotOccHU_W)

DNZ11$TotOccHU_W_10k <- DNZ11$TotOccHU_W/10000
range(DNZ11$TotOccHU_W_10k,na.rm=T)
plot(DNZ11$TotOccHU_W_10k)

#Acqui_RateBy_count_sfha_pct_W####
plot(DNZ11$Acqui_RateBy_count_sfha_pct)

quantile(DNZ11$Acqui_RateBy_count_sfha_pct, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$Acqui_RateBy_count_sfha_pct_W <- DNZ11$Acqui_RateBy_count_sfha_pct
DNZ11$Acqui_RateBy_count_sfha_pct_W[DNZ11$Acqui_RateBy_count_sfha_pct_W >=53.6993562  ]<- 53.6993562
DNZ11$Acqui_RateBy_count_sfha_pct_W[DNZ11$Acqui_RateBy_count_sfha_pct_W <=0  ]<- 0 
range(DNZ11$Acqui_RateBy_count_sfha_pct_W, na.rm=T)
plot(DNZ11$Acqui_RateBy_count_sfha_pct_W)

#Elev_RateBy_count_sfha_pct_W####
plot(DNZ11$Elev_RateBy_count_sfha_pct)

quantile(DNZ11$Elev_RateBy_count_sfha_pct, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$Elev_RateBy_count_sfha_pct_W <- DNZ11$Elev_RateBy_count_sfha_pct
DNZ11$Elev_RateBy_count_sfha_pct_W[DNZ11$Elev_RateBy_count_sfha_pct_W >=4.926079 ]<- 4.926079
DNZ11$Elev_RateBy_count_sfha_pct_W[DNZ11$Elev_RateBy_count_sfha_pct_W <=0  ]<- 0 
range(DNZ11$Elev_RateBy_count_sfha_pct_W, na.rm=T)
plot(DNZ11$Elev_RateBy_count_sfha_pct_W)


#Total_RateBy_count_sfha_pct_W####
plot(DNZ11$Total_RateBy_count_sfha_pct) #win

quantile(DNZ11$Total_RateBy_count_sfha_pct, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$Total_RateBy_count_sfha_pct_W <- DNZ11$Total_RateBy_count_sfha_pct
DNZ11$Total_RateBy_count_sfha_pct_W[DNZ11$Total_RateBy_count_sfha_pct_W >=57.32900433  ]<-57.32900433
DNZ11$Total_RateBy_count_sfha_pct_W[DNZ11$Total_RateBy_count_sfha_pct_W <=0.02064611    ]<-0.02064611
range(DNZ11$Total_RateBy_count_sfha_pct_W, na.rm=T)
plot(DNZ11$Total_RateBy_count_sfha_pct_W)


#IHP_fldDamAmountAdjDNZ_W_10M####
#winsorizing
names(DNZ11)
range(DNZ11$IHP_fldDamAmountAdjDNZ, na.rm=T)
plot(DNZ11$IHP_fldDamAmountAdjDNZ) 
table(is.infinite(DNZ11$IHP_fldDamAmountAdjDNZ)) #not returning any

quantile(DNZ11$IHP_fldDamAmountAdjDNZ, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$IHP_fldDamAmountAdjDNZ_W <- DNZ11$IHP_fldDamAmountAdjDNZ
DNZ11$IHP_fldDamAmountAdjDNZ_W[DNZ11$IHP_fldDamAmountAdjDNZ_W >=51954459  ]<- 51954459 
DNZ11$IHP_fldDamAmountAdjDNZ_W[DNZ11$IHP_fldDamAmountAdjDNZ_W <= 0.000  ]<-  0.000
range(DNZ11$IHP_fldDamAmountAdjDNZ_W, na.rm=T)
plot(DNZ11$IHP_fldDamAmountAdjDNZ_W)

DNZ11$IHP_fldDamAmountAdjDNZ_W_10M<- DNZ11$IHP_fldDamAmountAdjDNZ_W /10000000
range(DNZ11$IHP_fldDamAmountAdjDNZ_W_10M, na.rm=T)
hist(DNZ11$IHP_fldDamAmountAdjDNZ_W_10M)
plot(DNZ11$IHP_fldDamAmountAdjDNZ_W_10M)


#NFIP_AllClaimsAdj_YOLZ_W_10M####
#winsorizing
names(DNZ11)
range(DNZ11$NFIP_AllClaimsAdj_YOLZ, na.rm=T)
plot(DNZ11$NFIP_AllClaimsAdj_YOLZ) 
table(is.infinite(DNZ11$NFIP_AllClaimsAdj_YOLZ)) #not returning any

quantile(DNZ11$NFIP_AllClaimsAdj_YOLZ, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$NFIP_AllClaimsAdj_YOLZ_W <- DNZ11$NFIP_AllClaimsAdj_YOLZ
DNZ11$NFIP_AllClaimsAdj_YOLZ_W[DNZ11$NFIP_AllClaimsAdj_YOLZ_W >=219795543.95   ]<- 219795543.95 
DNZ11$NFIP_AllClaimsAdj_YOLZ_W[DNZ11$NFIP_AllClaimsAdj_YOLZ_W <= 0.000  ]<-  0.000
range(DNZ11$NFIP_AllClaimsAdj_YOLZ_W, na.rm=T)
plot(DNZ11$NFIP_AllClaimsAdj_YOLZ_W)

DNZ11$NFIP_AllClaimsAdj_YOLZ_W_10M <- DNZ11$NFIP_AllClaimsAdj_YOLZ_W /10000000
range(DNZ11$NFIP_AllClaimsAdj_YOLZ_W_10M, na.rm=T)
hist(DNZ11$NFIP_AllClaimsAdj_YOLZ_W_10M)
plot(DNZ11$NFIP_AllClaimsAdj_YOLZ_W_10M)

#NFIP_ICCadj_YOLZ_W_1M####
#winsorizing
names(DNZ11)
range(DNZ11$NFIP_ICCadj_YOLZ, na.rm=T)
plot(DNZ11$NFIP_ICCadj_YOLZ) 
table(is.infinite(DNZ11$NFIP_ICCadj_YOLZ)) #not returning any

quantile(DNZ11$NFIP_ICCadj_YOLZ, probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$NFIP_ICCadj_YOLZ_W <- DNZ11$NFIP_ICCadj_YOLZ
DNZ11$NFIP_ICCadj_YOLZ_W[DNZ11$NFIP_ICCadj_YOLZ_W >=2807394 ]<- 2807394
DNZ11$NFIP_ICCadj_YOLZ_W[DNZ11$NFIP_ICCadj_YOLZ_W <= 0.000  ]<-  0.000
range(DNZ11$NFIP_ICCadj_YOLZ_W, na.rm=T)
plot(DNZ11$NFIP_ICCadj_YOLZ_W)

DNZ11$NFIP_ICCadj_YOLZ_W_1M <- DNZ11$NFIP_ICCadj_YOLZ_W /1000000
range(DNZ11$NFIP_ICCadj_YOLZ_W_1M, na.rm=T)
hist(DNZ11$NFIP_ICCadj_YOLZ_W_1M)
plot(DNZ11$NFIP_ICCadj_YOLZ_W_10M)

##GOT HERE ####
#Need to rename the percents that are actually fractions as fractions
#Need to carefully select vars for analysis.

##TaxBaseEst_1B_W####
table(is.na(DNZ11$MHVadj))
range(DNZ11$MHVadj)
DNZ11$MHVadj_100k <- DNZ11$MHVadj /100000 #cannot preserve large numbers in multiplication so had to divide by 100k
range(DNZ11$MHVadj_100k)

DNZ11$TaxBaseEst_100k <- DNZ11$TotOccHU * DNZ11$MHVadj_100k
range(DNZ11$TaxBaseEst_100k)
table(is.na(DNZ11$TaxBaseEst_100k)) 
plot(DNZ11$TaxBaseEst_100k) #outlier

DNZ11$TaxBaseEst_1B <- DNZ11$TaxBaseEst_100k  / 10000
range(DNZ11$TaxBaseEst_1B)
plot(DNZ11$TaxBaseEst_1B)
plot(DNZ11$TaxBaseEst_1B, DNZ11$TotOccHU)
plot(DNZ11$MHVadj, DNZ11$TotOccHU) 

quantile(DNZ11$TaxBaseEst_1B , probs=c(0.01, 0.5, 0.99),na.rm=T)
DNZ11$TaxBaseEst_1B_W <- DNZ11$TaxBaseEst_1B 
DNZ11$TaxBaseEst_1B_W[DNZ11$TaxBaseEst_1B_W >=6.292628508   ]<- 6.292628508  
DNZ11$TaxBaseEst_1B_W[DNZ11$TaxBaseEst_1B_W <=0.006886499  ]<- 0.006886499
range(DNZ11$TaxBaseEst_1B_W,na.rm=T)
plot(DNZ11$TaxBaseEst_1B_W)

##HurricaneStorm binary####
table(DNZ11$incidentType)
DNZ11$HurrOrStorm <- 0
DNZ11$HurrOrStorm[DNZ11$incidentType=="Coastal Storm" |
                    DNZ11$incidentType=="Hurricane" |
                    DNZ11$incidentType=="Severe Ice Storm" |
                    DNZ11$incidentType=="Severe Storm"] <- 1
table(DNZ11$HurrOrStorm)



#Combine

##DATA REDUCTION ROUND 2####
#Vars I produced/ want:
  #WhitePct, fWhite, 
  #Decade_MedYrBuilt
  #MHIadj_W_10k
  #MHVadj_W_100k
  #OwnOccPct, fOwnOcc, 
  #PopDenseSqMile_W_1k
  #TotOccHU_W_10k

  #pct_fema_sfha, f_fema_sfha, 
  #count_fema_sfha_W_1k
  #fCoastal, CoastalPct,  Coastal_Binary
  #count_fs_risk_100_year00_W_1k
  #f_fs_risk_100_year00
  #f_fs_risk_500_year00
  #CSC

  #Acqui_RateBy_count_sfha_pct_W
  #Elev_RateBy_count_sfha_pct_W
  #Total_RateBy_count_sfha_pct_W

  #AcquiFedCostShareAdj_RateBy_countAcqui_10K_W
  #ElevFedCostShareAdj_RateBy_countElev_10K_W

  #IHP_fldDamAmountAdjDNZ_W_10M
  #IHP_fldDamAmountAdj_DNZ_RateBy_count_sfha_10k_W
  #IHP_rentalAssistanceAmountAdjDNZ_RateBy_count_sfha_10k_W
  #NFIP_AllClaimsAdj_YOLZ_W_10M
  #NFIP_AllClaimsAdj_YOLZ_RateBy_count_sfha_10k_W
  #NFIP_ICCadj_YOLZ_RateBy_count_sfha_10k_W
  #Combine_NFIP_IHPAdj_RateBy_count_sfha_10k_W
  
  #TaxBaseEst_1B_W
  #RepRate


#retaining final columns####
names(DNZ11)
sum(DNZ11$TotalProps)

DNZ12 <-DNZ11[,c("DNZ","DN","DD", "YOLZ","GZCTA","stateNumberCode", "p2state",
                 "incidentType","declarationTitle",
                 "Hurricane","Coastal", "Riverine", "Wind","HurrOrStorm",
                 "Event", "EventNum",
                 "TotalProps","Acqui", "Elev",
                 "facqui", "felev",
                 "CSC","ZCTA_shore",
                 "Coastal_Binary","fCoastal",
                 "count_fema_sfha","count_fema_sfha_W_1k",
                 "pct_fema_sfha", "f_fema_sfha",
                 "count_fs_risk_100_year00","count_fs_risk_100_year00_W_1k",
                 "pct_fs_risk_100_year00", "f_fs_risk_100_year00", 
                 "count_fs_risk_500_year00","count_fs_risk_500_year00_W_1k",
                 "pct_fs_risk_500_year00", "f_fs_risk_500_year00",
                 "AcquiFedCostShareAdj","ElevFedCostShareAdj","TotalFedCostShareAdj",
                 "Acqui_RateBy_count_sfha_pct_W",
                 "Elev_RateBy_count_sfha_pct_W",
                 "Total_RateBy_count_sfha_pct_W",
                 "AcquiFedCostShareAdj_RateBy_countAcqui_10K_W",
                 "ElevFedCostShareAdj_RateBy_countElev_10K_W",
                 "IHP_fldDamAmountAdjDNZ",
                 "IHP_fldDamAmountAdjDNZ_W_10M",
                 "IHP_fldDamAmountAdj_DNZ_RateBy_count_sfha_10k_W",
                 "IHP_rentalAssistanceAmountAdjDNZ",
                 "IHP_rentalAssistanceAmountAdjDNZ_RateBy_count_sfha_10k_W",
                 "NFIP_AllClaimsAdj_YOLZ",
                 "NFIP_AllClaimsAdj_YOLZ_W_10M",
                 "NFIP_AllClaimsAdj_YOLZ_RateBy_count_sfha_10k_W",
                 "NFIP_ICCadj_YOLZ",
                 "NFIP_ICCadj_YOLZ_W_1M",
                 "NFIP_ICCadj_YOLZ_RateBy_count_sfha_10k_W",
                 "Combine_NFIP_IHPAdj",
                 "Combine_NFIP_IHPAdj_RateBy_count_sfha_10k_W",
                 "NFIP_BuildClaimAdj_YOLZ",
                 "NFIP_ContClaimAdj_YOLZ",
                 "TotPop",
                 "TotPop_W_10k",
                 "PopDense_W_1k",
                 "WhitePct", "fWhite",
                 "BlackPct", "fBlack",
                 "HispPct", "fHisp",
                 "MHIadj_W_10k",
                 "MHVadj", "MHVadj_100k","MHVadj_W_100k",
                 "PopDenseSqMile_W_1k",
                 "TotOccHU_W_10k",
                 "OwnOccPct", "fOwnOcc",
                 "MedYrBuilt","Decade_MedYrBuilt",
                 "TaxBaseEst_1B_W",
                 "RepRate")]


names(DNZ12)

##subsetting out those with NAs####
colSums(is.na(DNZ12))
seeNAs <-  DNZ12[rowSums(is.na(DNZ12)) > 0, ]

sum(seeNAs$Elev) #31
sum(seeNAs$Acqui) #275

DNZ13 <- na.omit(DNZ12)
sum(DNZ13$TotalProps) #41041


##Creating StateNumLG####
names(DNZ13)
StateNumLG <- aggregate(DNZ13$TotalProps~DNZ13$p2state, FUN=sum)
names(StateNumLG)[names(StateNumLG)=="DNZ13$p2state"]<-"p2state"
names(StateNumLG)[names(StateNumLG)=="DNZ13$TotalProps"]<-"Total"

StateNumLG<-StateNumLG[order(StateNumLG$Total),]
StateNumLG2 <- as.data.frame(c(1:48))

StateNumLG <- cbind(StateNumLG, StateNumLG2)
names(StateNumLG)[names(StateNumLG)=="c(1:48)"]<- "StateNumLG"

StateNumLG <- StateNumLG[,c(1,3)]

DNZ14 <- left_join(DNZ13,StateNumLG, by ="p2state",copy=F)


#QC####
sum(DNZ14$TotalProps) #41041
sum(DNZ14$Elev) #5884
sum(DNZ14$Acqui) #35157


DNZ16 <- DNZ14


#cleaning remaining Events ####
Event <- aggregate(DNZ16$TotalProps~DNZ16$Event, FUN=sum)
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL STORM FAY"] <- "TROPICAL STORM FAY"
DNZ16$Event[DNZ16$declarationTitle %like% "HURRICANE FRAN"] <- "HURRICANE FRAN"
DNZ16$Event[DNZ16$declarationTitle == "HURRICANE FRAN AND ASSOCIATED HEAVY RAINS,FLOODING HIGHWIND"] <- "HURRICANE FRAN"
DNZ16$Event[DNZ16$declarationTitle %like% "HURRICANE JEANNE"] <- "HURRICANE JEANNE"
DNZ16$Event[DNZ16$declarationTitle %like% "TD GASTON"] <- "TROPICAL DEPRESSION GASTON"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL DEPRESSION ERNESTO"] <- "TROPICAL DEPRESSION ERNESTO"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL STORM CHARLEY"] <- "TROPICAL STORM CHARLEY"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL STORM DEBBY"] <- "TROPICAL STORM DEBBY"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL STORM HANNA"] <- "TROPICAL STORM HANNA"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL STORM JOSEPHINE"] <- "TROPICAL STORM JOSEPHINE"
DNZ16$Event[DNZ16$declarationTitle %like% "HURRICANE DORIAN"] <- "HURRICANE DORIAN"
DNZ16$Event[DNZ16$declarationTitle %like% "HURRICANE ERIN"] <- "HURRICANE ERIN"
DNZ16$Event[DNZ16$declarationTitle == "HURRICANE GEORGES - 18 SEP 98"] <- "HURRICANE GEORGES"
DNZ16$Event[DNZ16$declarationTitle %like% "HURRICANE HUGO"] <- "HURRICANE HUGO"
DNZ16$Event[DNZ16$declarationTitle %like% "HURRICANE ISAIAS"] <- "HURRICANE ISAIAS"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL DEPRESSION IVAN"] <- "TROPICAL DEPRESSION IVAN"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL STORM FRANCES"] <- "TROPICAL STORM FRANCES"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL STORM MICHAEL"] <- "TROPICAL STORM MICHAEL"
DNZ16$Event[DNZ16$declarationTitle %like% "TROPICAL STORM ZETA"] <- "TROPICAL STORM ZETA"
DNZ16$Event[DNZ16$declarationTitle == "SEVERE STORMS AND FLOODING ASSOC WITH TROPICAL STORM FRAN"] <- "TROPICAL STORM FRAN"
DNZ16$Event[DNZ16$Event == "Severe Storm"] <- "Severe Storm (unnamed)"
DNZ16$Event[DNZ16$Event == "Severe Ice Storm"] <- "Severe Ice Storm (unnamed)"
DNZ16$Event[DNZ16$Event == "Flood"] <- "Flood (unnamed)"
DNZ16$Event[DNZ16$DD == 1993 &  DNZ16$p2state !="Maine"] <- "Great Midwest Flood of 1993"


#incident Type####
table(DNZ16$incidentType)

DNZ16$incidentTypeB <- DNZ16$incidentType
table(DNZ16$incidentTypeB)
DNZ16$incidentTypeB[DNZ16$incidentType=="Coastal Storm" | #lumping coastal storm because it is all tropical storm allison
                      DNZ16$incidentType== "Severe Ice Storm" |
                      DNZ16$incidentType=="Severe Storm"] <- "Severe Storm (unnamed)" 

DNZ16$incidentTypeB[DNZ16$Event %like% "TROPICAL STORM"|
                      DNZ16$Event %like% "TROPICAL DEPRESSION"] <- "Tropical Storm"

DNZ16$SevereStorm <-0
DNZ16$SevereStorm [DNZ16$incidentTypeB=="Severe Storm (unnamed)"] <- 1
table(DNZ16$SevereStorm)

DNZ16$Flood <-0
DNZ16$Flood[DNZ16$incidentTypeB=="Flood"] <- 1
table(DNZ16$Flood)

DNZ16$TropicalStorm <-0
DNZ16$TropicalStorm[DNZ16$incidentTypeB=="Tropical Storm"] <- 1
table(DNZ16$TropicalStorm)

DNZ16$Hurricane <-0
DNZ16$Hurricane[DNZ16$incidentType=="Hurricane"] <- 1
table(DNZ16$Hurricane)

#Creating subsamples by Events ####
#working

names(DNZ16)
table(DNZ16$Event)
Event <- aggregate(DNZ16$TotalProps~DNZ16$Event, FUN=sum)
table(DNZ16$incidentType)
DecTitle <- as.data.frame(table(DNZ16$declarationTitle))

DNZ16$Sandy <-0
table(DNZ16$Event)
DNZ16$Sandy[DNZ16$Event=="HURRICANE SANDY"] <- 1

DNZ16$Allison<-0
table(DNZ16$Event)
DNZ16$Allison[DNZ16$Event=="TROPICAL STORM ALLISON"] <- 1

DNZ16$Harvey<-0
table(DNZ16$Event)
DNZ16$Harvey[DNZ16$Event=="HURRICANE HARVEY"] <- 1

DNZ16$Ike<-0
table(DNZ16$Event)
DNZ16$Ike[DNZ16$Event=="HURRICANE IKE"] <- 1

DNZ16$Katrina <-0
table(DNZ16$Event)
DNZ16$Katrina[DNZ16$Event=="HURRICANE KATRINA"] <- 1


DNZ16$Irene<-0
table(DNZ16$Event)
DNZ16$Irene[DNZ16$Event=="HURRICANE IRENE"] <- 1

DNZ16$Floyd<-0
table(DNZ16$Event)
DNZ16$Floyd[DNZ16$Event=="HURRICANE FLOYD"] <- 1

DNZ16$Fran<-0
table(DNZ16$Event)
DNZ16$Fran[DNZ16$Event=="HURRICANE FRAN"] <- 1

DNZ16$Matthew<-0
table(DNZ16$Event)
DNZ16$Matthew[DNZ16$Event=="HURRICANE MATTHEW"] <- 1

DNZ16$Lee<-0
table(DNZ16$Event)
DNZ16$Lee[DNZ16$Event=="TROPICAL STORM LEE"] <- 1

DNZ16$Maria <-0
table(DNZ16$Event)
DNZ16$Maria[DNZ16$Event=="HURRICANE MARIA"] <- 1 #NON EXISTANT!

DNZ16$GMF1993 <-0
table(DNZ16$Event)
DNZ16$GMF1993[DNZ16$Event=="Great Midwest Flood of 1993"] <- 1 


#by state#####
State <- aggregate(DNZ16$TotalProps~DNZ16$p2state, FUN=sum)

DNZ16$Texas <-0
table(DNZ16$Texas)
DNZ16$Texas[DNZ16$p2state=="Texas"] <- 1

DNZ16$Missouri <-0
DNZ16$Missouri[DNZ16$p2state=="Missouri"] <- 1

DNZ16$NorthCarolina <-0
DNZ16$NorthCarolina[DNZ16$p2state=="North Carolina"] <- 1

DNZ16$Illinois <-0
DNZ16$Illinois[DNZ16$p2state=="Iowa"] <- 1

DNZ16$Iowa <-0
DNZ16$Iowa[DNZ16$p2state=="Illinois"] <- 1

DNZ16$NewJersey<-0
DNZ16$NewJersey[DNZ16$p2state=="New Jersey"] <- 1

DNZ16$Louisiana<-0
DNZ16$Louisiana[DNZ16$p2state=="Louisiana"] <- 1


##DATA REDUCTION ROUND 3 ####
names(DNZ16)
DNZ16b <-DNZ16[,c("DNZ","DN","DD", "YOLZ","GZCTA","stateNumberCode", "p2state","StateNumLG",
                 "incidentTypeB","declarationTitle",
                 "Hurricane",
                 "Event", 
                 "TotalProps","Acqui", "Elev",
                 "facqui", "felev",
                 "CSC", "ZCTA_shore",
                 "count_fema_sfha","count_fema_sfha_W_1k",
                 "pct_fema_sfha", "f_fema_sfha",
                 "IHP_fldDamAmountAdjDNZ",
                 "IHP_fldDamAmountAdjDNZ_W_10M",
                 "NFIP_ICCadj_YOLZ",
                 "NFIP_ICCadj_YOLZ_W_1M",
                 "TotPop","TotPop_W_10k",
                 "WhitePct", "fWhite",
                 "BlackPct", "fBlack",
                 "HispPct", "fHisp",
                 "MHIadj_W_10k",
                 "MHVadj", "MHVadj_100k","MHVadj_W_100k",
                 "TotOccHU_W_10k",
                 "OwnOccPct", "fOwnOcc",
                 "RepRate",
                 "Sandy","Katrina","GMF1993"
                 )]

names(DNZ16b)

##Subsets####
DNZ16_Sandy <- subset(DNZ16, Sandy==1) ##113 DNZs, 2103 props 
DNZ16_Sandy_Coastal <- subset(DNZ16, Sandy==1 & Coastal_Binary==1) ##76 DNZs,

DNZ16_Allison <- subset(DNZ16, Allison==1) ##89 DNZs, 
DNZ16_Harvey <- subset(DNZ16, Harvey==1) ##91 DNZs, 
DNZ16_Ike <- subset(DNZ16, Ike==1) ##87 DNZs, 
DNZ16_Katrina <- subset(DNZ16, Katrina==1) ##119, 
DNZ16_Irene <- subset(DNZ16, Irene==1) ##169 DNZs,  props
DNZ16_Floyd <- subset(DNZ16, Floyd==1) ##92 DNZs,  props
DNZ16_Fran <- subset(DNZ16, Fran==1) ##76 DNZs,  props
DNZ16_Matthew <- subset(DNZ16, Matthew==1) ##79 DNZs,  props
DNZ16_Lee <- subset(DNZ16, Lee==1) ##77 DNZs,  props
DNZ16_Maria <- subset(DNZ16, Maria==1) ##0, 0 props
DNZ16_GMF1993 <- subset(DNZ16, GMF1993==1) ##224,  props

DNZ16_Coastal <- subset(DNZ16, Coastal_Binary==1) #441 DNZs, 
DNZ16_Coastal <- subset(DNZ16, Coastal > 0) #441 DNZs, 
DNZ16_Riverine <- subset(DNZ16, Riverine>0) #3901 DNZs, 

table(DNZ16$incidentTypeB)
DNZ16_Hurricane <- subset(DNZ16, incidentTypeB=="Hurricane") #1243 DNZs
DNZ16_SevereStorm <- subset(DNZ16, incidentTypeB=="Severe Storm (unnamed)") #1918 DNZs
DNZ16_Flood <- subset(DNZ16, incidentTypeB=="Flood") #907 DNZs
DNZ16_TropicalStorm <- subset(DNZ16, incidentTypeB=="Tropical Storm") #266 DNZs

DNZ16_Texas <- subset(DNZ16, Texas==1) #330, 
DNZ16_Missouri <- subset(DNZ16, Missouri==1) #193, 
DNZ16_NorthCarolina <- subset(DNZ16, NorthCarolina==1) #322, 
DNZ16_NewJersey <- subset(DNZ16, NewJersey==1) #111, 
DNZ16_Louisiana<- subset(DNZ16, Louisiana==1) #363, 

DNZ16_NJSandy <- subset(DNZ16, Sandy==1 & p2state=="New Jersey") ##62 DNZs, 
DNZ16_WOSandy <- subset(DNZ16, Sandy==0) ##4221 DNZs, 


DNZ16_LAKatrina <- subset(DNZ16, Katrina==1 & p2state=="Louisiana") ##93 DNZs, 
DNZ16_WOKatrina <- subset(DNZ16, Katrina==0) #4215 DNZa


names(DNZ16)

##by CSC####
DNZ16_CSC <- subset(DNZ16, CSC==1) #1324

##less Mix ####
DNZ16_lessMix <- subset(DNZ16, felev==0 | felev==1)

##less Mix ####
DNZ16_Mix <- subset(DNZ16, felev>0 & felev<1)

##After1993 ####
DNZ16_After1993 <- subset(DNZ16, DD>1993)


##Writing out files
path1 <- 'G:/My Drive/Academic/Research/Data/Fused/V3/FS/DNZLevel/DNZ16'
write.csv(DNZ16, file.path(path1, "DNZ16.csv"), row.names=TRUE)
write.csv(DNZ16b, file.path(path1, "DNZ16b.csv"), row.names=TRUE)


write.csv(DNZ16_Sandy, file.path(path1, "DNZ16_Sandy.csv"), row.names=TRUE)
write.csv(DNZ16_Sandy_Coastal, file.path(path1, "DNZ16_Sandy_Coastal.csv"), row.names=TRUE)
write.csv(DNZ16_NJSandy, file.path(path1, "DNZ16_NJSandy.csv"), row.names=TRUE)
write.csv(DNZ16_WOSandy, file.path(path1, "DNZ16_WOSandy.csv"), row.names=TRUE)

write.csv(DNZ16_Allison, file.path(path1, "DNZ16_Allison.csv"), row.names=TRUE)
write.csv(DNZ16_Harvey, file.path(path1, "DNZ16_Harvey.csv"), row.names=TRUE)
write.csv(DNZ16_Ike, file.path(path1, "DNZ16_Ike.csv"), row.names=TRUE)
write.csv(DNZ16_Katrina, file.path(path1, "DNZ16_Katrina.csv"), row.names=TRUE)
write.csv(DNZ16_LAKatrina, file.path(path1, "DNZ16_LAKatrina.csv"), row.names=TRUE)
write.csv(DNZ16_WOKatrina, file.path(path1, "DNZ16_WOKatrina.csv"), row.names=TRUE)
write.csv(DNZ16_Irene, file.path(path1, "DNZ16_Irene.csv"), row.names=TRUE)
write.csv(DNZ16_Floyd, file.path(path1, "DNZ16_Floyd.csv"), row.names=TRUE)
write.csv(DNZ16_Fran, file.path(path1, "DNZ16_Fran.csv"), row.names=TRUE)
write.csv(DNZ16_Matthew, file.path(path1, "DNZ16_Matthew.csv"), row.names=TRUE)
write.csv(DNZ16_Lee, file.path(path1, "DNZ16_Lee.csv"), row.names=TRUE)
write.csv(DNZ16_GMF1993, file.path(path1, "DNZ16_GMF1993.csv"), row.names=TRUE)


write.csv(DNZ16_Coastal, file.path(path1, "DNZ16_Coastal.csv"), row.names=TRUE)
write.csv(DNZ16_Riverine, file.path(path1, "DNZ16_Riverine.csv"), row.names=TRUE)

write.csv(DNZ16_Hurricane, file.path(path1, "DNZ16_Hurricane.csv"), row.names=TRUE)
write.csv(DNZ16_SevereStorm, file.path(path1, "DNZ16_SevereStorm.csv"), row.names=TRUE)
write.csv(DNZ16_Flood, file.path(path1, "DNZ16_Flood.csv"), row.names=TRUE)
write.csv(DNZ16_TropicalStorm, file.path(path1, "DNZ16_TropicalStorm.csv"), row.names=TRUE)

write.csv(DNZ16_Texas, file.path(path1, "DNZ16_Texas.csv"), row.names=TRUE)
write.csv(DNZ16_Missouri, file.path(path1, "DNZ16_Missouri.csv"), row.names=TRUE)
write.csv(DNZ16_NorthCarolina, file.path(path1, "DNZ16_NorthCarolina.csv"), row.names=TRUE)
write.csv(DNZ16_NewJersey, file.path(path1, "DNZ16_NewJersey.csv"), row.names=TRUE)
write.csv(DNZ16_Louisiana, file.path(path1, "DNZ16_Louisiana.csv"), row.names=TRUE)

write.csv(DNZ16_CSC, file.path(path1, "DNZ16_CSC.csv"), row.names=TRUE)

write.csv(DNZ16_lessMix, file.path(path1, "DNZ16_lessMix.csv"), row.names=TRUE)
write.csv(DNZ16_Mix, file.path(path1, "DNZ16_Mix.csv"), row.names=TRUE)

write.csv(DNZ16_After1993, file.path(path1, "DNZ16_After1993.csv"), row.names=TRUE)


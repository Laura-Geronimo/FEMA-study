# Setup
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)


# Importing data
myHMA4 <- read.csv('C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA/myHMA4.csv')
ZCTA_Census <- read.csv("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Census/TS_ZCTA_Tidy.csv")


#ZCTA_Census <- ZCTA_Census%>%
 # mutate(GZCTA = str_c('G', str_pad(GEOID, 5, pad = "0")))

#define fyDeclaredLess11
names(myHMA4)
range(myHMA4$fyDeclared)

myHMA4$fyDeclaredLess1 <- myHMA4$fyDeclared - 1
table(myHMA4$fyDeclaredLess1)

#joining data
myHMA5 <- left_join(myHMA4, ZCTA_Census, by="GZCTA")


##TotPop####
names(myHMA5)
head(myHMA5)
myHMA5$TotPop <- NA
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 <= 1999, myHMA5$y00_TotPop, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 >= 2000 & myHMA5$fyDeclaredLess1 <= 2009, myHMA5$y00_TotPop, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2010, myHMA5$y10_TotPop, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2011, myHMA5$y11_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2012, myHMA5$y12_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2013, myHMA5$y13_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2014, myHMA5$y14_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2015, myHMA5$y15_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2016, myHMA5$y16_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2017, myHMA5$y17_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2018, myHMA5$y18_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2019, myHMA5$y19_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2020, myHMA5$y20_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2021, myHMA5$y21_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2022, myHMA5$y22_TotPopE, myHMA5$TotPop)
myHMA5$TotPop <- ifelse(myHMA5$fyDeclaredLess1 == 2023, myHMA5$y22_TotPopE, myHMA5$TotPop)



names(myHMA5)
table(is.na(myHMA5$TotPop)) #167
test <- myHMA5 %>%
  select(GZCTA, fyDeclaredLess1, TotPop, y00_TotPop, y10_TotPop, y11_TotPopE, y12_TotPopE, y13_TotPopE, y14_TotPopE,
         y15_TotPopE,y16_TotPopE, y17_TotPopE, y18_TotPopE, y19_TotPopE, y20_TotPopE, y21_TotPopE, y22_TotPopE)


##WhitePct####
myHMA5$WhitePct <- NA
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 <= 1999, myHMA5$y00_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 >= 2000 & myHMA5$fyDeclaredLess1 <= 2009, myHMA5$y00_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2010, myHMA5$y10_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2011, myHMA5$y11_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2012, myHMA5$y12_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2013, myHMA5$y13_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2014, myHMA5$y14_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2015, myHMA5$y15_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2016, myHMA5$y16_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2017, myHMA5$y17_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2018, myHMA5$y18_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2019, myHMA5$y19_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2020, myHMA5$y20_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 == 2021, myHMA5$y21_WhitePct, myHMA5$WhitePct)
myHMA5$WhitePct <- ifelse(myHMA5$fyDeclaredLess1 >= 2022, myHMA5$y22_WhitePct, myHMA5$WhitePct)

table(is.na(myHMA5$WhitePct)) #168
names(myHMA5)
test <- myHMA5 %>%
  select(GZCTA, fyDeclaredLess1, WhitePct, y00_WhitePct, y10_WhitePct, y11_WhitePct, y12_WhitePct, y13_WhitePct, y14_WhitePct,
         y15_WhitePct,y16_WhitePct, y17_WhitePct, y18_WhitePct, y19_WhitePct, y20_WhitePct, y21_WhitePct, y22_WhitePct)

#good


##MHIadj####
##NOTES####
#no 1990 data so using 2000 as proxy
#no 2010 data, so using 2011 as proxy
names(myHMA5)
myHMA5$MHIadj <- NA
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 <= 2009, myHMA5$y00_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2010, myHMA5$y11_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2011, myHMA5$y11_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2012, myHMA5$y12_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2013, myHMA5$y13_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2014, myHMA5$y14_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2015, myHMA5$y15_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2016, myHMA5$y16_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2017, myHMA5$y17_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2018, myHMA5$y18_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2019, myHMA5$y19_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2020, myHMA5$y20_MHIE, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2021, myHMA5$y21_MHIadj, myHMA5$MHIadj)
myHMA5$MHIadj <- ifelse(myHMA5$fyDeclaredLess1 == 2022, myHMA5$y21_MHIadj, myHMA5$MHIadj)

table(is.na(myHMA5$MHIadj)) #174
test <- myHMA5 %>%
  select(GZCTA, fyDeclaredLess1, MHIadj, y00_MHIadj,  y11_MHIadj, y12_MHIadj, y13_MHIadj, y14_MHIadj,
         y15_MHIadj,y16_MHIadj, y17_MHIadj, y18_MHIadj, y19_MHIadj, y20_MHIE, y21_MHIadj, y22_MHIadj)


###################GOT HERE ####################
#would need to go back and make sure I pull the population density variable into the dataset

##PopDenseSqMile####
#no 90 data so using 2000 as proxy
names(myHMA5)
colSums(is.na(myHMA5))
myHMA5$PopDenseSqMile <- NA
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 <= 2009, myHMA5$y00_PopDenseSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2010, myHMA5$y10_PopDensSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2011, myHMA5$y11_PopDenseSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2012, myHMA5$y12_PopDensSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2013, myHMA5$y13_PopDenseSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2014, myHMA5$y14_PopDenseSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2015, myHMA5$y15_PopDense, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2016, myHMA5$y16_PopDenseSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2017, myHMA5$y17_PopDenseSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2018, myHMA5$y18_PopDenseSqMile, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 == 2019, myHMA5$y19_PopDense, myHMA5$PopDenseSqMile)
myHMA5$PopDenseSqMile <- ifelse(myHMA5$fyDeclaredLess1 >= 2020, myHMA5$y20_PopDenseSqMile, myHMA5$PopDenseSqMile)

table(is.na(myHMA5$PopDenseSqMile)) #167


##TotOccHU####
names(myHMA5)
myHMA5$TotOccHU <- NA
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 <= 1999, myHMA5$y90_TotHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2000, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2001, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2002, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2003, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2004, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2005, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2006, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2007, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2008, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2009, myHMA5$y00_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2010, myHMA5$y10_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2011, myHMA5$y11_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2012, myHMA5$y12_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2013, myHMA5$y13_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2014, myHMA5$y14_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2015, myHMA5$y15_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2016, myHMA5$y16_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2017, myHMA5$y17_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2018, myHMA5$y18_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 == 2019, myHMA5$y19_TotOccHU, myHMA5$TotOccHU)
myHMA5$TotOccHU <- ifelse(myHMA5$fyDeclaredLess1 >= 2020, myHMA5$y20_TotOccHU, myHMA5$TotOccHU)

table(is.na(myHMA5$TotOccHU)) #170


##OwnOccPct####
myHMA5$OwnOccPct <- NA
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 <= 1999, myHMA5$y90_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2000, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2001, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2002, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2003, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2004, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2005, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2006, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2007, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2008, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2009, myHMA5$y00_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2010, myHMA5$y10_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2011, myHMA5$y11_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2012, myHMA5$y12_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2013, myHMA5$y13_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2014, myHMA5$y14_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2015, myHMA5$y15_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2016, myHMA5$y16_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2017, myHMA5$y17_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2018, myHMA5$y18_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 == 2019, myHMA5$y19_OwnOccPct, myHMA5$OwnOccPct)
myHMA5$OwnOccPct <- ifelse(myHMA5$fyDeclaredLess1 >= 2020, myHMA5$y20_OwnOccPct, myHMA5$OwnOccPct)

table(is.na(myHMA5$OwnOccPct)) #168


##MedYrBuilt####
#no 1990 data so using 2000 as proxy
#No data for 2010, so using 2011 as proxy
myHMA5$MedYrBuilt <- NA
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 <= 2009, myHMA5$y00_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2010, myHMA5$y11_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2011, myHMA5$y11_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2012, myHMA5$y12_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2013, myHMA5$y13_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2014, myHMA5$y14_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2015, myHMA5$y15_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2016, myHMA5$y16_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2017, myHMA5$y17_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2018, myHMA5$y18_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 == 2019, myHMA5$y19_MedYrBuilt, myHMA5$MedYrBuilt)
myHMA5$MedYrBuilt <- ifelse(myHMA5$fyDeclaredLess1 >= 2020, myHMA5$y20_MedYrBuilt, myHMA5$MedYrBuilt)

table(is.na(myHMA5$MedYrBuilt)) #168


##MHVadj####
#no 1990 data so using 2000 as proxy
#no 2010 data, so using 2011 as proxy
myHMA5$MHVadj <- NA
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 <= 2009, myHMA5$y00MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2010, myHMA5$y11MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2011, myHMA5$y11MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2012, myHMA5$y12MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2013, myHMA5$y13MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2014, myHMA5$y14MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2015, myHMA5$y15MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2016, myHMA5$y16MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2017, myHMA5$y17MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2018, myHMA5$y18MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 == 2019, myHMA5$y19MHVadj, myHMA5$MHVadj)
myHMA5$MHVadj <- ifelse(myHMA5$fyDeclaredLess1 >= 2020, myHMA5$y20_MHV, myHMA5$MHVadj)

table(is.na(myHMA5$MHVadj)) #74

colSums(is.na(myHMA5)) #for Time series, missing up to 174 

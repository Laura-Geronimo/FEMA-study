# Setup
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)


# Importing data
myHMA6 <- read.csv('C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA/myHMA6.csv')
ZCTA_Census <- read.csv("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Census/TS_ZCTA_Tidy.csv")


#joining data
myHMA7 <- left_join(myHMA6, ZCTA_Census, by="GZCTA")


#define fyDeclaredLess11
names(myHMA7)
range(myHMA7$fyDeclared)

myHMA7$fyDeclaredLess1 <- myHMA7$fyDeclared - 1
table(myHMA7$fyDeclaredLess1)

##TotPop####
names(myHMA7)
head(myHMA7)
myHMA7$TotPop <- NA
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_TotPop, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_TotPop, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_TotPop, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2022, myHMA7$y22_TotPopE, myHMA7$TotPop)
myHMA7$TotPop <- ifelse(myHMA7$fyDeclaredLess1 == 2023, myHMA7$y22_TotPopE, myHMA7$TotPop)

names(myHMA7)
table(is.na(myHMA7$TotPop)) #4441
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, TotPop, y90_TotPop, y00_TotPop, y10_TotPop, y11_TotPopE, y12_TotPopE, y13_TotPopE, y14_TotPopE,
         y15_TotPopE,y16_TotPopE, y17_TotPopE, y18_TotPopE, y19_TotPopE, y20_TotPopE, y21_TotPopE, y22_TotPopE)


##WhitePct####
myHMA7$WhitePct <- NA
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_WhitePct, myHMA7$WhitePct)
myHMA7$WhitePct <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_WhitePct, myHMA7$WhitePct)

table(is.na(myHMA7$WhitePct)) #4443
names(myHMA7)
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, WhitePct, y90_WhitePct, y00_WhitePct, y10_WhitePct, y11_WhitePct, y12_WhitePct, y13_WhitePct, y14_WhitePct,
         y15_WhitePct,y16_WhitePct, y17_WhitePct, y18_WhitePct, y19_WhitePct, y20_WhitePct, y21_WhitePct, y22_WhitePct)


##BlackPct####
myHMA7$BlackPct <- NA
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_BlackPct, myHMA7$BlackPct)
myHMA7$BlackPct <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_BlackPct, myHMA7$BlackPct)

table(is.na(myHMA7$BlackPct)) #4443
names(myHMA7)
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, BlackPct, y90_BlackPct, y00_BlackPct, y10_BlackPct, y11_BlackPct, y12_BlackPct, y13_BlackPct, y14_BlackPct,
         y15_BlackPct,y16_BlackPct, y17_BlackPct, y18_BlackPct, y19_BlackPct, y20_BlackPct, y21_BlackPct, y22_BlackPct)



##HispPct####
myHMA7$HispPct <- NA
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_HispPct, myHMA7$HispPct)
myHMA7$HispPct <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_HispPct, myHMA7$HispPct)

table(is.na(myHMA7$HispPct)) #4443
names(myHMA7)
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, HispPct, y90_HispPct, y00_HispPct, y10_HispPct, y11_HispPct, y12_HispPct, y13_HispPct, y14_HispPct,
         y15_HispPct,y16_HispPct, y17_HispPct, y18_HispPct, y19_HispPct, y20_HispPct, y21_HispPct, y22_HispPct)



##MHIadj####
## notes####
#no 1990 data so using 2000 as proxy
#no 2010 data, so using 2011 as proxy
names(myHMA7)
myHMA7$MHIadj <- NA
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y11_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_MHIE, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_MHIadj, myHMA7$MHIadj)
myHMA7$MHIadj <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y21_MHIadj, myHMA7$MHIadj)

table(is.na(myHMA7$MHIadj)) #4211
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, MHIadj, y00_MHIadj,  y11_MHIadj, y12_MHIadj, y13_MHIadj, y14_MHIadj,
         y15_MHIadj,y16_MHIadj, y17_MHIadj, y18_MHIadj, y19_MHIadj, y20_MHIE, y21_MHIadj, y22_MHIadj)


##MHVadj####
##notes####
#no 1990 data so using 2000 as proxy
#no 2010 data, so using 2011 as proxy
names(myHMA7)
myHMA7$MHVadj <- NA
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y11_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_MHVE, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_MHVadj, myHMA7$MHVadj)
myHMA7$MHVadj <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y21_MHVadj, myHMA7$MHVadj)

table(is.na(myHMA7$MHVadj)) #4211
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, MHVadj, y00_MHVadj,  y11_MHVadj, y12_MHVadj, y13_MHVadj, y14_MHVadj,
         y15_MHVadj,y16_MHVadj, y17_MHVadj, y18_MHVadj, y19_MHVadj, y20_MHVE, y21_MHVadj, y22_MHVadj)


##PopDenseSqMile####
#no 90 data so using 2000 as proxy
names(myHMA7)
colSums(is.na(myHMA7))
myHMA7$PopDenseSqMile <- NA
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_PopDenseSqMile, myHMA7$PopDenseSqMile)
myHMA7$PopDenseSqMile <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_PopDenseSqMile, myHMA7$PopDenseSqMile)

table(is.na(myHMA7$PopDenseSqMile)) #4032


##TotHU####
names(myHMA7)
myHMA7$TotHU <- NA
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_TotHU, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_TotHU, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_TotHU, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_TotHUE, myHMA7$TotHU)
myHMA7$TotHU <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_TotHUE, myHMA7$TotHU)

table(is.na(myHMA7$TotHU)) #4441
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, TotHU, y90_TotHU, y00_TotHU,  y11_TotHUE, y12_TotHUE, y13_TotHUE, y14_TotHUE,
         y15_TotHUE,y16_TotHUE, y17_TotHUE, y18_TotHUE, y19_TotHUE, y20_TotHUE, y21_TotHUE, y22_TotHUE)



##TotOccHU####
names(myHMA7)
myHMA7$TotOccHU <- NA
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_TotOcc_HU, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_TotOccHU, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_TotOccHU, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_TotOccHUE, myHMA7$TotOccHU)
myHMA7$TotOccHU <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_TotOccHUE, myHMA7$TotOccHU)

table(is.na(myHMA7$TotOccHU)) #4441
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, TotOccHU, y90_TotOcc_HU, y00_TotOccHU,  y11_TotOccHUE, y12_TotOccHUE, y13_TotOccHUE, y14_TotOccHUE,
         y15_TotOccHUE,y16_TotOccHUE, y17_TotOccHUE, y18_TotOccHUE, y19_TotOccHUE, y20_TotOccHUE, y21_TotOccHUE, y22_TotOccHUE)


##OwnOccPct####
myHMA7$OwnOccPct <- NA
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_OwnOccPct, myHMA7$OwnOccPct)
myHMA7$OwnOccPct <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_OwnOccPct, myHMA7$OwnOccPct)

table(is.na(myHMA7$OwnOccPct)) #4444
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, OwnOccPct, y90_OwnOccPct, y00_OwnOccPct,  y11_OwnOccPct, y12_OwnOccPct, y13_OwnOccPct, y14_OwnOccPct,
         y15_OwnOccPct,y16_OwnOccPct, y17_OwnOccPct, y18_OwnOccPct, y19_OwnOccPct, y20_OwnOccPct, y21_OwnOccPct, y22_OwnOccPct)

##RentOccPct####
myHMA7$RentOccPct <- NA
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_RentOccPct, myHMA7$RentOccPct)
myHMA7$RentOccPct <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_RentOccPct, myHMA7$RentOccPct)

table(is.na(myHMA7$RentOccPct)) #4444
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, RentOccPct, y90_RentOccPct, y00_RentOccPct,  y11_RentOccPct, y12_RentOccPct, y13_RentOccPct, y14_RentOccPct,
         y15_RentOccPct,y16_RentOccPct, y17_RentOccPct, y18_RentOccPct, y19_RentOccPct, y20_RentOccPct, y21_RentOccPct, y22_RentOccPct)


##2ndHomePct####
myHMA7$S2ndHomePct <- NA
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 <= 1999, myHMA7$y90_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 >= 2000 & myHMA7$fyDeclaredLess1 <= 2009, myHMA7$y00_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2010, myHMA7$y10_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2011, myHMA7$y11_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2012, myHMA7$y12_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2013, myHMA7$y13_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2014, myHMA7$y14_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2015, myHMA7$y15_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2016, myHMA7$y16_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2017, myHMA7$y17_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2018, myHMA7$y18_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2019, myHMA7$y19_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2020, myHMA7$y20_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 == 2021, myHMA7$y21_2ndHomePct, myHMA7$S2ndHomePct)
myHMA7$S2ndHomePct <- ifelse(myHMA7$fyDeclaredLess1 >= 2022, myHMA7$y22_2ndHomePct, myHMA7$S2ndHomePct)

table(is.na(myHMA7$S2ndHomePct)) #4444
test <- myHMA7 %>%
  select(GZCTA, fyDeclaredLess1, S2ndHomePct, y90_2ndHomePct, y00_2ndHomePct,  y11_2ndHomePct, y12_2ndHomePct, y13_2ndHomePct, y14_2ndHomePct,
         y15_2ndHomePct,y16_2ndHomePct, y17_2ndHomePct, y18_2ndHomePct, y19_2ndHomePct, y20_2ndHomePct, y21_2ndHomePct, y22_2ndHomePct)

#Selecting Vars ####
variable_names <- colnames(myHMA7)
names(myHMA7)
myHMA8 <- myHMA7 %>%
  select(id, zip, city, county, p2county, p2countyCode, stateNumberCode,
         p2state, region, p2region, projectIdentifier, propertyAction,
         propertyAction3, structureType, typeOfResidency, typeOfResidency2,
         foundationType, programArea, programFy, actualAmountPaid,
         disasterNumber, myHazard, damageCategory, p2projectType, p2status,
         p2subrecipient, p2dateApproved, p2dateClosed, p2projectAmount,
         p2federalShareObligated, p2subrecipientAdminCostAmt,
         p2recipientAdminCostAmt, p2costSharePercentage, p2benefitCostRatio,
         p2netValueBenefits, p2numberOfFinalProperties, GZCTA, DN,
         declarationType, fyDeclared, incidentType, declarationTitle,
         Hurricane, SevereStorm, TropicalStorm, CoastalStorm, Tornado,
         Wind, WinterStorm, GroundSaturation, Flood, Mudslide, Landslide,
         HighSurf, TorrentialRain, HurricaneName, TropicalStormName,
         fyDeclaredLess1, TotPop, WhitePct, BlackPct, HispPct, MHIadj, MHVadj,
         PopDenseSqMile, TotHU, TotOccHU, OwnOccPct, RentOccPct, S2ndHomePct)
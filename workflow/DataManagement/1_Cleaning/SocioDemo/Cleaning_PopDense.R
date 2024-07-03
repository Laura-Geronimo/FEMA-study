## combining sociodemographic data derived from 

  # 1) Tidy Census
  # 2) 1990 NHGIS
  # 3) Social explorer (population density data)

#Setup
library(dplyr)
library(data.table)
library(stringr)

#reading data ####
SE <- read.csv("H:/My Drive/Scarlet/Academic/Research/Data/Census/SocialExplorerPull/Produced/ZCTA_Census_SE.csv")
y21_pd <- read.csv("H:/My Drive/Scarlet/Academic/Research/Data/Census/SocialExplorerPull/ZCTA_2021_acs/ACS_2021_PopDense.csv")
y22_pd <- read.csv("H:/My Drive/Scarlet/Academic/Research/Data/Census/SocialExplorerPull/ZCTA_2022_acs/ACS_2022_PopDense.csv")


#cleaning ####
names(SE)

names(y21_pd)
names(y22_pd)

y21_pd  <- y21_pd [-1, ]
names(y21_pd)[names(y21_pd)=="Population.Density..Per.Sq..Mile."] <- "y21_PopDenseSqMile"
y21_pd$GZCTA <- str_c("G",y21_pd$FIPS)
y21_pd <- y21_pd %>%
  select(GZCTA, y21_PopDenseSqMile)

y22_pd  <- y22_pd [-1, ]
names(y22_pd)[names(y22_pd)=="Population.Density..Per.Sq..Mile."] <- "y22_PopDenseSqMile"
y22_pd$GZCTA <- str_c("G",y22_pd$FIPS)
y22_pd <- y22_pd %>%
  select(GZCTA, y22_PopDenseSqMile)

names(SE)
names(SE)[names(SE)=="y10_PopDensSqMile"] <- "y10_PopDenseSqMile"
names(SE)[names(SE)=="y12_PopDensSqMile"] <- "y12_PopDenseSqMile"
names(SE)[names(SE)=="y15_PopDense"] <- "y15_PopDenseSqMile"
names(SE)[names(SE)=="y19_PopDense"] <- "y19_PopDenseSqMile"


SE_PD <- SE %>%
  select(GZCTA,
         y00_PopDenseSqMile,
         y10_PopDenseSqMile,
         y11_PopDenseSqMile,
         y12_PopDenseSqMile,
         y13_PopDenseSqMile,
         y14_PopDenseSqMile,
         y15_PopDenseSqMile,
         y16_PopDenseSqMile,
         y17_PopDenseSqMile,
         y18_PopDenseSqMile,
         y19_PopDenseSqMile,
         y20_PopDenseSqMile
         )

SE_PD <- left_join(SE_PD, y21_pd, by="GZCTA")
SE_PD <- left_join(SE_PD, y22_pd, by="GZCTA")

names(SE_PD)

#writing out files ####
path1 <- 'C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Census'
write.csv(SE_PD, file.path(path1, "SE_PopDense00_22.csv"), row.names=TRUE)

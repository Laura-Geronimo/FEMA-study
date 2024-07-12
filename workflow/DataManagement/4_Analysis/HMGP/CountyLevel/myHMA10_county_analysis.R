

getwd()
setwd('C:/Users/lgero/Box/Research/FEMA_project')

library(ggplot2)
library(tidycensus)
library(tidyverse)
library(mapview)
library(classInt)


options(scipen =999)

#importing data####
Cnty <- read.csv('C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA/HMGP_Agg/County_HMGP.csv',header=T)

US_County <-get_acs("county",variables = "B25077_001", 
                    year=2019,
                    geometry = TRUE)

cnty_FS <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/Firststreet/Aggregated/V2/fsf_flood_county_summary.csv')
CSC <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/NOAA/Produced/CSCs_wo_Lakes_cnty_info_clean.csv')


#cleaning ###
names(Cnty)
head(Cnty$CntyID)

names(US_County)
head(US_County)
range(US_County$GEOID)
US_County$CntyID <- str_c("C", US_County$GEOID)

US_County <- US_County[,c("NAME", "geometry", "CntyID")] 

names(cnty_FS)
head(cnty_FS$fips)
cnty_FS$CntyID <- str_pad(cnty_FS$fips, 5, pad="0")
cnty_FS$CntyID <- str_c("C", cnty_FS$CntyID)
head(cnty_FS$CntyID)

names(CSC)
head(CSC$cnty_fips)
CSC$CntyID <- str_pad(CSC$cnty_fips, 5, pad="0")
CSC$CntyID <- str_c("C", CSC$CntyID)
head(CSC$CntyID)
head(CSC)
CSC$CSC <- 1
CSC <- CSC[,c("CntyID", "CSC")]

#Joining data
U_Cnty <- left_join(US_County, Cnty, by="CntyID")
U_Cnty <- left_join(U_Cnty, cnty_FS, by="CntyID")
U_Cnty <- left_join(U_Cnty, CSC, by="CntyID")


#developing rate by cnty_fema_SFHA
U_Cnty$Acqui_RateBy_count_fema_sfha <- U_Cnty$Acqui / U_Cnty$count_fema_sfha
U_Cnty$Elev_RateBy_count_fema_sfha <- U_Cnty$Elev / U_Cnty$count_fema_sfha


###Checking inf values####
#caused because there are places where count_fema_sfha = 0
#chaning to NaN
table(is.infinite(U_Cnty$Acqui_RateBy_count_fema_sfha)) #16
table(is.infinite(U_Cnty$Elev_RateBy_count_fema_sfha)) #4

#Managing infinite NAN####
U_Cnty$Acqui_RateBy_count_fema_sfha[sapply(U_Cnty$Acqui_RateBy_count_fema_sfha, is.infinite)]<- NaN
U_Cnty$Elev_RateBy_count_fema_sfha[sapply(U_Cnty$Elev_RateBy_count_fema_sfha, is.infinite)]<- NaN

class(U_Cnty$Acqui_RateBy_count_fema_sfha)
U_Cnty$Acqui_RateBy_count_fema_sfha <- as.numeric(U_Cnty$Acqui_RateBy_count_fema_sfha)

##Subset to CSCs ####
U_Cnty$CSC[is.na(U_Cnty$CSC)] <- 0
table(U_Cnty$CSC)
U_Cnty_CSC <- subset(U_Cnty, CSC==1)

##Barchart Rates####
ggplot(data= U_Cnty_CSC %>% slice_max(Acqui_RateBy_count_fema_sfha, n = 10), #This pipe allows you to slice the top 10 data without having to subset it
       aes(fct_reorder(NAME, Acqui_RateBy_count_fema_sfha),  #this part orders the columns
           Acqui_RateBy_count_fema_sfha))+
  geom_bar(stat="identity") +
  labs(y="Rate of Acquisitions", x="",
       title="Top 10 Coastal Counties by \nRate of Acquisitions per homes in FEMA SFHA \n(1989-2022)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(1, 1, 1, 1, "cm"))


ggplot(data= U_Cnty_CSC %>% slice_max(Elev_RateBy_count_fema_sfha, n = 10), #This pipe allows you to slice the top 10 data without having to subset it
       aes(fct_reorder(NAME, Elev_RateBy_count_fema_sfha),  #this part orders the columns
           Elev_RateBy_count_fema_sfha))+
  geom_bar(stat="identity") +
  labs(y="Rate of Elevations", x="",
       title="Top 10 Coastal Counties by \nRate of Elevations per homes in FEMA SFHA \n(1989-2022)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(1, 1, 1, 1, "cm"))

##Barchart Counts####
ggplot(data= U_Cnty_CSC %>% slice_max(Acqui, n = 10), #This pipe allows you to slice the top 10 data without having to subset it
       aes(fct_reorder(NAME, Acqui),  #this part orders the columns
           Acqui))+
  geom_bar(stat="identity") +
  labs(y="Count of Acquisitions", x="",
       title="Top 10 Coastal Counties by \nCount of Acquisitions (1989-2022)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(1, 1, 1, 1, "cm"))

ggplot(data= U_Cnty_CSC %>% slice_max(Elev, n = 10), #This pipe allows you to slice the top 10 data without having to subset it
       aes(fct_reorder(NAME, Elev),  #this part orders the columns
           Elev))+
  geom_bar(stat="identity") +
  labs(y="Count of Elevations", x="",
       title="Top 10 Coastal Counties by \nCount of Elevations (1989-2022)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(1, 1, 1, 1, "cm"))


ggplot(data= U_Cnty_CSC %>% slice_max(Elev_RateBy_count_fema_sfha, n = 10), #This pipe allows you to slice the top 10 data without having to subset it
       aes(fct_reorder(NAME, Elev_RateBy_count_fema_sfha),  #this part orders the columns
           Elev_RateBy_count_fema_sfha))+
  geom_bar(stat="identity") +
  labs(y="Rate of Elevations", x="",
       title="Top 10 Coastal Counties by \nRate of Elevations per homes in FEMA SFHA \n(1989-2022)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(1, 1, 1, 1, "cm"))




#mapping ####
names(U_Cnty)
# Calculate Jenks natural breaks
jenks_breaks <- classIntervals(U_Cnty_CSC$Acqui_RateBy_count_fema_sfha, n = 10, style = "jenks")$brks


# Create the plot using ggplot2
ggplot()+
  geom_sf(data=U_Cnty_CSC, aes(fill=Acqui_RateBy_count_fema_sfha, color="white")) +
  scale_fill_gradient(low = "lightblue", high = "red", na.value="grey",
                      name="Rates of \nHMGP \nAcquisitions") +
  scale_size(0.05)+
  scale_color_manual(values='transparent', guide="none")+
  ggtitle("Rates of HMGP Acquisitions for Flood Related Events \n(by counts of HUs in FEMA SFHA)(W)\nby County (1989-2022)")+
  #geom_sf(data=state_shp, fill="transparent", color="black", lwd=0.01)+
  coord_sf(xlim=c(-130,-60), ylim=c(+23,+50))+
  theme(plot.title = element_text(hjust=0.5))



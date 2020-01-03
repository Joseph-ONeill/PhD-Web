# Joining the dataframe for Popa Dataset of 2007.

# Loading the all required packages

library(tidyverse)
library(lubridate)
library(BIOMASS)
library(ape)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(maps)
library(mapdata)
library(maptools)
library(sp)
library(raster)
library(BIOMASS)
library(ape)
library(magrittr)
library(tinytex)
library(raster)
library(rgdal)
library(measurements)
library(lme4)
library(bbmle)

First_Year_Forests <- read.csv("Data/All_data_Final.csv", stringsAsFactors = F)
Popa_Forests <- First_Year_Forests %>% filter(Forest_type!="Moist_Mixed_Deciduous_Forest")

NA_Rows <- Popa_Forests %>% filter(!complete.cases(.))

NA_Rows$Species_names[is.na(NA_Rows$Species_names)] <- "Unknown species"
NA_Rows$Genus[is.na(NA_Rows$Genus)] <- "Unknown"
NA_Rows$Species[is.na(NA_Rows$Species)] <- "species"
NA_Rows$Family[is.na(NA_Rows$Family)] <- "Unknown"
NA_Rows_Completed <- NA_Rows

Complete_Rows <- Popa_Forests %>% filter(complete.cases(.))

Popa_Forests_Full <- rbind(Complete_Rows, NA_Rows_Completed)

Popa_Forests_Full <- Popa_Forests_Full %>% dplyr::select(Forest_type, Plot_id, Genus, Species, Dbh_cm,Long, Lat, H_m) %>% rename(DBH_cm=Dbh_cm) %>% rename(H_ft= H_m) %>% mutate(H_m= H_ft*0.3048) %>% mutate(Year=paste(2007))

Taxo <- correctTaxo(genus= Popa_Forests_Full$Genus, species = Popa_Forests_Full$Species)
Popa_Forests_Full$genusCorr <- Taxo$genusCorrected
Popa_Forests_Full$speciesCorr <- Taxo$speciesCorrected

APG <- getTaxonomy(Popa_Forests_Full$genusCorr, findOrder =T)

Popa_Forests_Full$familyAPG <- APG$family

Popa_Forests_Full$orderAPG <- APG$order
dataWD <- getWoodDensity(genus=Popa_Forests_Full$genusCorr,
                         species=Popa_Forests_Full$speciesCorr,
                         stand=NULL, family = Popa_Forests_Full$familyAPG, region = "World")

Popa_Forests_Full <- Popa_Forests_Full %>% mutate(WD=dataWD$meanWD)

CoordsH <- cbind(Popa_Forests_Full$Long, Popa_Forests_Full$Lat)
Popa_Forests_Full <- Popa_Forests_Full %>% mutate(AGB=computeAGB(D = DBH_cm, WD = WD, H = H_m, coord = CoordsH, Dlim = NULL))
Popa_Forests_Full <- Popa_Forests_Full %>% mutate(Carbon_Mg= AGB*0.471)

Plot_Popa_Forests <- Popa_Forests_Full %>% 
  group_by(Plot_id,Forest_type,Long,Lat,Year) %>%
  dplyr::summarise(C_Tree_total = sum(Carbon_Mg)) %>%
  mutate(C_ha=C_Tree_total/0.04) %>% dplyr::select(-C_Tree_total)

Carbon_07 <- Plot_Popa_Forests %>% group_by(Plot_id) %>% summarise(Carbon_ha = sum(C_ha)) %>% rename(Plot = Plot_id)

NDVI2007_Popa_4Forests <-raster("Data/NDVI2007_Popa_4Forests.tif")
mypts <-readOGR("Four_Forests_shapefile/FourForests.shp")
NDVI2007_4Popa_Forests <- trim(NDVI2007_Popa_4Forests)
plot(NDVI2007_Popa_4Forests)
mydata <- raster::extract(NDVI2007_Popa_4Forests, mypts)
plot(mypts, add = TRUE, cex=0.1)

class(mydata)
NDVI2007_values <- as.data.frame(mydata)
NDVI2007 <- NDVI2007_values %>% rename(NDVI2007=mydata)
NDVI2007_Popa_4Forests <- bind_cols(Plot_Popa_Forests,NDVI2007)
NDVI2007_Popa_4Forests <- NDVI2007_Popa_4Forests %>% rename(Plot=Plot_id)

NDVI_07 <- NDVI2007_Popa_4Forests[!duplicated(NDVI2007_Popa_4Forests$Plot),]

Carbon_Ndvi_07 <- inner_join(Carbon_07, NDVI_07) %>% dplyr::select(-C_ha)


EVI2007_Popa_4Forests <-raster("Data/EVI2007_Popa_4Forests.tif")
mypts <-readOGR("Four_Forests_shapefile/FourForests.shp")
EVI2007_4Popa_Forests <- trim(EVI2007_Popa_4Forests)
plot(EVI2007_Popa_4Forests)
mydata <- raster::extract(EVI2007_Popa_4Forests, mypts)
plot(mypts, add = TRUE, cex=0.1)

class(mydata)
EVI2007_values <- as.data.frame(mydata)
EVI2007 <- EVI2007_values %>% rename(EVI2007=mydata)
EVI2007_Popa_4Forests <- bind_cols(Plot_Popa_Forests,EVI2007)
EVI2007_Popa_4Forests <- EVI2007_Popa_4Forests %>% rename(Plot=Plot_id)

EVI_07 <- EVI2007_Popa_4Forests[!duplicated(EVI2007_Popa_4Forests$Plot),]

EVI_07 <- EVI_07 %>% ungroup() %>% dplyr::select(c(Plot,EVI2007))


Carbon_Ndvi_Evi_07 <- left_join(Carbon_Ndvi_07, EVI_07, by='Plot')

WCL_Popa_4Forests <- read.csv("Data_Output/WCL_Popa_4Forests.csv", stringsAsFactors = F) %>% rename(Plot = Plot_id)

WCL_07 <- WCL_Popa_4Forests[!duplicated(WCL_Popa_4Forests$Plot),]
WCL_07 <- WCL_07 %>% dplyr::select(-c(Forest_type, Lat, Long, Year,C_ha))

Carbon_Ndvi_Evi_Wcl_07 <- left_join(Carbon_Ndvi_Evi_07, WCL_07, by="Plot")


Avt_Carbon_Popa_4Forests <- read.csv("Data_Output/Avitabile_AGB_Popa_4Forests.csv", stringsAsFactors = F)
Avt_Carbon_Popa_4Forests <- Avt_Carbon_Popa_4Forests %>% rename(Avt_Carbon = mean) %>% rename(Plot = Plot_id)
AVT_07 <- Avt_Carbon_Popa_4Forests[!duplicated(Avt_Carbon_Popa_4Forests$Plot),]
AVT_07 <- AVT_07 %>% dplyr::select(-c(Forest_type, Lat, Long, Year,C_ha))

Carbon_Ndvi_Evi_Wcl_Avt_07 <- left_join(Carbon_Ndvi_Evi_Wcl_07, AVT_07, by="Plot")

# Join with Aelvation_SRTM_07 data

Elevation_07 <- read.csv("Data_Output/SRTM_Popa2007_ele_as_sl.csv", stringsAsFactors = F)

Elevation_07 <- Elevation_07[!duplicated(Elevation_07$Plot_id),] 


Elevation_07 <- Elevation_07 %>% 
  rename(Plot = Plot_id) %>% 
  mutate(Aspect_direction = case_when(Aspect_deg == 0 ~ 'N', 
                                      Aspect_deg == 360 ~ 'N',
                                      between(Aspect_deg,0.1,44.9)  ~ 'NNE',
                                      Aspect_deg == 45 ~ 'NE',
                                      between(Aspect_deg,45,67.4)  ~ 'NE',
                                      between(Aspect_deg,67.5,89.9)  ~ 'ENE',
                                      Aspect_deg == 90 ~ 'E',
                                      between(Aspect_deg,90.1,112.5)  ~ 'ESE',
                                      between(Aspect_deg,112.6,134.9)  ~ 'ESE',
                                      Aspect_deg == 135 ~ 'SE',
                                      between(Aspect_deg,135.1,157.4)  ~ 'SE',
                                      Aspect_deg == 157.5 ~ 'SSE',
                                      between(Aspect_deg,157.6,179.9)  ~ 'SSE',
                                      Aspect_deg == 180 ~ 'S',
                                      between(Aspect_deg,180.1,202.5)  ~ 'SSW',
                                      between(Aspect_deg,202.6,224.9)  ~ 'SSW',
                                      Aspect_deg == 225 ~ 'SW',
                                      between(Aspect_deg,225,247.4)  ~ 'SW',
                                      Aspect_deg == 247.5 ~ 'WSW',
                                      between(Aspect_deg,247.5,269.9)  ~ 'WSW',
                                      Aspect_deg == 270 ~ 'W',
                                      between(Aspect_deg,270.1,292.4)  ~ 'WNW',
                                      between(Aspect_deg,292.5,314.9)  ~ 'WNW',
                                      Aspect_deg == 315 ~ 'NW',
                                      between(Aspect_deg,315.1,337.4)  ~ 'NW',
                                      Aspect_deg == 337.5 ~ 'NNW',
                                      between(Aspect_deg,337.5,359.9)  ~ 'NNW')) 

ELE_07 <- Elevation_07 %>% dplyr::select(-c(Forest_type, Lat, Long, Year, C_ha))

Carbon_Ndvi_Evi_Wcl_Avt_Ele_07 <- left_join(Carbon_Ndvi_Evi_Wcl_Avt_07, ELE_07, by="Plot") %>% mutate(Ecoregion = paste("Irrawaddy Dry Forests" ))


# Adding the LAI values of 2007

LAI_Pp2007 <-raster("Data/LAI_Pp2007.tif")
LAI_Pp2007 <- trim(LAI_Pp2007)
mypts <-readOGR("Four_Forests_shapefile/FourForests.shp")
LAI <- crop(LAI_Pp2007, mypts)
plot(LAI_Pp2007)
mydata <- raster::extract(LAI_Pp2007, mypts)
plot(mypts, add = TRUE, cex=0.1)
class(mydata)
LAI_values <- as.data.frame(mydata)
LAI <- LAI_values %>% rename(LAI=mydata)
Plot_names <- read.csv("Data_Output/WCL_Popa_4Forests.csv",stringsAsFactors = F)
LAI_Plot <- bind_cols(Plot_names,LAI)

Scaled_LAI <- LAI_Plot %>% mutate(Scaled_LAI = LAI/10) %>% rename(Plot=Plot_id) %>% dplyr::select(Plot,Scaled_LAI)

Scaled_LAI_2 <- Scaled_LAI[!duplicated(Scaled_LAI$Plot),]

Carbon_Ndvi_Evi_Wcl_Avt_Ele_08 <- left_join(Carbon_Ndvi_Evi_Wcl_Avt_Ele_07, Scaled_LAI_2, by = 'Plot') %>% rename(LAI = Scaled_LAI)


# Producing the big dataframe with all information for all plots of Popa(2007)

write.csv(Carbon_Ndvi_Evi_Wcl_Avt_Ele_08, file = "Data_Output/20191130Popa2007Df.csv", row.names = F)

### load Main dataframe with all information
Popa_2007 <- read.csv("Data_Output/20191130Popa2007Df.csv", stringsAsFactors = F)
```

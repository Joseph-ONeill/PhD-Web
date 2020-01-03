# Joining the dataframe for Moist Mixed Deciduous Forest's dataset (2013)

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

# Loading the Fieldwork data collected in 2013 by the Forest Department**

Forests <- read.csv("Data/All_data_Final.csv", stringsAsFactors = F)
  
Forests <- filter(Forests,!is.na(Species_names) | !is.na(Dbh_cm))
  
Mmdf_2013 <- Forests %>% filter(Forest_type=="Moist_Mixed_Deciduous_Forest") %>% mutate(Year = paste(2013))

# 50 m * 50 m sample plot with a 25 m * 25 m nested plot. Therefore, the big dataframe was splitted into two according to the DBH limit measured in the 50m * 50m  subplot. *(25m x 25m and 50m x 50m)*  
  
MMDF_50m <- Mmdf_2013 %>% filter(Dbh_cm >= 20)
MMDF_25m <- Mmdf_2013 %>% filter(Dbh_cm < 20)


# Calculating the carbon per ha for two different size plots**  

Plot_50M <- MMDF_50m %>%
  group_by(Plot_id,Forest_type,Year, Long, Lat) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  mutate(C_ha=C_Tree_total/0.25) %>% dplyr::select(-C_Tree_total)

Plot_25M <- MMDF_25m %>%
  group_by(Plot_id,Forest_type,Year, Long, Lat) %>%
  dplyr::summarise(C_Tree_total = sum(C_Tree)) %>%
  mutate(C_ha=C_Tree_total/0.0625) %>% dplyr::select(-C_Tree_total)

# Combining two dataframes of different plot size according to Plot names.Carbon per hectare columns of the two dataframes were added together to obtain total carbon of each plot in hectare scale. 

Mmdf_2013 <- bind_rows(Plot_25M,Plot_50M) %>% group_by(Plot_id,Forest_type,Year, Long, Lat) %>% 
  summarise(C_ha=sum(C_ha)) %>% 
  rename(Carbon_ha = C_ha)

Carbon_13 <- Mmdf_2013 %>% rename(Plot = Plot_id)

# Enhanced  Vegetation Index and Carbon from 2013 forest inventory#

EVI2013_MMDF <-raster("Data/EVI_MMDF_2013.tif")
mypts <-readOGR("MMDF_2013_shapefiles/MMDF_2013.shp")
EVI2013_MMDF <- trim(EVI2013_MMDF)
plot(EVI2013_MMDF)
mydata <- raster::extract(EVI2013_MMDF, mypts)
plot(mypts, add = TRUE, cex=0.1)

class(mydata)
EVI2013_values <- as.data.frame(mydata)
EVI2013 <- EVI2013_values %>% rename(EVI2013=mydata)
EVI2013_MMDF <- bind_cols(Carbon_13,EVI2013)
EVI_2013 <- EVI2013_MMDF 
EVI_13 <- EVI_2013 %>% ungroup() %>% dplyr::select(-c(Forest_type, Year,Long,Lat,Carbon_ha))

Carbon_Evi_13 <- left_join(Carbon_13, EVI_13, by = 'Plot')


# Normalised  Vegetation Index and Carbon from 2013 forest inventory#
NDVI2013_MMDF <-raster("Data/NDVI_MMDF_2013.tif")
mypts <-readOGR("MMDF_2013_shapefiles/MMDF_2013.shp")
NDVI2013_MMDF <- trim(NDVI2013_MMDF)
plot(NDVI2013_MMDF)
mydata <- raster::extract(NDVI2013_MMDF, mypts)
plot(mypts, add = TRUE, cex=0.1)

class(mydata)
NDVI2013_values <- as.data.frame(mydata)
NDVI2013 <- NDVI2013_values %>% rename(NDVI2013=mydata)
NDVI2013_MMDF <- bind_cols(EVI2013_MMDF,NDVI2013)

Carbon_Evi_Ndvi_13 <- NDVI2013_MMDF

# Carbon stocks (2013) and environmental conditions relationship# 

WCL_MMDF_2013 <- read.csv("Data_Output/WCL_MMDF_2013.csv", stringsAsFactors = F) %>% rename(Plot= Plot_id)

WCL_2013 <- WCL_MMDF_2013 %>% dplyr::select(-c(C_ha, Forest_type,Lat, Long,Year))

Carbon_Evi_Ndvi_Wcl_13 <- left_join(Carbon_Evi_Ndvi_13,WCL_2013, by= 'Plot')

#Fieldwork Carbon (2013) and Avitabile carbon projection#

Avt_Carbon_MMDF <- read.csv("Data_Output/Avitabile_AGB_MMDF.csv", stringsAsFactors = F) %>% rename(Plot = Plot_id)

AVT_2013 <- Avt_Carbon_MMDF %>% dplyr::select(-c(C_ha,Frst_ty,Lat,Long,Year,Avt_AGB))

Carbon_Evi_Ndvi_Wcl_Avt_13 <- left_join(Carbon_Evi_Ndvi_Wcl_13,AVT_2013, by='Plot')

# Join with Aelvation_SRTM_13 data

Elevation_13 <- read.csv("Data_Output/SRTM_MMDF2013_ele_as_sl.csv", stringsAsFactors = F)



Elevation_13 <- Elevation_13 %>% 
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

ELE_2013 <- Elevation_13 %>% dplyr::select(c(Plot,Elevation_m,Slope_deg,Aspect_deg,Aspect_direction))

Carbon_Evi_Ndvi_Wcl_Avt_ele_13 <- left_join(Carbon_Evi_Ndvi_Wcl_Avt_13,ELE_2013, by='Plot') %>% mutate(Ecoregion=paste("Irrawaddy Mosit Deciduous Forests"))

# Adding the LAI values into the dataframe. LAI is the median value from 2010-2017. 

LAI_Mmdf <-raster("Data/LAI_Mmdf.tif")
LAI_Mmdf <- trim(LAI_Mmdf)
mypts <-readOGR("MMDF_2013_shapefiles/MMDF_2013.shp")
LAI <- crop(LAI_Mmdf, mypts)
plot(LAI_Mmdf)
mydata <- raster::extract(LAI_Mmdf, mypts)
plot(mypts, add = TRUE, cex=0.1)
class(mydata)
LAI_values <- as.data.frame(mydata)
LAI <- LAI_values %>% rename(LAI=mydata)
Plot_names <- read.csv("Data_Output/20191130Mmdf2013Df.csv",stringsAsFactors = F)
LAI_Plot <- bind_cols(Plot_names,LAI)
LAI_Plot_2 <- LAI_Plot %>% dplyr::select(Plot,LAI) %>% 
  mutate(LAI_2=LAI/10) %>% dplyr::select(c(Plot,LAI_2)) %>% rename(LAI=LAI_2)

Carbon_Evi_Ndvi_Wcl_Avt_ele_14 <- left_join(Carbon_Evi_Ndvi_Wcl_Avt_ele_13, LAI_Plot_2, by = 'Plot')

# Producing the big dataframe with all information for all plots of Moist Mixed Deciduous Forest (2013)


# Producing the big dataframe with all information for all plots of NFI(2010-2017)


col_order_2 <- c("Plot","Ecoregion",
                 "Long","Lat","Year","NDVI2013","EVI2013","LAI",
                 "Elevation_m","Slope_deg","Aspect_deg","Aspect_direction", 
                 "bio01","bio02","bio03","bio04","bio05","bio06"           
                 ,"bio07","bio08","bio09","bio10","bio11","bio12"           
                 ,"bio13","bio14","bio15","bio16","bio17","bio18","bio19",
                 "Avt_Carbon", "Carbon_ha")

Carbon_Evi_Ndvi_Wcl_Avt_ele_14 <- Carbon_Evi_Ndvi_Wcl_Avt_ele_14[,col_order_2]

write.csv(Carbon_Evi_Ndvi_Wcl_Avt_ele_13, file = "Data_Output/20191130Mmdf2013Df.csv", row.names = F)

### load Main dataframe with all information
Mmdf_2013 <- read.csv("Data_Output/20191130Mmdf2013Df.csv", stringsAsFactors = F)

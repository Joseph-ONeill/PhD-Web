### load Main dataframe with all information

NFI2010_17 <- read.csv("Data_Output/20191129NFIDataframe.csv", stringsAsFactors = F) %>% rename(Ecoregion = ECO_NAME) %>%
  rename(Plot = Plot_names)




col_order <- c("Plot","Ecoregion", "DIST",
               "EAST","NORTH", "Zone", "Year","NDVI2010","EVI2010",
               "Elevation_m","Slope_deg","Aspect_deg","Aspect_direction", 
               "bio01","bio02","bio03","bio04","bio05","bio06"           
               ,"bio07","bio08","bio09","bio10","bio11","bio12"           
               ,"bio13","bio14","bio15","bio16","bio17","bio18","bio19",
               "AvgTime_min", "Avt_Carbon", "Carbon_ha")

NFI2010_17_2 <- NFI2010_17[,col_order]

# Adding the LAI values into the dataframe. LAI is the median value from 2010-2017. 

LAI_NFI <-raster("Data/LAI_NFI.tif")
LAI_NFI <- trim(LAI_NFI)
mypts <-readOGR("Edited_Plots_shapefile/locations_WGS84Edited.shp")
LAI <- crop(LAI_NFI, mypts)
plot(LAI_NFI)
mydata <- raster::extract(LAI_NFI, mypts)
plot(mypts, add = TRUE, cex=0.1)
class(mydata)
LAI_values <- as.data.frame(mydata)
LAI <- LAI_values %>% rename(LAI=mydata)
Plot_names <- read.csv("Data_Output/WCL_plots_edited(31-10-2019).csv",stringsAsFactors = F)
LAI_Plot <- bind_cols(Plot_names,LAI)
LAI_Plot_2 <- LAI_Plot[!duplicated(LAI_Plot$Plot_names),] %>% dplyr::select(Plot_names,LAI) %>% rename(Plot=Plot_names) %>% 
  mutate(LAI_2=LAI/10) %>% dplyr::select(c(Plot,LAI_2)) %>% rename(LAI=LAI_2)

NFI2010_17_3 <- left_join(NFI2010_17_2,LAI_Plot_2, by='Plot')


# Producing the big dataframe with all information for all plots of NFI(2010-2017)


col_order_2 <- c("Plot","Ecoregion", "DIST",
               "EAST","NORTH", "Zone", "Year","NDVI2010","EVI2010","LAI",
               "Elevation_m","Slope_deg","Aspect_deg","Aspect_direction", 
               "bio01","bio02","bio03","bio04","bio05","bio06"           
               ,"bio07","bio08","bio09","bio10","bio11","bio12"           
               ,"bio13","bio14","bio15","bio16","bio17","bio18","bio19",
               "AvgTime_min", "Avt_Carbon", "Carbon_ha")

NFI2010_17_3 <- NFI2010_17_3[,col_order_2]


write.csv(NFI2010_17_3, file = "Data_Output/20191130NFI2010_17Df.csv", row.names = F)

### load Main dataframe with all information
NFI2010_17 <- read.csv("Data_Output/20191130NFI2010_17Df.csv", stringsAsFactors = F)



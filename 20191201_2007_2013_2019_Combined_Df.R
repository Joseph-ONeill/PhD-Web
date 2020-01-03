# Loading the main different dataframes

col_order <- c("Plot", "Forest_type", "Ecoregion", 
               "Long","Lat", "Year","NDVI","EVI",
               "Elevation_m","Slope_deg","Aspect_deg","Aspect_direction", 
               "bio01","bio02","bio03","bio04","bio05","bio06"           
               ,"bio07","bio08","bio09","bio10","bio11","bio12"           
               ,"bio13","bio14","bio15","bio16","bio17","bio18","bio19",
               "Avt_Carbon", "Carbon_ha")


Pp2007 <- read.csv("Data_Output/20191130Popa2007Df.csv", stringsAsFactors = F) %>% rename(EVI = EVI2007) %>% 
  rename(NDVI = NDVI2007)

# Adding the suffix, pp2007 in the plot name
Pp2007$Plot <- sub("^", "Pp2007_", Pp2007$Plot)
Pp2007_2 <- Pp2007[, col_order]

Pp2019 <- read.csv("Data_Output/20191130Pp2019Df.csv", stringsAsFactors = F) %>% rename(Elevation_m = Elevation_SRTM)

# Adding the suffix, pp2007 in the plot name
Pp2019$Plot <- sub("^", "Pp2019_", Pp2019$Plot)
Pp2019_2 <- Pp2019[,col_order]


Mmdf2013 <- read.csv("Data_Output/20191130Mmdf2013Df.csv", stringsAsFactors = F) %>% rename(EVI= EVI2013) %>% rename(NDVI = NDVI2013)
Mmdf2013$Plot <- sub("^", "Mmdf2013_", Mmdf2013$Plot)
Mmdf2013_2 <- Mmdf2013[,col_order]

MainDF <- rbind(Pp2019_2,Pp2007_2,Mmdf2013_2)


write.csv(MainDF, file = "Data_Output/20191201_07_13_19_Df.csv", row.names = F)

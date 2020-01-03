
NDVI2018Popa <-raster("Data/NDVI2018_Popa.tif")
mypts <-readOGR("Field_Plots_shapefile/FieldPopa_plots.shp")
NDVI2018Popa <- trim(NDVI2018Popa)
plot(NDVI2018Popa)
mydata <- raster::extract(NDVI2018Popa, mypts)
plot(mypts, add = TRUE, cex=0.1)

class(mydata)
NDVI2018_values <- as.data.frame(mydata)
NDVI2018 <- NDVI2018_values %>% rename(NDVI2018=mydata)
Plot_names <- read.csv("Data_Output/FieldPlots.csv",stringsAsFactors = F)
Plots_NDVI2018 <- Plot_names %>% group_by(Plot) %>% summarise(Long = mean(Long),Lat=mean(Lat))
NDVI2018_Popa <- bind_cols(Plots_NDVI2018,NDVI2018)
NDVI2018_Popa <- NDVI2018_Popa %>% dplyr::select(-c(Long,Lat))
Ndvi <- NDVI2018_Popa
### load Main dataframe with all information

Pp2019 <- read.csv("Data_Output/20191129Popa2019Df.csv", stringsAsFactors = F) %>% rename(Plot=Plot_names) %>% 
  mutate(Ecoregion = "Irrawaddy Dry Forest") %>% rename(EVI = EVI2018)

Pp2019$NDVI <- Ndvi$NDVI2018

# Producing the big dataframe with all information for all plots of NFI(2010-2017)

write.csv(Pp2019, file = "Data_Output/20191130Pp2019Df.csv", row.names = F)

### load Main dataframe with all information
Pp2019 <- read.csv("Data_Output/20191130Pp2019Df.csv", stringsAsFactors = F)


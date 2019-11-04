In_46 <- read.csv("Data_Output/Qgis_edited_Plots_Locations and Carbon/In_Plots46.csv",stringsAsFactors = F)
In_47 <- read.csv("Data_Output/Qgis_edited_Plots_Locations and Carbon/In_Plots47.csv",stringsAsFactors = F)
Out_47 <- read.csv("Data_Output/Qgis_edited_Plots_Locations and Carbon/Out_47.csv", stringsAsFactors = F)
Out_46 <- read.csv("Data_Output/Qgis_edited_Plots_Locations and Carbon/Out_46.csv", stringsAsFactors = F)
Out_46Adjusted <- read.csv("Data_Output/Qgis_edited_Plots_Locations and Carbon/46 Adjusted.csv", stringsAsFactors = F)
Out_47Adjusted <- read.csv("Data_Output/Qgis_edited_Plots_Locations and Carbon/47 Adjusted.csv",stringsAsFactors = F)
# to substract Out-46adjusted from out 46
Out46RowToBeRemoved <- Out_46Adjusted$Plot_names
Cleaned_Out46 <- Out_46[-which(Out_46$Plot_names %in% Out46RowToBeRemoved),]
New_Out46 <- rbind(Cleaned_Out46,Out_46Adjusted)                  
                  

Out47RowToBeRemoved <- Out_47Adjusted$Plot_names
Cleaned_Out47 <- Out_47[-which(Out_47$Plot_names %in% Out47RowToBeRemoved),]
New_Out47 <- rbind(Cleaned_Out47,Out_47Adjusted)   
   

Lcorrected_Plots <- rbind(In_46,In_47,New_Out46, New_Out47)
Carbon <- read.csv("Data_Output/All_plots.csv", stringsAsFactors = F)
Carbon <- Carbon %>% dplyr::select(Plot_names,Sum_CHa)


Plot_Carbon <- left_join(Lcorrected_Plots,Carbon, by="Plot_names")


str(Plot_Carbon)

summary(Plot_Carbon)

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

world2HiresMapEnv

data(wrld_simpl)
myanmar <- wrld_simpl[wrld_simpl$NAME=="Burma",]
plot(myanmar)
myanmar_utm46 <- spTransform(myanmar, "+init=epsg:32646")
myanmar_utm47 <- spTransform(myanmar, "+init=epsg:32647")
plot(myanmar_utm47)
plot(myanmar_utm46)
# Loading the known plots and add a colum called Zone (46/47)----

coords46 <- Plot_Carbon %>% 
  filter(Zone==46)

coords47 <-Plot_Carbon %>% 
  filter(Zone ==47) 


coords46 = SpatialPoints(cbind(coords46$EAST, coords46$NORTH))
proj4string(coords46) = CRS("+init=epsg:32646")
coords46<-spTransform(coords46, "+init=epsg:4326")

coords47 = SpatialPoints(cbind(coords47$EAST, coords47$NORTH))
proj4string(coords47) = CRS("+init=epsg:32647")
coords47<-spTransform(coords47, "+init=epsg:4326")

par(mar=c(4,4,2,2), mfrow =c(2,2))
plot(myanmar, border = "red")
plot(coords46, add = TRUE, cex = 0.1, col = "blue")
plot(coords47, add= TRUE, cex = 0.1, col = "green")

Plot_Carbon$Year <- as.factor(Plot_Carbon$Year)
ggplot(data = Plot_Carbon, aes(x = DIST, y= log(Sum_CHa), color= Year))+ 
  geom_boxplot()+
  labs(title = "Carbon storage per ha in 21 disctricts from 2010 to 2017", 
       x= "District name",
       y= "Carbon(t/ha)")+
  theme(axis.text.x = element_text(size=7, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 7))



Grid46 <- Plot_Carbon %>% filter(Zone=="46")

Grid47 <- Plot_Carbon %>% filter(Zone=="47")

coords46 = SpatialPoints(cbind(Grid46$EAST, Grid46$NORTH))
proj4string(coords46) = CRS("+init=epsg:32646")
coords46 <- spTransform(coords46, "+init=epsg:4326")

coords47 = SpatialPoints(cbind(Grid47$EAST, Grid47$NORTH))
proj4string(coords47) = CRS("+init=epsg:32647")
coords47 <- spTransform(coords47, "+init=epsg:4326")

Plots_location <- rbind(coords46,coords47) 

spdf = SpatialPointsDataFrame(Plots_location,data = Plot_Carbon)

writeOGR(spdf,"locations_WGS84Edited.shp", layer="plot_locations", driver = "ESRI Shapefile")


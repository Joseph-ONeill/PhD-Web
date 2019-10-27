**Extract the plots' locations to extract other values from Google Earth Engine and QGIS. (Plots locations were originally wrong. Therfore, a lot of data cleaning process is done here)**

```{r}

PlotLocs <- read.csv("Data_Output/Carbon_Per_Plots.csv", stringsAsFactors = F)

world2HiresMapEnv

data(wrld_simpl)
myanmar <- wrld_simpl[wrld_simpl$NAME=="Burma",]
plot(myanmar)
myanmar_utm46 <- spTransform(myanmar, "+init=epsg:32646")
myanmar_utm47 <- spTransform(myanmar, "+init=epsg:32647")
plot(myanmar_utm47)
plot(myanmar_utm46)
```

points from scratch by creating for 47 Q and 46 Q by changing the dataframe into spatial class. 

```{r}
coords = cbind(PlotLocs$EAST, PlotLocs$NORTH)
sp_46 <- SpatialPoints(coords)
sp_47 <- SpatialPoints(coords)
```

Changing the projection for different UTM Zones----

```{r}
proj4string(sp_46) = CRS("+init=epsg:32646")
proj4string(sp_47) = CRS("+init=epsg:32647")
```

set the graph layout------

```{r}
par(mar=c(4,4,2,2), mfrow =c(2,2))
plot(sp_46, cex = 0.1, col = "blue")
plot(myanmar_utm46, add = TRUE, border = "red")
plot(sp_47, cex = 0.1, col = "green")
plot(myanmar_utm47, add = TRUE, border = "red")
hist(PlotLocs$EAST, breaks = 100); abline(v = 500000, col = "red"); abline(v = 1500000, col = "red")
hist(PlotLocs$NORTH, breaks = 100); abline(v = 1500000, col = "red")
```

```{r}
PlotLocs %>% 
  filter(EAST > 500000 & EAST < 1500000) %>%
  filter(NORTH >1500000) %>%
  summarise(EAST_min = min(EAST),
            EAST_max = max(EAST),
            NORTH_min = min(NORTH),
            NORTH_max = max(NORTH)
            )
  

coords46 <- PlotLocs %>% 
  filter(EAST > 500000 & EAST < 1500000) %>%
  filter(NORTH >1500000)


coords47<-PlotLocs %>% 
  filter(EAST < 500000) %>%
  filter(NORTH >1500000)
```

Loading the known plots and add a colum called Zone (46/47)----

```{r}
coords46A <- PlotLocs %>% 
  filter(EAST > 500000 & EAST < 1500000) %>%
  filter(NORTH >1500000) %>% mutate(Zone = paste("46"))

coords47A<-PlotLocs %>% 
  filter(EAST < 500000) %>%
  filter(NORTH >1500000) %>% mutate(Zone = paste("47"))
```

```{r}
coords46 = SpatialPoints(cbind(coords46A$EAST, coords46$NORTH))
proj4string(coords46) = CRS("+init=epsg:32646")
coords46<-spTransform(coords46, "+init=epsg:4326")

coords47 = SpatialPoints(cbind(coords47A$EAST, coords47$NORTH))
proj4string(coords47) = CRS("+init=epsg:32647")
coords47<-spTransform(coords47, "+init=epsg:4326")

par(mar=c(4,4,2,2), mfrow =c(2,2))
plot(myanmar, border = "red")
plot(coords46, add = TRUE, cex = 0.1, col = "blue")
plot(coords47, add= TRUE, cex = 0.1, col = "green")

Corrected_Plots <- rbind(coords46A,coords47A)
```


Finding the error plots------------

```{r}
Errror_Plots <- PlotLocs %>% 
  filter(EAST < 500000 & EAST > 1500000) %>%
  filter(NORTH <1500000) %>%
  summarise(EAST_min = min(EAST),
            EAST_max = max(EAST),
            NORTH_min = min(NORTH),
            NORTH_max = max(NORTH)
            )
  

coords46B <- PlotLocs %>% 
  filter(EAST < 500000 & EAST > 1500000) %>%
  filter(NORTH <1500000)


coords47B<-PlotLocs %>% 
  filter(EAST > 500000) %>%
  filter(NORTH <1500000)



```

Loading the fixed plots---------------

```{r}
data(wrld_simpl)
myanmar <- wrld_simpl[wrld_simpl$NAME=="Burma",]
plot(myanmar)
myanmar_utm46 <- spTransform(myanmar, "+init=epsg:32646")
myanmar_utm47 <- spTransform(myanmar, "+init=epsg:32647")

plot(myanmar_utm47)

coords_B = cbind(Error_Plots$EAST, Error_Plots$NORTH)

sp_46_B <- SpatialPoints(coords_B)
sp_47_B <- SpatialPoints(coords_B)

```

# Changing the projection for different UTM Zones
```{r}
proj4string(sp_46_B) = CRS("+init=epsg:32646")
proj4string(sp_47_B) = CRS("+init=epsg:32647")

plot(myanmar, border = "red")
plot(sp_46_B, add = TRUE, cex = 0.1, col = "blue")
plot(sp_47_B, add= TRUE, cex = 0.1, col = "green")
```

# set the graph layout---------
```{r}
par(mar=c(4,4,2,2), mfrow =c(2,2))
plot(sp_46_B, cex = 0.1, col = "blue")
plot(myanmar_utm46, add = TRUE, border = "red")
plot(sp_47_B, cex = 0.1, col = "green")
plot(myanmar_utm47, add = TRUE, border = "red")
hist(Error_Plots$EAST, breaks = 100); abline(v = 600000, col = "red"); abline(v = 1500000, col = "red")
hist(Error_Plots$NORTH, breaks = 100); abline(v = 600000, col = "red")

```
```{r}
coords46_B <- Error_Plots%>% 
  filter(EAST > 600000) %>% mutate(Zone = paste("46")) 

coords47_B <- Error_Plots %>% 
  filter(EAST < 600000) %>% mutate(Zone = paste("47"))

# to be fixed (TBF)

PlotsTBF <- rbind(coords46_B, coords47_B)

Plots_46_M <- PlotsTBF %>% filter(Zone == 46)
Plots_47_M <- PlotsTBF %>% filter(Zone == 47)

```

# Putting the plots on the map (MS= Modified Plots Spatial)
```{r} 
coords46_MS = SpatialPoints(cbind(Plots_46_M$EAST, Plots_46_M$NORTH))
proj4string(coords46_MS) = CRS("+init=epsg:32646")
coords46_MS <-spTransform(coords46_MS, "+init=epsg:4326")

coords47_MS= SpatialPoints(cbind(Plots_47_M$EAST, Plots_47_M$NORTH))
proj4string(coords47_MS) = CRS("+init=epsg:32647")
coords47_MS <- spTransform(coords47, "+init=epsg:4326")

par(mar=c(4,4,2,2), mfrow =c(2,2))
plot(myanmar, border = "red")
plot(coords46_MS, add = TRUE, cex = 0.1, col = "blue")
plot(coords47_MS, add= TRUE, cex = 0.1, col = "green")

```


# Creating a goeoposition fixed main_dataframe---------

```{r}
Mainframe_Plots <- rbind(Corrected_Plots,Fixed_Plots)


```
# Writing a final csv file.
```{r}
 write.csv(Mainframe_Plots, file = "Data_Output/CarbonLocs.csv", row.names = F)
```


# Load the cleaned data file

```{r}
Cleaned_Data <- read.csv("Data_Output/CarbonLocs.csv",stringsAsFactors = FALSE)
Grid46 <- Cleaned_Data %>% filter(Zone=="46")

Grid47 <- Cleaned_Data %>% filter(Zone=="47")

coords46 = SpatialPoints(cbind(Grid46$EAST, Grid46$NORTH))
proj4string(coords46) = CRS("+init=epsg:32646")
coords46 <- spTransform(coords46, "+init=epsg:4326")

coords47 = SpatialPoints(cbind(Grid47$EAST, Grid47$NORTH))
proj4string(coords47) = CRS("+init=epsg:32647")
coords47 <- spTransform(coords47, "+init=epsg:4326")

Plots_location <- rbind(coords46,coords47) 

spdf = SpatialPointsDataFrame(Plots_location,data = Cleaned_Data)

writeOGR(spdf,"locations_WGS84.shp", layer="plot_locations", driver = "ESRI Shapefile")













#####Cut Out bits
```{r}
# 
# Missing_DistrictX <- is.na(All_Plots$District.x)
# All_Plots$District.x[Missing_DistrictX] <- All_Plots$District.y[Missing_DistrictX]
# 
# 
# Missing_DistrictY <- is.na(All_Plots$District.y)
# All_Plots$District.y[Missing_DistrictY]<- All_Plots$District.x[Missing_DistrictY]
# 
# 
# 
# Missing_YearX <- is.na(All_Plots$Year.x)
# All_Plots$Year.x[Missing_YearX] <- All_Plots$Year.y[Missing_DistrictX]
# 
# 
# Missing_YearY <- is.na(All_Plots$Year.y)
# All_Plots$Year.y[Missing_YearY]<- All_Plots$Year.x[Missing_YearY]
# 
# 
# Missing_50 <- is.na(All_Plots$C_ha.y)
# All_Plots$C_ha.y[Missing_50] <- 0
# 
# Missing_100 <- is.na(All_Plots$C_ha.x)
# All_Plots$C_ha.x[Missing_100] <- 0
# 
# Missing_CT_total <- is.na(All_Plots$C_Tree_total)
# All_Plots$C_Tree_total[Missing_CT_total] <- 0
# 
# sum(!duplicated(All_Plots))
# All_Plots_Carbon <- All_Plots %>% mutate(Carbon_THa = C_ha.x+ C_ha.y) %>% dplyr::select(-C_Tree_total,-District.y,-Year.y)
# All_Plots_Carbon <- All_Plots_Carbon %>% rename(District =District.x, Year=Year.x,Carbon_100m = C_ha.x, Carbon_50m = C_ha.y)
# 
# 
# All_Plots_Carbon <- All_Plots_Carbon %>% drop_na()
# sum(is.na(All_Plots_Carbon))
# Duplinames <- All_Plots_Carbon[duplicated(All_Plots_Carbon$Plot_names),]




```
View(Field_Data)

# change the degree symbol to a space
Field_Data$North = gsub('°', ' ',Field_Data$North)
Field_Data$North = gsub("'", ' ',Field_Data$North)
Field_Data$North = gsub('"', ' ',Field_Data$North)
Field_Data$North = gsub('N', ' ',Field_Data$North)
Field_Data$North = gsub('   ', ' ',Field_Data$North)
Field_Data$North = gsub('  ', ' ',Field_Data$North)

Field_Data$East = gsub('°', ' ',Field_Data$East)
Field_Data$East = gsub("'", ' ',Field_Data$East)
Field_Data$East = gsub('"', ' ',Field_Data$East)
Field_Data$East = gsub('E', ' ',Field_Data$East)
Field_Data$East = gsub('   ', ' ',Field_Data$East)
Field_Data$East = gsub("  ",' ',Field_Data$East)


# convert from decimal minutes to decimal degrees
Field_Data$Lat = measurements::conv_unit(Field_Data$North, from = 'deg_min_sec', to = 'dec_deg')
Field_Data$Long = measurements::conv_unit(Field_Data$East, from = 'deg_min_sec', to = 'dec_deg')



# Diameter distribution in three areas---------------------------------------------------------------------------

# Field_Data$Plot <- as.factor(Field_Data$Plot)
# cbPalette=c("forestgreen","blue", "darkred")
# ggplot(Field_Data, aes(x = Plot, y = Dbh_cm)) +
#   geom_boxplot(aes(fill = Location), alpha = 0.5)+
#   geom_jitter(aes(color = Location, width = 0.01))+
#   scale_fill_manual(values=cbPalette)+ # Boxplot fill color
#   scale_color_manual(values = cbPalette)+# Jitter color palette
#   ylab("DBH (cm)") +
#   xlab("Location")+
#   scale_y_continuous(breaks = c(0,20,40,60,80,100))+
#   ylim(1,100)+
#   theme_bw()+
#   theme(text = element_text(size=15), axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))

# # Height distribution in three areas---------------------------------------------------------------------------------
# ggplot(Field_Data, aes(x = Location, y = H_m)) +
#   geom_boxplot(aes(fill = Location), alpha = 0.5)+
#   geom_jitter(aes(color = Location))+
#   scale_fill_manual(values=cbPalette)+ # Boxplot fill color
#   scale_color_manual(values = cbPalette)+# Jitter color palette
#   ylab("Height(m)") +
#   xlab("Location")+
#   scale_y_continuous(breaks = c(0,10,20,30,40,50))+
#   ylim(1,50)+
#   theme_bw()+
#   theme(text = element_text(size=15), axis.text.x = element_blank(),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
#
#
# # Height and diameter relationship------------------------------------------------------------------------------------
# Field_Data %>%
#   na.omit %>%
#   ggplot(aes(y = H_m, x = Dbh_cm)) +
#   geom_point(position = "jitter", color = "green", size = 3)+
#   xlab("DBH (cm)")+
#   ylab("Height-H (m)")+
#   scale_x_continuous(breaks = c(0,20,40,60,80,100, 120))+
#   geom_smooth(method = "lm")+
#   theme_bw() +
#   theme(text = element_text(size=15),axis.text.x  = element_text(angle=0, hjust=2),
#         axis.title.x = element_text(margin = margin(t = 18, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)))
#
# Field_Data %>% lm(H_m~Dbh_cm)
#
# LmHD <- lm(Field_Data$H_m~Field_Data$Dbh_cm)
# summary(LmHD)
#
#
#
# Draft-------
#   Data1 <- na.omit(Field_Data)
# > View(Data1)
# > View(Field_Data)
# > View(Data1)
# > Data2 <- na.omit(Field_Data$Genus)
# > Data2 <- na.omit(Field_Data[,-Genus])
# Error in `[.data.frame`(Field_Data, , -Genus) : object 'Genus' not found
# > Data2 <- na.omit(Field_Data[,-Field_Data$Genus])
# Error in `[.data.frame`(Field_Data, , -Field_Data$Genus) :
#   undefined columns selected
# In addition: Warning message:
#   In Ops.factor(Field_Data$Genus) : ‘-’ not meaningful for factors
# > Data2 <- na.omit(Field_Data[-Genus])
# Error in `[.data.frame`(Field_Data, -Genus) : object 'Genus' not found
# > Data2 <- na.omit(Field_Data[-7])
# > View(Data2)


Carbon_Distance <- Carbon_dataframe %>% left_join(dplyr:: select(Distance_dataframe, Plot_names, Travel_Time_min),by= "Plot_names") %>% dplyr::select(-NORTH, -EAST) %>% drop_na(Travel_Time_min)

All_data0 <- Carbon_Distance %>% left_join(dplyr:: select(NDVI_dataframe,NDVI2010, Plot_names), by = "Plot_names")

All_data1 <- All_data0 %>% left_join(dplyr:: select(EVI_dataframe, Plot_names, EVI2010), by= "Plot_names")

All_data2 <- All_data1 %>% left_join(dplyr:: select(WCL_dataframe, Plot_names, bio01, bio12), by="Plot_names") %>% rename(Annual_mean_Temperature = bio01, Annual_Precipitation = bio12)

All_data4 <- All_data3 %>% left_join(dplyr:: select(Avitabile_dataframe, Plot_names,mean), by= "Plot_names") %>% rename(Avt_Carbon = mean)


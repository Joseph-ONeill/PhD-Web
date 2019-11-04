
Creating a spatial dataframe with the plot location

```{r}
names(All_Plots)
head(All_Plots$EAST)
head(All_Plots$NORTH)

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
coords = cbind(All_Plots$EAST, All_Plots$NORTH)
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
plot(myanmar_utm47, add = TRUE, border = "black")
hist(All_Plots$EAST, breaks = 100); abline(v = 500000, col = "red"); abline(v = 1500000, col = "red")
hist(All_Plots$NORTH, breaks = 100); abline(v = 1500000, col = "red")
```

```{r}
In_Plots <- All_Plots %>% 
  filter(EAST > 500000 & EAST < 1500000) %>%
  filter(NORTH > 1500000) %>%
  summarise(EAST_min = min(EAST),
            EAST_max = max(EAST),
            NORTH_min = min(NORTH),
            NORTH_max = max(NORTH)
  )


coords46 <- All_Plots %>% 
  filter(EAST > 500000 & EAST < 1500000) %>%
  filter(NORTH >1500000)


coords47<-All_Plots %>% 
  filter(EAST < 500000) %>%
  filter(NORTH >1500000)
```

Loading the known plots and add a colum called Zone (46/47)----
  
  ```{r}
coords46In <- All_Plots %>% 
  filter(EAST > 500000 & EAST < 1500000) %>%
  filter(NORTH >1500000) %>% mutate(Zone = paste("46"))

coords47In <-All_Plots %>% 
  filter(EAST < 500000) %>%
  filter(NORTH >1500000) %>% mutate(Zone = paste("47"))
```

```{r}
coords46 = SpatialPoints(cbind(coords46In$EAST, coords46In$NORTH))
proj4string(coords46) = CRS("+init=epsg:32646")
coords46<-spTransform(coords46, "+init=epsg:4326")

coords47 = SpatialPoints(cbind(coords47In$EAST, coords47In$NORTH))
proj4string(coords47) = CRS("+init=epsg:32647")
coords47<-spTransform(coords47, "+init=epsg:4326")

par(mar=c(4,4,2,2), mfrow =c(2,2))
plot(myanmar, border = "red")
plot(coords46, add = TRUE, cex = 0.1, col = "blue")
plot(coords47, add= TRUE, cex = 0.1, col = "green")

In_Plots <- rbind(coords46In,coords47In)
In_Plots46 <-  In_Plots %>% filter(Zone==46)
In_Plots47 <- In_Plots %>% filter(Zone==47)

write.csv(In_Plots46, file="Data_Output/In_Plots46.csv",row.names = F)
write.csv(In_Plots47, file="Data_Output/In_Plots47.csv",row.names = F)

```


Finding the error outplots------------
  
  
  ```{r}
Out_Plots <- All_Plots %>% 
  filter(NORTH < 1500000) %>%
  summarise(NORTH = NORTH,
            EAST_min = min(EAST),
            EAST_max = max(EAST),
            NORTH_min = min(NORTH),
            NORTH_max = max(NORTH)
  )
Out_Plots <- Out_Plots %>% rename(EAST=NORTH, NORTH=EAST)

```

Loading the known plots and add a colum called Zone (46/47)----
  
  ```{r}
coords46Out <- Out_Plots %>% 
  filter(EAST > 500000 & EAST < 1500000) %>%
  filter(NORTH >1500000) %>% mutate(Zone = paste("46"))

coords47Out <-Out_Plots %>% 
  filter(EAST < 500000) %>%
  filter(NORTH > 1500000) %>% mutate(Zone = paste("47"))

Out_Sorted <- rbind(coords46Out, coords47Out)

write.csv(coords46Out, file="Data_Output/Out_46.csv",row.names = F)
write.csv(coords47Out, file="Data_Output/Out_47.csv",row.names = F)

```

```{r}
coords46Out = SpatialPoints(cbind(coords46Out$EAST, coords46Out$NORTH))
proj4string(coords46Out) = CRS("+init=epsg:32646")
coords46Out <-spTransform(coords46Out, "+init=epsg:4326")

coords47Out = SpatialPoints(cbind(coords47Out$EAST, coords47Out$NORTH))
proj4string(coords47Out) = CRS("+init=epsg:32647")
coords47<-spTransform(coords47Out, "+init=epsg:4326")

par(mar=c(4,4,2,2), mfrow =c(2,2))
plot(myanmar, border = "red")
plot(coords46Out, add = TRUE, cex = 0.1, col = "blue")
plot(coords47Out, add= TRUE, cex = 0.1, col = "green")



write.csv(Out_Plots, file="Data_Output/Out_Plots.csv",row.names = F)

```

```{r}
data(wrld_simpl)
myanmar <- wrld_simpl[wrld_simpl$NAME=="Burma",]
plot(myanmar)
myanmar_utm46 <- spTransform(myanmar, "+init=epsg:32646")
myanmar_utm47 <- spTransform(myanmar, "+init=epsg:32647")

plot(myanmar_utm47)
plot(myanmar_utm46)

coords_out = cbind(Out_Plots$EAST,Out_Plots$NORTH)

coords_out_Plots <- SpatialPoints(coords_out)

```

# Changing the projection for different UTM Zones
```{r}
proj4string(coords_out_Plots) = CRS("+init=epsg:32646")
# proj4string(coords_out_Plots) = CRS("+init=epsg:32647")

plot(myanmar, border = "red")
plot(coords_out_Plots, add = TRUE, cex = 0.1, col = "blue")
# plot(sp_47_B, add= TRUE, cex = 0.1, col = "green")
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

**Produce a cleaned data frame for per ha carbon for each plot**
  
  ```{r}
write.csv(All_Plots,file = "Data_Output/Carbon_Per_Plots.csv",row.names = FALSE)

```

Creating a spatial dataframe with the plot location



**Carbon according to WWF's ecoregions. Use the original shape file of plots Locations to extract values** 

```{r}

Ecoregions <- read.csv("Data_Output/WWFEcoregions-Plots(23-10-2019).csv", stringsAsFactors = F)


Carbon <- read.csv("Data_Output/Carbon_Per_Plots.csv", stringsAsFactors = F)
Eco_Carbon <- Carbon %>% inner_join(dplyr::select(Ecoregions, -Year, -District),by.x = "Plot_names") %>% dplyr::select(-EAST,-NORTH) %>% drop_na(ECO_NAME)

attach(Eco_Carbon)

lm.CarbonEcoregions <- lm(log(Sum_CHa)~ECO_NAME, data = Eco_Carbon)
anova(lm.CarbonEcoregions)
plot(lm.CarbonEcoregions)
summary(lm.CarbonEcoregions)

boxplot(log(Sum_CHa)~ECO_NAME, data = Eco_Carbon)
aov.CarbonEcoregions <- aov(Sum_CHa~ECO_NAME, data = Eco_Carbon)


ggplot(data=Eco_Carbon, aes(x=ECO_NAME, y = log(Sum_CHa), color=ECO_NAME))+ geom_boxplot()+
  labs(title = "Carbon storage according to ecoregions", 
       x= "Ecoregion",
       y= "MaxCarbon(t/ha)")+
  theme(axis.text.x = element_text(size=7, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 7))
detach(Eco_Carbon)


```
**Carbon and environmental conditions relationship** 

BIO1 = Annual Mean Temperature  
BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))  
BIO3 = Isothermality (BIO2/BIO7) (* 100)  
BIO4 = Temperature Seasonality (standard deviation *100)  
BIO5 = Max Temperature of Warmest Month  
BIO6 = Min Temperature of Coldest Month  
BIO7 = Temperature Annual Range (BIO5-BIO6)  
BIO8 = Mean Temperature of Wettest Quarter  
BIO9 = Mean Temperature of Driest Quarter  
BIO10 = Mean Temperature of Warmest Quarter  
BIO11 = Mean Temperature of Coldest Quarter  
BIO12 = Annual Precipitation  
BIO13 = Precipitation of Wettest Month  
BIO14 = Precipitation of Driest Month  
BIO15 = Precipitation Seasonality (Coefficient of Variation)  
BIO16 = Precipitation of Wettest Quarter  
BIO17 = Precipitation of Driest Quarter  
BIO18 = Precipitation of Warmest Quarter  
BIO19 = Precipitation of Coldest Quarter  

**Modelling carbon storage differences due to environmental impacts in districts**

A one-way ANOVA showed that carbon storage differed significantly between districts (F=55.27, df=20,7860, p < 0.001)  

```{r}
lm.CarbonDistrict <- lm(log(Sum_CHa)~District, data = Eco_Carbon)
anova(lm.CarbonDistrict)
plot(lm.CarbonDistrict)
```

```{r}
Env <- read.csv("Data_Output/WCL_plots(23-10-2019).csv", stringsAsFactors = F)
Env_Carbon <- Carbon %>% left_join(Env,by.x="Plot_names") %>% drop_na(bio01)

```
the test has very conclusively rejected the hypothesis that all means are equal. However, this was not due to all of the sample means being different, but rather just because one of the groups is very different from the others. In order to drill down and investigate this further we use a new test called Tukey's range test.This will compare all of the groups in a pairwise fashion and reports on whether a significant difference exists.

```{r}
boxplot(log(Sum_CHa)~District, data=Env_Carbon)
aov.Carbon <- aov(log(Sum_CHa)~District,data= Env_Carbon)
```

I performed a simple liner regression analysis on the two variables Carbon and Annual precipitaion(AP)/Annual Mean Temperature(AMT). I wish to determine wheter the AP/AMT varible is a significant predictor of the Carbon Variable. 

```{r}
plot(log(Sum_CHa)~bio12, data = Env_Carbon)
lm.AMP <- lm(log(Sum_CHa)~bio12, data=Env_Carbon)
anova(lm.AMP)
summary(lm.AMP)


ggplot(data=Env_Carbon, aes(x=bio12, y = log(Sum_CHa), color=District))+ geom_point()+
  labs(title = "Carbon storage and Annual Precipitation Relationship", 
       x= "Annual Annual Precipitation (mm)",
       y= "MaxCarbon(t/ha)")+
  theme(axis.text.x = element_text(size=7, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 7))

# According to Year 
ggplot(data=Env_Carbon, aes(x=bio12, y = log(Sum_CHa), color=District))+ geom_line()+
  facet_wrap(facets = vars(Year))+
  labs(title = "Carbon storage and Annual Precipitation (mm) Relationship According to Census Year", 
       x= "Annual Precipitation (mm)",
       y= "MaxCarbon(t/ha)")+
  theme(axis.text.x = element_text(size=7, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 7))

# According to Districts

ggplot(data=Env_Carbon, aes(x=bio12, y = log(Sum_CHa), color=District))+ geom_line()+
  facet_wrap(facets = vars(District))+
  labs(title = "Carbon storage and Annual Precipitation (mm) Relationship according to Districts", 
       x= "Annual Precipitation (mm)",
       y= "MaxCarbon(t/ha)")+
  theme(axis.text.x = element_text(size=7, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 7))




```
```{r}
plot(log(Sum_CHa)~log(bio01), data = Env_Carbon)
lm.AMT <- lm(log(Sum_CHa)~log(bio01), data=Env_Carbon)
anova(lm.AMT)
summary(lm.AMT)

# According to Year 
ggplot(data=Env_Carbon, aes(x=log(bio01), y = log(Sum_CHa), color=District))+ geom_line()+
  facet_wrap(facets = vars(Year))+
  labs(title = "Carbon storage and Annual Mean Temperature Relationship according to Census Year", 
       x= "Annual Mean Temperature",
       y= "MaxCarbon(t/ha)")+
  theme(axis.text.x = element_text(size=7, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 7))

# According to Districts

ggplot(data=Env_Carbon, aes(x=log(bio01), y = log(Sum_CHa), color=District))+ geom_line()+
  facet_wrap(facets = vars(District))+
  labs(title = "Carbon storage and Annual Mean Temperature Relationship according to District", 
       x= "Annual Mean Temperature",
       y= "MaxCarbon(t/ha)")+
  theme(axis.text.x = element_text(size=7, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 7))

```

**-Creating a dataframe which includes all the variables-**

```{r}
library(tidyverse)
Carbon_dataframe <- read.csv("Data_Output/Carbon_Per_Plots.csv", stringsAsFactors = F)
NDVI_dataframe <- read.csv("Data_Output/NDVIC_2010.csv", stringsAsFactors = F)
EVI_dataframe <- read.csv("Data_Output/EVIC_2010.csv", stringsAsFactors = F)
WCL_dataframe <- read.csv("Data_Output/WCL_plots(23-10-2019).csv", stringsAsFactors = F)
Avitabile_dataframe <- read.csv("Data_Output/Avitabile_Data(23-10-2019).csv", stringsAsFactors = F)
Distance_dataframe <- read.csv("Data_Output/distance_to_cities.csv", stringsAsFactors = F)

TEST <- Carbon_dataframe %>% 
  inner_join(dplyr::select(NDVI_dataframe,Plot_names,NDVI2010), by= c("Plot_names"= "Plot_names")) %>% 
    inner_join(dplyr::select(EVI_dataframe,Plot_names,EVI2010), by= c("Plot_names"= "Plot_names")) %>%
  inner_join(dplyr:: select(WCL_dataframe,Plot_names,bio01,bio12), by= c("Plot_names"= "Plot_names")) %>% 
  inner_join(dplyr:: select(Avitabile_dataframe,Plot_names,mean), by= c("Plot_names"= "Plot_names")) %>% rename(Avt_Carbon = mean) %>% 
  inner_join(dplyr:: select(Distance_dataframe,Plot_names,Travel_Time_min), by= c("Plot_names"= "Plot_names"))
library(stringr)
TEST$Ploat_names <- str_trim(TEST$Plot_names)
Final_Dataframe <- TEST[!duplicated(TEST$Sum_CHa),] 
All_variables <- Final_Dataframe[!duplicated(Final_Dataframe$Plot_names),] %>% dplyr:: select(-Ploat_names) %>% rename(Annual_mean_Temperature = bio01, Annual_Precipitation = bio12) %>% dplyr:: select(-c(EAST,NORTH))

```
```{r}
attach(All_variables)
lm1 <- lm(log(Sum_CHa)~log(Annual_mean_Temperature), data=All_variables)
lm2 <- lm(log(Sum_CHa)~log(Annual_mean_Temperature)+ log(Annual_Precipitation), data=All_variables)
summary(lm2)

```



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


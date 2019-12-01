### load Main dataframe with all information

NFI2010_17 <- read.csv("Data_Output/20191129NFIDataframe.csv", stringsAsFactors = F) %>% rename(Ecoregion = ECO_NAME) %>%
  rename(Plot = Plot_names)

# Producing the big dataframe with all information for all plots of NFI(2010-2017)

write.csv(NFI2010_17, file = "Data_Output/20191130NFI2010_17Df.csv", row.names = F)

### load Main dataframe with all information
NFI2010_17 <- read.csv("Data_Output/20191130NFI2010_17Df.csv", stringsAsFactors = F)


### load Main dataframe with all information

Pp2019 <- read.csv("Data_Output/20191129Popa2019Df.csv", stringsAsFactors = F) %>% rename(Plot=Plot_names) %>% mutate(Ecoregion = "Irrawaddy Dry Forest")

# Producing the big dataframe with all information for all plots of NFI(2010-2017)

write.csv(Pp2019, file = "Data_Output/20191130Pp2019Df.csv", row.names = F)

### load Main dataframe with all information
Pp2019 <- read.csv("Data_Output/20191130Pp2019Df.csv", stringsAsFactors = F)


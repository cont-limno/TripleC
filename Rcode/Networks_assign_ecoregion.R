############# Assignment of LAGOS networks by NARS ecoregion ###################################
# Date: 4-26-21
# updated: 5-18-21
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(raster)
library(dplyr)

#### Input data ####
networks_NARS <- shapefile("C:/Ian_GIS/TripleC_GIS/Networks/network_NARS/network_NARS.shp")
networks_NARS_df <- as.data.frame(networks_NARS@data)

#### Main program ####
# Assign networks that span multiple ecoregions to ecoregion that contains the majority of the network
networks_ecoregion <- networks_NARS_df %>% 
  count(net_id, WSA9) %>%
  group_by(net_id) %>%
  slice(which.max(n)) #this takes the ecoregion with the most lakes in a network; may not want that

# this is not the total number of lakes per network
#write.csv(networks_ecoregion, file='Data/Networks/networks_NARS_ecoregions.csv', row.names=F)


############# Assignment of LAGOS networks by NARS ecoregion ###################################
# Date: 4-26-21
# updated: 
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
  slice(which.max(n))

# manually assign ecoregions to networks that got missed by ArcGIS spatial join (topology issue?)
# relevant to two small networks on Texas coast (CPL ecoregion)
networks_ecoregion$WSA9 <- ifelse(networks_ecoregion$net_id %in% c('839','859'), 'CPL',networks_ecoregion$WSA9)

write.csv(networks_ecoregion, file='Data/Networks/networks_NARS_ecoregions.csv', row.names=F)


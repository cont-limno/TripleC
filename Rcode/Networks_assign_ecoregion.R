############# Assignment of LAGOS networks by NARS ecoregion ###################################
# Date: 4-26-21
# updated: 5-27-21
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(raster)
library(dplyr)

#### Input data ####
# This file has lake networks spatially joined to National Aquatic Resource Survey ecoregions (US EPA) in ArcGIS
networks_NARS <- shapefile("C:/Ian_GIS/TripleC_GIS/Networks/network_NARS/network_NARS.shp")
networks_NARS_df <- as.data.frame(networks_NARS@data)

#Original data source and resources:
# King, K., Wang, Q., Rodriguez, L.K., Haite, M., Danila, L., Pang-Ning, T., Zhou, J., and Cheruvelil, K.S. under review. 
# LAGOS-US NETWORKS v1.0: Data module of surface water networks characterizing connections among lakes, streams, and
# rivers in the conterminous U.S. Environmental Data Initiative. 
# https://portal-s.edirepository.org/nis/mapbrowse?scope=edi&identifier=213. Dataset accessed XX/XX/2021.
# 
# King, K., Wang, Q., Rodriguez, L.K., and Cheruvelil, K.S. under review. Lake networks and connectivity metrics
# for the conterminous U.S. (LAGOS-US NETWORKS v1). Limnology and Oceanography Letters. 

#### Main program ####
# Assign networks that span multiple ecoregions to ecoregion that contains the majority of the network
networks_ecoregion <- networks_NARS_df %>% 
  count(net_id, WSA9) %>%
  group_by(net_id) %>%
  slice(which.max(n)) #this takes the ecoregion with the most lakes in a network; may not want that

# save output table; this is not the total number of lakes per network
#write.csv(networks_ecoregion, file='Data/Networks/networks_NARS_ecoregions.csv', row.names=F)


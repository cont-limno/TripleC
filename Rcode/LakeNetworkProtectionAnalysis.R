############# Analyze LAGOS networks by protected status #######################################
# Date: 4-26-21
# updated: 
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(raster)
library(dplyr)

#### Input data ####
netricks <- read.csv('Data/Networks/network_scale_metrics.csv')
networks_NARS <- read.csv("Data/Networks/networks_NARS_ecoregions.csv")
networks_pts_NARS <- shapefile("C:/Ian_GIS/TripleC_GIS/Networks/network_NARS/network_NARS.shp")
GAP12_lakes_net <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP12_lake_pts_net.shp")
GAP123_lakes_net <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP123_lake_pts_net.shp")
GAP12_lakes_net80 <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP12_lake_pts_net_80pct.shp")
GAP123_lakes_net80 <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP123_lake_pts_net_80pct.shp")

#### Main program ####
# get lagoslakeids of protected lakes by different status
GAP12_lakes_net_lagoslakeid <- GAP12_lakes_net@data$lagoslakei
GAP123_lakes_net_lagoslakeid <- GAP123_lakes_net@data$lagoslakei
GAP12_lakes_net80_lagoslakeid <- GAP12_lakes_net80@data$lagoslakei
GAP123_lakes_net80_lagoslakeid <- GAP123_lakes_net80@data$lagoslakei

# get table of network ids and lagoslakeids
ID_table <- networks_pts_NARS@data[,c('lagoslakei','net_id','WSA9')]
names(ID_table) <- c('lagoslakeid','net_id','WSA9')

## get table of lakes, networks, ecoregions and protection status
# crete new columns for protection status
protection <- ID_table
protection$GAP12_ctr <- NA
protection$GAP123_ctr <- NA
protection$GAP12_80pct <- NA
protection$GAP123_80pct <- NA

# assign 1 for protected, 0 for not
protection$GAP12_ctr <- ifelse(protection$lagoslakeid %in% GAP12_lakes_net_lagoslakeid, 1,0)
protection$GAP123_ctr <- ifelse(protection$lagoslakeid %in% GAP123_lakes_net_lagoslakeid, 1,0)
protection$GAP12_80pct <- ifelse(protection$lagoslakeid %in% GAP12_lakes_net80_lagoslakeid, 1,0)
protection$GAP123_80pct <- ifelse(protection$lagoslakeid %in% GAP123_lakes_net80_lagoslakeid, 1,0)

## calculate proportion of lakes in each network protected by different protection status
nLakes <- netricks[,c('net_id','net_lakes_n')]
prop_protection <- protection %>% 
  group_by(net_id) %>%
  summarize(GAP12_ctr=sum(GAP12_ctr),GAP123_ctr=sum(GAP123_ctr),
            GAP12_80pct=sum(GAP12_80pct), GAP123_80pct=sum(GAP123_80pct))

prop_protection <- merge(prop_protection, nLakes, by='net_id')
prop_protection$GAP12_ctr_pct <- prop_protection$GAP12_ctr/prop_protection$net_lakes_n
prop_protection$GAP123_ctr_pct <- prop_protection$GAP123_ctr/prop_protection$net_lakes_n
prop_protection$GAP123_80pct_pct <- prop_protection$GAP12_80pct/prop_protection$net_lakes_n
prop_protection$GAP12_80pct_pct <- prop_protection$GAP123_80pct/prop_protection$net_lakes_n

# Basic figure of network protection (no ecoregions)
jpeg('Figures/network_protection_histograms.jpeg',width = 7,height = 5,units = 'in',res=300)
par(mfrow=c(2,2))
par(mar = c(4, 4, 2, 2)) #bot,left,top,right
hist(prop_protection$GAP12_ctr_pct, main='GAPS 1-2, lake center protected', las=1, xlab='Proportion network protected', ylim=c(0,800))
hist(prop_protection$GAP123_ctr_pct, main='GAPS 1-3, lake center protected', las=1, xlab='Proportion network protected', ylim=c(0,800))
hist(prop_protection$GAP12_80pct_pct, main='GAPS 1-2, 80% watershed protected', las=1, xlab='Proportion network protected', ylim=c(0,800))
hist(prop_protection$GAP123_80pct_pct, main='GAPS 1-3, 80% watershed protected', las=1, xlab='Proportion network protected', ylim=c(0,800))
dev.off()

## Bringing in the ecoregions
prop_protection_NARS<- merge(prop_protection, networks_NARS[,c(1,2)], by='net_id')

jpeg('Figures/network_protection_boxplotsNARS.jpeg',width = 7,height = 5,units = 'in',res=300)
par(mfrow=c(2,2))
par(mar = c(4, 4, 2, 2)) #bot,left,top,right
boxplot(prop_protection_NARS$GAP12_ctr_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-2, lake center protected')
boxplot(prop_protection_NARS$GAP123_ctr_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-3, lake center protected')
boxplot(prop_protection_NARS$GAP12_80pct_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-2, 80% watershed protected')
boxplot(prop_protection_NARS$GAP123_80pct_pct~prop_protection_NARS$WSA9, las=2,xlab='', ylab='Proportion protected', main='GAPS1-3, 80% watershed protected')
dev.off()
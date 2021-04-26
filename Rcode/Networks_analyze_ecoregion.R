############# Analyze LAGOS networks by NARS ecoregion #########################################
# Date: 4-26-21
# updated: 
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(dplyr)

#### Input data ####
networks_NARS <- read.csv("Data/Networks/networks_NARS_ecoregions.csv")
netricks <- read.csv('Data/Networks/network_scale_metrics.csv') 
between_cent <- read.csv("Data/Networks/betweenness_out_full.csv")

#### Main program ####
# Select network variables that had been used for clustering
dat <- netricks %>% 
  dplyr::select(net_id, edge_dens, artic_count, min_cut_lat, maxkmNS, net_lakes_n, net_averagelakedistance_km,
                net_rangeorder)
head(dat)

# optionally add in betweenness centrality metric
dat <- merge(dat, between_cent[,c(1,4)], by='net_id')

# Remove 1 NA value (this is MS river)
dat <- dat %>% filter(!is.na(maxkmNS))

# Remove networks with < 4 lakes
#dat <- dat %>% 
#  filter(net_lakes_n > 4)

# Add row names
row.names(dat) <- dat$net_id
dim(dat)
summary(dat)

# Remove net_id (redundant if already removed MS river)
#dat <- dat[,-1]

# Merge netricks data to NARS
netricks_NARS <- merge(dat, networks_NARS[,c(1,2)], by='net_id')

# summary stats of netricks by NARS
# (not finished)
netricks_stats <- netricks_NARS %>% 
  group_by(WSA9) %>%
  summarize(median=median(edge_dens),n=n())

# plots
jpeg('Figures/netricks_NARS_boxplots.jpeg',width = 7,height = 10,units = 'in',res=300)
par(mfrow=c(4,2))
par(mar = c(3, 4, 2, 2)) #bot,left,top,right
boxplot(netricks_NARS$edge_dens ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Edge density', las=2)
boxplot(netricks_NARS$artic_count ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Articulation points', las=2)
boxplot(netricks_NARS$min_cut_lat ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Min cuts latitude', las=2)
boxplot(netricks_NARS$maxkmNS ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Max N-S breadth (km)', las=2)
boxplot(netricks_NARS$net_lakes_n ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Number of lakes', las=2)
boxplot(netricks_NARS$net_averagelakedistance_km ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Avg lake distance', las=2)
boxplot(netricks_NARS$net_rangeorder ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Lake order range', las=2)
boxplot(netricks_NARS$vert_btwn_centr_norm_mean ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Betweenness centrality', las=2)
dev.off()

############# Analyze LAGOS networks by NARS ecoregion #########################################
# Date: 4-26-21
# updated: 5-6-21
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(dplyr)

#### Input data ####
networks_NARS <- read.csv("Data/Networks/networks_NARS_ecoregions.csv")
netricks <- read.csv('Data/Networks/network_scale_metrics.csv') 
between_cent <- read.csv("Data/Networks/betweenness_out_full.csv")
network_lakes_NARS_nonMS <- read.csv("Data/Networks/nLakes_networks_NARS_nonMS.csv")

#### Main program ####
# count networks by ecoregion
networks_NARS_counts <- networks_NARS %>%
  group_by(WSA9) %>%
  summarize(n=n())

# count network lakes by ecoregion without MS river
network_lakes_NARS_counts_nonMS <- network_lakes_NARS_nonMS %>%
  group_by(WSA9) %>%
  summarize(n=n())


# Select network variables that had been used for clustering
netricks$artic_pct <- netricks$artic_count/netricks$net_lakes_n #calculate % of lakes in network that are articulation pts
dat <- netricks %>% 
  dplyr::select(net_id, edge_dens, artic_pct, min_cut_lat, maxkmNS, net_lakes_n, net_averagelakedistance_km,
                net_rangeorder, net_dams_n)
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
  summarize(min_nLakes=min(net_lakes_n), median_nLakes=round(median(net_lakes_n),0), max_nLakes=max(net_lakes_n), n=n(),
            minNS=round(min(maxkmNS, na.rm=T),0), medianNS=round(median(maxkmNS, na.rm=T),0), maxNS=round(max(maxkmNS, na.rm=T),0),
            minDams=min(net_dams_n), medianDams=round(median(net_dams_n),0), maxDams=max(net_dams_n),
            minartic=round(min(artic_pct),2), medianartic=round(median(artic_pct),2), maxartic=round(max(artic_pct),2),
            min_latcuts=min(min_cut_lat), median_latcuts=median(min_cut_lat), max_latcuts=max(min_cut_lat))

netricks_stats$combined_col_nLakes <- paste0(netricks_stats$min_nLakes, ', ', netricks_stats$median_nLakes, ', ', netricks_stats$mmax_nLakes)
netricks_stats$combined_colNS <- paste0(netricks_stats$minNS, ', ', netricks_stats$medianNS, ', ', netricks_stats$maxNS)
netricks_stats$combined_colDams <- paste0(netricks_stats$minDams, ', ', netricks_stats$medianDams, ', ', netricks_stats$maxDams)
netricks_stats$combined_colartic <- paste0(netricks_stats$minartic, ', ', netricks_stats$medianartic, ', ', netricks_stats$maxartic)
netricks_stats$combined_collatcuts <- paste0(netricks_stats$min_latcuts, ', ', netricks_stats$median_latcuts, ', ', netricks_stats$max_latcuts)

#write.csv(netricks_stats, "Data/Networks/netrick_stats_NARS.csv")

# plots
jpeg('Figures/netricks_NARS_boxplots.jpeg',width = 7,height = 10,units = 'in',res=300)
par(mfrow=c(4,2))
par(mar = c(3, 4, 2, 2)) #bot,left,top,right
boxplot(netricks_NARS$edge_dens ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Edge density', las=2)
boxplot(netricks_NARS$artic_pct ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Articulation points', las=2)
boxplot(netricks_NARS$min_cut_lat ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Min cuts latitude', las=2)
boxplot(netricks_NARS$maxkmNS ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Max N-S breadth (km)', las=2)
boxplot(netricks_NARS$net_lakes_n ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Number of lakes', las=2)
boxplot(netricks_NARS$net_averagelakedistance_km ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Avg lake distance', las=2)
boxplot(netricks_NARS$net_rangeorder ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Lake order range', las=2)
boxplot(netricks_NARS$vert_btwn_centr_norm_mean ~ netricks_NARS$WSA9, xlab='Ecoregion', ylab='Betweenness centrality', las=2)
dev.off()

netricks_NARS_min4 <- subset(netricks_NARS, net_lakes_n >=4)

jpeg('Figures/netricks_NARS_boxplots_min4lakes.jpeg',width = 7,height = 10,units = 'in',res=300)
par(mfrow=c(4,2))
par(mar = c(3, 4, 2, 2)) #bot,left,top,right
boxplot(netricks_NARS_min4$edge_dens ~ netricks_NARS_min4$WSA9, xlab='Ecoregion', ylab='Edge density', las=2)
boxplot(netricks_NARS_min4$artic_pct ~ netricks_NARS_min4$WSA9, xlab='Ecoregion', ylab='Articulation points', las=2)
boxplot(netricks_NARS_min4$min_cut_lat ~ netricks_NARS_min4$WSA9, xlab='Ecoregion', ylab='Min cuts latitude', las=2)
boxplot(netricks_NARS_min4$maxkmNS ~ netricks_NARS_min4$WSA9, xlab='Ecoregion', ylab='Max N-S breadth (km)', las=2)
boxplot(netricks_NARS_min4$net_lakes_n ~ netricks_NARS_min4$WSA9, xlab='Ecoregion', ylab='Number of lakes', las=2)
boxplot(netricks_NARS_min4$net_averagelakedistance_km ~ netricks_NARS_min4$WSA9, xlab='Ecoregion', ylab='Avg lake distance', las=2)
boxplot(netricks_NARS_min4$net_rangeorder ~ netricks_NARS_min4$WSA9, xlab='Ecoregion', ylab='Lake order range', las=2)
boxplot(netricks_NARS_min4$vert_btwn_centr_norm_mean ~ netricks_NARS_min4$WSA9, xlab='Ecoregion', ylab='Betweenness centrality', las=2)
dev.off()


############# Analyze LAGOS networks by protected status #######################################
# Date: 4-26-21
# updated: 
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(raster)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

#### Input data ####
netricks <- read.csv('Data/Networks/network_scale_metrics.csv')
networks_NARS <- read.csv("Data/Networks/networks_NARS_ecoregions.csv")
networks_pts_NARS <- shapefile("C:/Ian_GIS/TripleC_GIS/Networks/network_NARS/network_NARS.shp")
GAP12_lakes_net <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP12_lake_pts_net.shp")
GAP123_lakes_net <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP123_lake_pts_net.shp")
GAP12_lakes_net80 <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP12_lake_pts_net_80pct.shp")
GAP123_lakes_net80 <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP123_lake_pts_net_80pct.shp")
hub_lakes <- read.csv("Data/Networks/VIP_lakes.csv")

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

## Hub lake analysis
# figure out ecoregion membership of hubs
hub_lakes_NARS <- merge(hub_lakes, ID_table, by='lagoslakeid')

hub_lakes_NARS_counts <- hub_lakes_NARS %>% 
  group_by(WSA9) %>%
  summarize(n=n())

net_lakes_NARS_counts <- protection %>% 
  group_by(WSA9) %>%
  summarize(total_n=n())

# protection of hubs
hub_lake_protection <- subset(protection, lagoslakeid %in% hub_lakes$lagoslakeid)

hub_lake_prop_protection <- hub_lake_protection %>% 
  group_by(WSA9) %>%
  summarize(GAP12_ctr=sum(GAP12_ctr),GAP123_ctr=sum(GAP123_ctr),
            GAP12_80pct=sum(GAP12_80pct), GAP123_80pct=sum(GAP123_80pct))

hub_lake_prop_protection <- merge(hub_lake_prop_protection, hub_lakes_NARS_counts, by='WSA9')
hub_lake_prop_protection$GAP12_ctr_pct <- (hub_lake_prop_protection$GAP12_ctr/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP123_ctr_pct <- (hub_lake_prop_protection$GAP123_ctr/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP12_80pct_pct <- (hub_lake_prop_protection$GAP12_80pct/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP123_80pct_pct <- (hub_lake_prop_protection$GAP123_80pct/hub_lake_prop_protection$n)*100

# isolated added GAP3 effect
hub_lake_prop_protection$GAP3_ctr_only_pct <- hub_lake_prop_protection$GAP123_ctr_pct - hub_lake_prop_protection$GAP12_ctr_pct
hub_lake_prop_protection$GAP3_80pct_only_pct <- hub_lake_prop_protection$GAP123_80pct_pct - hub_lake_prop_protection$GAP12_80pct_pct

stacked_ctr_df <- melt(hub_lake_prop_protection[,c(1,7,11)], 'WSA9')
stacked_80pct_df <- melt(hub_lake_prop_protection[,c(1,9,12)], 'WSA9')

stacked_ctr_plot <- ggplot(stacked_ctr_df, aes(fill=variable, y=value, x=WSA9)) + 
  geom_bar(stat="identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  guides(fill = guide_legend(reverse=T)) +  
  #theme_bw() +
  ggtitle('a) Protected hub = lake center in protected area')+
  scale_y_continuous(limits=c(0,75), breaks=seq(0,75,5)) +
  #scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("GAP3_ctr_only_pct" = "navajowhite2", "GAP12_ctr_pct" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  theme(legend.position=c(0.13,0.86))+ #manually reposition legend inside plot
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  #theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title


stacked_80pct_plot <- ggplot(stacked_80pct_df, aes(fill=variable, y=value, x=WSA9)) + 
  geom_bar(stat="identity") +
  xlab("") +
  ylab("Percent of lakes protected") +
  guides(fill = guide_legend(reverse=T)) +  
  #theme_bw() +
  ggtitle('b) Protected hub = 80% watershed protected')+
  scale_y_continuous(limits=c(0,75), breaks=seq(0,75,5)) +
  #scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("GAP3_80pct_only_pct" = "navajowhite2", "GAP12_80pct_pct" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  theme(legend.position=c(0.13,0.86))+ #manually reposition legend inside plot
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  #theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title


png('Figures/HubLakeProtectionByNARS.png',width = 4.5,height = 6,units = 'in',res=300)
  grid.arrange(stacked_ctr_plot, stacked_80pct_plot, nrow=2)
dev.off()

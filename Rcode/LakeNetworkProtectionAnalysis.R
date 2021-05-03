############# Analyze LAGOS networks by protected status #######################################
# Date: 4-26-21
# updated: 4-27-21
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
between_cent <- read.csv("Data/Networks/betweenness_out_full.csv")
network_lakes_NARS_nonMS <- read.csv("Data/Networks/nLakes_networks_NARS_nonMS.csv")

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
prop_protection$GAP12_80pct_pct <- prop_protection$GAP12_80pct/prop_protection$net_lakes_n
prop_protection$GAP123_80pct_pct <- prop_protection$GAP123_80pct/prop_protection$net_lakes_n

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

net_lakes_NARS_counts <- protection %>% 
  group_by(WSA9) %>%
  summarize(total_n=n())

prop_protection_NARS_counts <- prop_protection_NARS %>%
  group_by(WSA9) %>%
  summarize(GAP12_ctr=sum(GAP12_ctr),GAP123_ctr=sum(GAP123_ctr),
            GAP12_80pct=sum(GAP12_80pct), GAP123_80pct=sum(GAP123_80pct))

prop_protection_NARS_counts <- merge(prop_protection_NARS_counts, net_lakes_NARS_counts, by='WSA9')
prop_protection_NARS_counts$GAP12_ctr_pct <- (prop_protection_NARS_counts$GAP12_ctr/prop_protection_NARS_counts$total_n)*100
prop_protection_NARS_counts$GAP123_ctr_pct <- (prop_protection_NARS_counts$GAP123_ctr/prop_protection_NARS_counts$total_n)*100
prop_protection_NARS_counts$GAP12_80pct_pct <- (prop_protection_NARS_counts$GAP12_80pct/prop_protection_NARS_counts$total_n)*100
prop_protection_NARS_counts$GAP123_80pct_pct <- (prop_protection_NARS_counts$GAP123_80pct/prop_protection_NARS_counts$total_n)*100


jpeg('Figures/network_protection_boxplotsNARS.jpeg',width = 7,height = 5,units = 'in',res=300)
par(mfrow=c(2,2))
par(mar = c(4, 4, 2, 2)) #bot,left,top,right
boxplot(prop_protection_NARS$GAP12_ctr_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-2, lake center protected')
boxplot(prop_protection_NARS$GAP123_ctr_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-3, lake center protected')
boxplot(prop_protection_NARS$GAP12_80pct_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-2, 80% watershed protected')
boxplot(prop_protection_NARS$GAP123_80pct_pct~prop_protection_NARS$WSA9, las=2,xlab='', ylab='Proportion protected', main='GAPS1-3, 80% watershed protected')
dev.off()

# ggplot grouped boxplot
prop_protection_NARS_noMS <- subset(prop_protection_NARS, net_id>1)
prop_protection_grouped_ctr <- prop_protection_NARS_noMS[,c(7,8,11)]
prop_protection_grouped_ctr <- melt(prop_protection_grouped_ctr)
colnames(prop_protection_grouped_ctr) <- c('WSA9','Protection','Percent')
prop_protection_grouped_ctr$Percent <- prop_protection_grouped_ctr$Percent *100

prop_protection_grouped_80pct <- prop_protection_NARS_noMS[,c(9,10,11)]
prop_protection_grouped_80pct <- melt(prop_protection_grouped_80pct)
colnames(prop_protection_grouped_80pct) <- c('WSA9','Protection','Percent')
prop_protection_grouped_80pct$Percent <- prop_protection_grouped_80pct$Percent *100

ctr_grouped <- ggplot(prop_protection_grouped_ctr, aes(WSA9, Percent, fill=Protection)) +
  geom_boxplot()+
  xlab("") +
  ylab("% of network protected") +
  guides(fill = guide_legend(reverse=T)) +  
  #theme_bw() +
  ggtitle('a) Network protection (lake centers)')+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20)) +
  #scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("GAP123_ctr_pct" = "navajowhite2", "GAP12_ctr_pct" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  #theme(legend.position=c(0.43,0.86))+ #manually reposition legend inside plot
  theme(legend.position = 'none')+
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  #theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title

plot80pct_grouped <- ggplot(prop_protection_grouped_80pct, aes(WSA9, Percent, fill=Protection)) +
  geom_boxplot()+
  xlab("") +
  ylab("% of network protected") +
  guides(fill = guide_legend(reverse=T)) +  
  #theme_bw() +
  ggtitle('c) Network protection (80% watershed)')+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20)) +
  #scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("GAP123_80pct_pct" = "navajowhite2", "GAP12_80pct_pct" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  theme(legend.position=c('none'))+ #manually reposition legend inside plot
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  #theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title

# export multi-panel image
#png('Figures/NetworkProtectionGroupedNARSBoxplots.png',width = 4.5,height = 6,units = 'in',res=300)
#  grid.arrange(ctr_grouped, plot80pct_grouped, nrow=2)
#dev.off()


# calculate stats on network protection level across ecoregions and different protection status
prop_protection_NARS_melted <- melt(prop_protection_NARS[,c(7:11)], id.vars='WSA9')
names(prop_protection_NARS_melted) <- c('WSA9','protection','protection_pct')
prop_protection_NARS_melted$protection_pct <- round((prop_protection_NARS_melted$protection_pct *100),2)

network_protection_NARS_stats <- prop_protection_NARS_melted %>%
  group_by(WSA9, protection) %>%
  summarize(min=min(protection_pct), median=median(protection_pct), max=max(protection_pct))
  
a <- subset(network_protection_NARS_stats, protection=='GAP12_ctr_pct')
a <- cbind.data.frame(a[,c(1:2)], paste0('(',a$min,'), ', '(',a$median, '), ','(',a$max,')'))
names(a) <- c('WSA9','protection','MinMedianMax')

b <- subset(network_protection_NARS_stats, protection=='GAP123_ctr_pct')
b <- cbind.data.frame(b[,c(1:2)], paste0('(',b$min,'), ', '(',b$median, '), ','(',b$max,')'))
names(b) <- c('WSA9','protection','MinMedianMax')

c <- subset(network_protection_NARS_stats, protection=='GAP12_80pct_pct')
c <- cbind.data.frame(c[,c(1:2)], paste0('(',c$min,'), ', '(',c$median, '), ','(',c$max,')'))
names(c) <- c('WSA9','protection','MinMedianMax')

d <- subset(network_protection_NARS_stats, protection=='GAP123_80pct_pct')
d <- cbind.data.frame(d[,c(1:2)], paste0('(',d$min,'), ', '(',d$median, '), ','(',d$max,')'))
names(d) <- c('WSA9','protection','MinMedianMax')

abdc <- rbind.data.frame(a,b,c,d)
#write.csv(abdc, file='Data/Networks/network_protection_NARS_stats.csv')

# same calculations across all ecoregions
prop_protection_NARS_melted %>%
  group_by(protection) %>%
  summarize(min=min(protection_pct), median=median(protection_pct), max=max(protection_pct))

## Hub lake analysis
# figure out ecoregion membership of hubs
hub_lakes_NARS <- merge(hub_lakes, ID_table, by='lagoslakeid')

hub_lakes_NARS_counts <- hub_lakes_NARS %>% 
  group_by(WSA9) %>%
  summarize(n=n())

# count network lakes by ecoregion without MS river
network_lakes_NARS_counts_nonMS <- network_lakes_NARS_nonMS %>%
  group_by(WSA9) %>%
  summarize(n=n())

# protection of hubs
hub_lake_protection <- subset(protection, lagoslakeid %in% hub_lakes$lagoslakeid)

hub_lake_prop_protection <- hub_lake_protection %>% 
  group_by(WSA9) %>%
  summarize(GAP12_ctr=sum(GAP12_ctr),GAP123_ctr=sum(GAP123_ctr),
            GAP12_80pct=sum(GAP12_80pct), GAP123_80pct=sum(GAP123_80pct))

hub_lake_prop_protection <- merge(hub_lake_prop_protection, network_lakes_NARS_counts_nonMS, by='WSA9')
hub_lake_prop_protection$GAP12_ctr_pct <- (hub_lake_prop_protection$GAP12_ctr/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP123_ctr_pct <- (hub_lake_prop_protection$GAP123_ctr/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP12_80pct_pct <- (hub_lake_prop_protection$GAP12_80pct/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP123_80pct_pct <- (hub_lake_prop_protection$GAP123_80pct/hub_lake_prop_protection$n)*100

# isolated added GAP3 effect
hub_lake_prop_protection$GAP3_ctr_only_pct <- hub_lake_prop_protection$GAP123_ctr_pct - hub_lake_prop_protection$GAP12_ctr_pct
hub_lake_prop_protection$GAP3_80pct_only_pct <- hub_lake_prop_protection$GAP123_80pct_pct - hub_lake_prop_protection$GAP12_80pct_pct

stacked_ctr_df <- melt(hub_lake_prop_protection[,c(1,7,8)], 'WSA9')
stacked_80pct_df <- melt(hub_lake_prop_protection[,c(1,9,10)], 'WSA9')

stacked_ctr_plot <- ggplot(stacked_ctr_df, aes(fill=variable, y=value, x=WSA9)) + 
  geom_bar(stat="identity", position='dodge') +
  xlab("") +
  ylab("% of hub lakes protected") +
  guides(fill = guide_legend(reverse=T)) +  
  #theme_bw() +
  ggtitle('b) Hub lake protection (lake centers)')+
  scale_y_continuous(limits=c(0,3), breaks=seq(0,3,1)) +
  #scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  #geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("GAP123_ctr_pct" = "navajowhite2", "GAP12_ctr_pct" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  theme(legend.position=c(0.2,0.86))+ #manually reposition legend inside plot
  #theme(legend.position=c('none'))+
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  #theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title


stacked_80pct_plot <- ggplot(stacked_80pct_df, aes(fill=variable, y=value, x=WSA9)) + 
  geom_bar(stat="identity", position='dodge') +
  xlab("") +
  ylab("% of hub lakes protected") +
  guides(fill = guide_legend(reverse=T)) +  
  #theme_bw() +
  ggtitle('d) Hub lake protection (80% watershed)')+
  scale_y_continuous(limits=c(0,3), breaks=seq(0,3,1)) +
  #scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  #geom_hline(yintercept=17, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("GAP123_80pct_pct" = "navajowhite2", "GAP12_80pct_pct" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  #theme(legend.position=c(0.13,0.86))+ #manually reposition legend inside plot
  theme(legend.position='none')+
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  #theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title


png('Figures/NetworkHubLakeProtectionByNARS.png',width = 7.5,height = 7.5,units = 'in',res=300)
  grid.arrange(ctr_grouped, stacked_ctr_plot, plot80pct_grouped, stacked_80pct_plot, nrow=2, ncol=2)
dev.off()


### Does protection relate to network conn stats? ##
# Select network variables for clustering
dat <- netricks %>% 
  dplyr::select(net_id, edge_dens, artic_count, min_cut_lat, maxkmNS, net_lakes_n, net_averagelakedistance_km,
                net_rangeorder)

# optionally add in betweenness centrality metric
dat <- merge(dat, between_cent[,c(1,4)], by='net_id')

# Remove 1 NA value
dat <- dat %>% filter(!is.na(maxkmNS))

# Remove networks with < 4 lakes
#dat <- dat %>% 
#  filter(net_lakes_n > 4)

# Add row names
row.names(dat) <- dat$net_id

# combine netricks with protection 
conn_protection <- merge(dat, prop_protection_NARS[,c(1,7:11)], by='net_id')
par(mfrow=c(2,2))

plot(conn_protection$GAP12_ctr_pct ~ conn_protection$net_lakes_n, xlab='Number of lakes', 
     ylab='Propotion network protected', las=1, main='GAPS 1-2, lake centers')
plot(conn_protection$GAP123_ctr_pct ~ conn_protection$net_lakes_n, xlab='Number of lakes', 
     ylab='Propotion network protected', las=1, main='GAPS 1-3, lake centers')
plot(conn_protection$GAP12_80pct_pct ~ conn_protection$net_lakes_n, xlab='Number of lakes', 
     ylab='Propotion network protected', las=1, main='GAPS 1-2, 80% watershed')
plot(conn_protection$GAP123_80pct_pct ~ conn_protection$net_lakes_n, xlab='Number of lakes', 
     ylab='Propotion network protected', las=1, main='GAPS1-3, 80% watershed')

cormat <- as.data.frame(cor(conn_protection[2:13], use='pairwise.complete.obs',method='pearson'))
#write.csv(cormat[9:12], file='Data/Networks/pearson_cormat_networkconn_protection.csv')


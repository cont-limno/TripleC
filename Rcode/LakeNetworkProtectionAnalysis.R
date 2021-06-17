############# Analyze LAGOS networks by protected status #######################################
# Date: 4-26-21
# updated: 6-17-21
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
#hub_lakes <- read.csv("Data/Networks/VIP_lakes.csv") #without MS River network
hub_lakes <- read.csv("Data/Networks/Hub_lakes_withMS.csv") #all hubs
between_cent <- read.csv("Data/Networks/betweenness_out_full.csv")
network_lakes_NARS_nonMS <- read.csv("Data/Networks/nLakes_networks_NARS_nonMS.csv")

# Original data sources and resources:
# Cheruvelil, K. S., Soranno, P. A., McCullough, I. M., Webster, K. E., Rodriguez, L. and N. J. Smith. 
# LAGOS-US LOCUS v1.0: Data module of location, identifiers, and physical characteristics of lakes and their 
# watersheds in the conterminous U.S. Limnology and Oceanography Letters(data paper in review).
# 
# King, K., Wang, Q., Rodriguez, L.K., Haite, M., Danila, L., Pang-Ning, T., Zhou, J., and Cheruvelil, K.S. under review. 
# LAGOS-US NETWORKS v1.0: Data module of surface water networks characterizing connections among lakes, streams, and
# rivers in the conterminous U.S. Environmental Data Initiative. 
# https://portal-s.edirepository.org/nis/mapbrowse?scope=edi&identifier=213. Dataset accessed XX/XX/2021.
# 
# King, K., Wang, Q., Rodriguez, L.K., and Cheruvelil, K.S. under review. Lake networks and connectivity metrics
# for the conterminous U.S. (LAGOS-US NETWORKS v1). Limnology and Oceanography Letters. 
#
# PADUS v 1.4 (2.0 exists but using 1.4 because that is in LAGOS-US-GEO)
# US Geological Survey (USGS) Gap Analysis Program (GAP), 20160505, Protected Areas Database of the United States (PAD-US): 
# USGS Gap Analysis Program (GAP), https://doi.org/10.5066/F7G73BSZ. 

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

#write.csv(prop_protection, 'Data/Networks/network_protection.csv')

# # Basic figure of network protection (no ecoregions)
# jpeg('Figures/network_protection_histograms.jpeg',width = 7,height = 5,units = 'in',res=300)
# par(mfrow=c(2,2))
# par(mar = c(4, 4, 2, 2)) #bot,left,top,right
# hist(prop_protection$GAP12_ctr_pct, main='GAPS 1-2, lake center protected', las=1, xlab='Proportion network protected', ylim=c(0,800))
# hist(prop_protection$GAP123_ctr_pct, main='GAPS 1-3, lake center protected', las=1, xlab='Proportion network protected', ylim=c(0,800))
# hist(prop_protection$GAP12_80pct_pct, main='GAPS 1-2, 80% watershed protected', las=1, xlab='Proportion network protected', ylim=c(0,800))
# hist(prop_protection$GAP123_80pct_pct, main='GAPS 1-3, 80% watershed protected', las=1, xlab='Proportion network protected', ylim=c(0,800))
# dev.off()

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

# How many fully protected networks?
w=subset(prop_protection_NARS, GAP12_ctr_pct >=1)
x=subset(prop_protection_NARS, GAP123_ctr_pct >=1)
y=subset(prop_protection_NARS, GAP12_80pct_pct >=1)
z=subset(prop_protection_NARS, GAP123_80pct_pct >=1)

w %>%
  group_by(WSA9) %>%
  summarize(n=n())

x %>%
  group_by(WSA9) %>%
  summarize(n=n())

y %>%
  group_by(WSA9) %>%
  summarize(n=n())

z %>%
  group_by(WSA9) %>%
  summarize(n=n())

# jpeg('Figures/network_protection_boxplotsNARS.jpeg',width = 7,height = 5,units = 'in',res=300)
# par(mfrow=c(2,2))
# par(mar = c(4, 4, 2, 2)) #bot,left,top,right
# boxplot(prop_protection_NARS$GAP12_ctr_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-2, lake center protected')
# boxplot(prop_protection_NARS$GAP123_ctr_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-3, lake center protected')
# boxplot(prop_protection_NARS$GAP12_80pct_pct~prop_protection_NARS$WSA9, las=2, xlab='', ylab='Proportion protected', main='GAPS1-2, 80% watershed protected')
# boxplot(prop_protection_NARS$GAP123_80pct_pct~prop_protection_NARS$WSA9, las=2,xlab='', ylab='Proportion protected', main='GAPS1-3, 80% watershed protected')
# dev.off()

### ggplot grouped boxplot of protection across ecoregions
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
  geom_hline(yintercept=17, linetype='dotted', color='black')+
  geom_hline(yintercept=30, linetype='dashed', color='black')+
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
  geom_hline(yintercept=17, linetype='dotted', color='black')+
  geom_hline(yintercept=30, linetype='dashed', color='black')+
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

# how many networks meet 17% Aichi target
aichi_networks <- subset(prop_protection_NARS_melted, protection_pct >= 17)

aichi_networks_summary <- aichi_networks %>%
  group_by(WSA9, protection) %>%
  summarize(n=n())

by30_networks <- subset(prop_protection_NARS_melted, protection_pct >= 30)

by30_networks_summary <- by30_networks %>%
  group_by(WSA9, protection) %>%
  summarize(n=n())

#write.csv(aichi_networks_summary, "Data/Networks/protected_networks_Aichi.csv")
#write.csv(by30_networks_summary, "Data/Networks/protected_networks_30by30.csv")

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

#hub_lake_prop_protection <- merge(hub_lake_prop_protection, network_lakes_NARS_counts_nonMS, by='WSA9')#if want % protection as divided by network lakes
hub_lake_prop_protection <- merge(hub_lake_prop_protection, hub_lakes_NARS_counts, by='WSA9')
hub_lake_prop_protection$GAP12_ctr_pct <- (hub_lake_prop_protection$GAP12_ctr/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP123_ctr_pct <- (hub_lake_prop_protection$GAP123_ctr/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP12_80pct_pct <- (hub_lake_prop_protection$GAP12_80pct/hub_lake_prop_protection$n)*100
hub_lake_prop_protection$GAP123_80pct_pct <- (hub_lake_prop_protection$GAP123_80pct/hub_lake_prop_protection$n)*100

# isolated added GAP3 effect
hub_lake_prop_protection$GAP3_ctr_only_pct <- hub_lake_prop_protection$GAP123_ctr_pct - hub_lake_prop_protection$GAP12_ctr_pct
hub_lake_prop_protection$GAP3_80pct_only_pct <- hub_lake_prop_protection$GAP123_80pct_pct - hub_lake_prop_protection$GAP12_80pct_pct

#write.csv(hub_lake_prop_protection, "Data/Networks/hub_lake_protection_NARS.csv")

stacked_ctr_df <- melt(hub_lake_prop_protection[,c(1,7,8)], 'WSA9')
stacked_80pct_df <- melt(hub_lake_prop_protection[,c(1,9,10)], 'WSA9')

stacked_ctr_plot <- ggplot(stacked_ctr_df, aes(fill=variable, y=value, x=WSA9)) + 
  geom_bar(stat="identity", position='dodge') +
  xlab("") +
  ylab("% of hub lakes protected") +
  guides(fill = guide_legend(reverse=T)) +  
  #theme_bw() +
  ggtitle('b) Hub protection (lake centers)')+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20)) +
  #scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dotted', color='black')+
  geom_hline(yintercept=30, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("GAP123_ctr_pct" = "navajowhite2", "GAP12_ctr_pct" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Strict + Multi-use'))+
  theme(legend.position=c(0.25,0.86))+ #manually reposition legend inside plot
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
  ggtitle('d) Hub protection (80% watershed)')+
  scale_y_continuous(limits=c(0,100), breaks=seq(0,100,20)) +
  #scale_x_discrete(labels=c('IS','HW','DRS','DRLS'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  #theme(axis.text.x=element_text(angle=50, hjust=1))+ #tilt axis labels
  geom_hline(yintercept=17, linetype='dotted', color='black')+
  geom_hline(yintercept=30, linetype='dashed', color='black')+
  theme(axis.title.y = element_text(vjust=2.7, color='black'))+ #nudge y axis label away from axis a bit
  scale_fill_manual("legend", values = c("GAP123_80pct_pct" = "navajowhite2", "GAP12_80pct_pct" = "olivedrab3"),#,"Unprotected" = "gray70"),
                    labels=c('Strict','Multi-use'))+
  #theme(legend.position=c(0.13,0.86))+ #manually reposition legend inside plot
  theme(legend.position='none')+
  theme(axis.text.y = element_text(color='black'), axis.text.x=element_text(color='black'))+
  #theme(legend.position='none')+
  theme(legend.title=element_blank()) #remove legend title

# combine boxplots of network protection and hub protection by ecoregion into multi-panel figure
tiff('Figures/NetworkHubLakeProtectionByNARS.tif',width = 7.5,height = 7.5,units = 'in',res=300)
  grid.arrange(ctr_grouped, stacked_ctr_plot, plot80pct_grouped, stacked_80pct_plot, nrow=2, ncol=2)
dev.off()


### Does protection relate to network connectivity stats? ##
# Select network variables for connectivity scores/stats
dat <- netricks %>% 
  dplyr::select(net_id, edge_dens, artic_count, min_cut_lat, maxkmNS, net_lakes_n, net_averagelakedistance_km,
                net_rangeorder, net_dams_n)

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

cormat <- as.data.frame(cor(conn_protection[2:14], use='pairwise.complete.obs',method='pearson'))
#write.csv(cormat[10:13], file='Data/Networks/pearson_cormat_networkconn_protection.csv')

## Relationship between network connectivity scores and hub lakes
# analysis of both networks with just high scores and all networks with scores
pca_Scores <- read.csv("Data/Networks/pca_network_conn_scores_wDamRate.csv")[,c(9,10)]

pca_Scores_hubs <- merge(hub_lakes_NARS[,c(1:2)], pca_Scores, by='net_id', all=T)
length(unique(pca_Scores_hubs$net_id))

pca_hiScores_hubs <- subset(pca_Scores_hubs, PCconnall > 4)
length(unique(pca_hiScores_hubs$net_id))

# how many hubs in high-score networks? Appears smallest hub net_id 106 (WA state) has no hubs
pca_hiScores_hubs_summary <- pca_hiScores_hubs %>%
  group_by(net_id) %>%
  summarize(nHubs=n())

pca_allScores_hubs_summary <- pca_Scores_hubs %>%
  group_by(net_id) %>%
  summarize(nHubs=sum(!is.na(lagoslakeid)))

# how well protected are these hubs in high-score networks?
hiScores_hubs_protection <- merge(pca_hiScores_hubs, hub_lake_protection, by='lagoslakeid')

hiScores_hubs_protection_summary <- hiScores_hubs_protection %>%
  group_by(net_id.x) %>%
  summarize(GAP12_ctr=sum(GAP12_ctr),
            GAP123_ctr=sum(GAP123_ctr),
            GAP12_80pct=sum(GAP12_80pct),
            GAP123_80pct=sum(GAP123_80pct))

colnames(hiScores_hubs_protection_summary) <- c('net_id','GAP12_ctr','GAP123_ctr','GAP12_80pct','GAP123_80pct')

hiScores_hubs_protection_summary <- merge(hiScores_hubs_protection_summary, pca_hiScores_hubs_summary, by='net_id')

# calculate % of hubs protected in each network
hiScores_hubs_protection_summary$GAP12_ctr_pct <- round((hiScores_hubs_protection_summary$GAP12_ctr/hiScores_hubs_protection_summary$nHubs)*100,2)
hiScores_hubs_protection_summary$GAP123_ctr_pct <- round((hiScores_hubs_protection_summary$GAP123_ctr/hiScores_hubs_protection_summary$nHubs)*100,2)
hiScores_hubs_protection_summary$GAP12_80pct_pct <- round((hiScores_hubs_protection_summary$GAP12_80pct/hiScores_hubs_protection_summary$nHubs)*100,2)
hiScores_hubs_protection_summary$GAP123_80pct_pct <- round((hiScores_hubs_protection_summary$GAP123_80pct/hiScores_hubs_protection_summary$nHubs)*100,2)

test <- merge(hiScores_hubs_protection_summary, dat, by='net_id', all=F)
test <- merge(test, networks_NARS[,c(1:2)], by='net_id', all=F)
test <- merge(test, pca_Scores, by='net_id', all=F)

#write.csv(test, "Data/Networks/HighScoreNetworkStats.csv", row.names=F)

## How about another table of all networks, netricks, scores and protection of networks and hubs
# Maybe can focus on top-scoring networks in MS

# First do same analysis as above, but for all scored networks:
# how well protected are these hubs in scored networks?
allScores_hubs_protection <- merge(pca_Scores_hubs, hub_lake_protection, by='lagoslakeid', all=T)

allScores_hubs_protection_summary <- allScores_hubs_protection %>%
  group_by(net_id.x) %>%
  summarize(GAP12_ctr=sum(GAP12_ctr),
            GAP123_ctr=sum(GAP123_ctr),
            GAP12_80pct=sum(GAP12_80pct),
            GAP123_80pct=sum(GAP123_80pct))

colnames(allScores_hubs_protection_summary) <- c('net_id','GAP12_ctr','GAP123_ctr','GAP12_80pct','GAP123_80pct')

allScores_hubs_protection_summary <- merge(allScores_hubs_protection_summary, pca_allScores_hubs_summary, by='net_id')

# calculate % of hubs protected in each network
allScores_hubs_protection_summary$GAP12_ctr_pct <- round((allScores_hubs_protection_summary$GAP12_ctr/allScores_hubs_protection_summary$nHubs)*100,2)
allScores_hubs_protection_summary$GAP123_ctr_pct <- round((allScores_hubs_protection_summary$GAP123_ctr/allScores_hubs_protection_summary$nHubs)*100,2)
allScores_hubs_protection_summary$GAP12_80pct_pct <- round((allScores_hubs_protection_summary$GAP12_80pct/allScores_hubs_protection_summary$nHubs)*100,2)
allScores_hubs_protection_summary$GAP123_80pct_pct <- round((allScores_hubs_protection_summary$GAP123_80pct/allScores_hubs_protection_summary$nHubs)*100,2)

test2 <- merge(allScores_hubs_protection_summary, dat, by='net_id', all=F)
test2 <- merge(test2, networks_NARS[,c(1:2)], by='net_id', all=F)
test2 <- merge(test2, pca_Scores, by='net_id', all=F)

# final merging
layover <- conn_protection[,c(1,11:14)]
colnames(layover) <- c('net_id','net_GAP12_ctr_pct','net_GAP123_ctr_pct','net_GAP12_80pct_pct','net_GAP123_80pct_pct')
money_table <- merge(test2, layover, by='net_id', all=F)

#write.csv(money_table, file='Data/Networks/network_hub_protection_and_scores.csv', row.names=F)

# exploratory analysis of network connectivity scores, hubs and network variables
#par(mfrow=c(2,2))
plot(money_table$PCconnall ~ money_table$net_dams_n, pch=16)
plot(money_table$PCconnall ~ money_table$nHubs, pch=16)
plot(money_table$nHubs ~ money_table$net_dams_n, pch=16)
plot(money_table$nHubs ~ money_table$net_lakes_n, pch=16)

cor(money_table$PCconnall, money_table$net_dams_n, use='pairwise.complete.obs')
cor(money_table$PCconnall, money_table$nHubs, use='pairwise.complete.obs')
cor(money_table$PCconnall, money_table$net_lakes_n, use='pairwise.complete.obs')
cor(money_table$net_lakes_n, money_table$net_dams_n, use='pairwise.complete.obs')
cor(money_table$net_lakes_n, money_table$nHubs, use='pairwise.complete.obs')
cor(money_table$nHubs, money_table$net_dams_n, use='pairwise.complete.obs')

# relationship between scores and hubs
score_hub_df <- data.frame(PCconnall=money_table$PCconnall, nHubs=money_table$nHubs)
score_hub_df <- subset(score_hub_df, nHubs > 0)
par(mfrow=c(1,1))
hist(score_hub_df$nHubs)
cor(log(score_hub_df$PCconnall), log(score_hub_df$nHubs), use='pairwise.complete.obs')
cor.test(log(score_hub_df$PCconnall), log(score_hub_df$nHubs), method='pearson')


money_table$LakesHubs_pct <- money_table$nHubs/money_table$net_lakes_n
hist(money_table$LakesHubs_pct)
money_table$HubDam_pct <- money_table$nHubs/money_table$net_dams_n
hist(money_table$HubDam_pct)
money_table$HubRate <- money_table$nHubs/money_table$net_lakes_n
hist(money_table$HubRate)
money_table$DamRate <- money_table$net_dams_n/money_table$net_lakes_n
hist(money_table$DamRate)

plot(money_table$PCconnall ~ money_table$LakesHubs_pct, pch=16)
plot(money_table$PCconnall ~ money_table$HubRate, pch=16)
plot(log(money_table$PCconnall) ~ log(money_table$DamRate), pch=16)
plot(log(money_table$PCconnall) ~ log(money_table$HubRate), pch=16)


# 3d plot?
#library(rgl)
#library(scatterplot3d)
par(mfrow=c(1,1))
money_table$WSA9 <- as.factor(money_table$WSA9)
net_colors <- c('orange','lightcoral','khaki','lightgreen','gray60','dodgerblue','lightskyblue','forestgreen','yellow')
money_table$color <- net_colors[ as.numeric(money_table$WSA9)]

# hubbub <- scatterplot3d(money_table[,c(21,28,29)], main='Hubbub', 
#                      color=money_table$color, pch=16, angle=55)
# legend(hubbub$xyz.convert(3.5,0.5,0), legend=levels(money_table$WSA9), col=net_colors, pch=16, bty='n')

# erm maybe 2d is better
plot(log(money_table$PCconnall) ~ log(money_table$HubDam_pct), pch=16, xlab='log(Hub/dam ratio)', las=1,
     ylab='Network connectivity score', col=money_table$color)
legend('topright',legend=levels(money_table$WSA9), col=net_colors, pch=16, ncol=3)
cor(money_table$PCconnall, money_table$HubDam_pct, method='pearson', use='pairwise.complete.obs')

# deal with -Inf  that messes up correlation
log_table <- data.frame(logscore=log(money_table$PCconnall), logDamRate=log(money_table$DamRate))

is.na(log_table) <- sapply(log_table, is.infinite)
cor(log_table$logscore, log_table$logDamRate, use='pairwise.complete.obs', method='pearson')
summary(lm(log_table$logscore ~ log_table$logDamRate))

# supplemental figure showing log connectivity score vs. log dam rate
# jpeg('Figures/network_conn_score_dam_rateLOG.jpeg',width = 7,height = 5,units = 'in',res=300)
# plot(log(money_table$PCconnall) ~ log(money_table$DamRate), pch=16, xlab='log(Dam rate)', las=1,
#      ylab='log(Network connectivity score)', col=money_table$color)
# legend('topleft',legend=levels(money_table$WSA9), col=net_colors, pch=16, ncol=3)
# dev.off()

plot(log(money_table$PCconnall) ~ log(money_table$net_dams_n), pch=16, xlab='log(Dams)', las=1,
     ylab='log(Network connectivity score)', col=money_table$color)
legend('topleft',legend=levels(money_table$WSA9), col=net_colors, pch=16, ncol=3)

# compare dam rate across classes of conn scores
DamRate_df <- money_table[,c('PCconnall','DamRate')]
DamRate_df$Class <- ifelse(DamRate_df$PCconnall < 2, 'Low','Medium')
DamRate_df$Class <- ifelse(DamRate_df$PCconnall >= 4, 'High',DamRate_df$Class)
DamRate_df$Class <- as.factor(DamRate_df$Class)
boxplot(DamRate_df$DamRate ~ DamRate_df$Class, xlab='Connectivity score', ylab='Dam rate', las=1)

class.lm <- lm(DamRate ~ Class, data=DamRate_df)
class.aov <- aov(class.lm)
class.tukey <- TukeyHSD(class.aov)
class.tukey

plot(class.tukey)

# untransformed data
summary(money_table$PCconnall)
summary(money_table$DamRate)

plot(money_table$PCconnall ~ money_table$DamRate, pch=16, xlab='Dam rate', las=1,
     ylab='Network connectivity score', col=money_table$color)
legend('topright',legend=levels(money_table$WSA9), col=net_colors, pch=16, ncol=3)
abline(v=median(money_table$DamRate), lty=2)
abline(h=median(money_table$PCconnall), lty=2)
cor(money_table$PCconnall, money_table$DamRate, method='pearson', use='pairwise.complete.obs')

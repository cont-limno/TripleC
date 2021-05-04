###################### Connectivity scores for LAGOS-US-NETWORKS ###############################
# Date: 5-4-21
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

# rm(list=ls())
#### R libraries ####
library(dplyr)
library(raster)
library(ggplot2)
library(rgdal)
library(vegan)
library(factoextra)
library(igraph)
library(psych)
library(cluster)
library(factoextra)
library(mapdata)

#### Input data ####
# locus <- read.csv('../Data/locus/lake_information_20200520.csv', stringsAsFactors = FALSE, na.strings= c("NA", "NULL", "")) 
# names(locus)
# dim(locus)

# Lake characteristics
# char <- read.csv('../Data/locus/lake_characteristics.csv', stringsAsFactors = FALSE, na.strings= c("NA", "NULL", ""))
# head(char)
# dim(char)

lake_network_pts <- shapefile("C:/Ian_GIS/TripleC_GIS/Networks/LAGOS_1ha_pts_networks.shp")
states_shp <- shapefile("Data/lower48/lower48.shp")
states_shp <- spTransform(states_shp, crs(lake_network_pts))

# network data
nets <- read.csv('Data/Networks/network_scale_metrics.csv') 
head(nets)
str(nets)
between_cent <- read.csv("Data/Networks/betweenness_out_full.csv")

# Select network variables for clustering
dat <- nets %>% 
  dplyr::select(net_id, edge_dens, artic_count, min_cut_lat, maxkmNS, net_lakes_n, net_averagelakedistance_km,
                net_rangeorder, net_dams_n)
head(dat)

# optionally add in betweenness centrality metric
dat <- merge(dat, between_cent[,c(1,4)], by='net_id')
dat$artic_pct <- dat$artic_count/dat$net_lakes_n

# Remove 1 NA value
dat <- dat %>% filter(!is.na(maxkmNS))

# Remove networks with < 4 lakes
dat <- dat %>% 
  filter(net_lakes_n > 4)

# Add row names
row.names(dat) <- dat$net_id
dim(dat)
summary(dat)

# Remove net_id
#dat <- dat[,-1]

# Standardize variables
clus_dat <- as.data.frame(scale(dat[,c(2:ncol(dat))]))
# Correlations
cor(clus_dat)


########### Connectivity scoring #############
# for now, removing dams and artic count (highly correlated with number of lakes)
# avg lake dist highly correlated with mxkmNS and number of lakes
# took out range order because it's more about network characteristics rather than conn
# removed maxkmNS because 0.76 correlated with net_lakes_n
pca_conn <- princomp(~ edge_dens + min_cut_lat + net_lakes_n + vert_btwn_centr_norm_mean + artic_pct, 
                      data=clus_dat, cor=T, scores=T)
par(mfrow=c(1,1))
screeplot(pca_conn, type='l')
summary(pca_conn)
loadings(pca_conn)
eigenvals(pca_conn)

fviz_pca_var(pca_conn,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# To get a composite of first 2 components, can do pythagorean on scores for PCs 1 and 2, but also can extend pythagorean theorem to use all axes
pca_conn_scores <- as.data.frame(scores(pca_conn))
pca_conn_scores$PCconnall <- sqrt((pca_conn_scores$Comp.1 ^2) + (pca_conn_scores$Comp.2 ^2) + 
                                     (pca_conn_scores$Comp.3 ^2))# + (pca_conn_scores$Comp.4 ^2) + 
                                     #(pca_conn_scores$Comp.5 ^2))
hist(pca_conn_scores$PCconnall)
pca_conn_scores$net_id <- rownames(clus_dat)

pca_conn_scores_shp <- merge(lake_network_pts, pca_conn_scores, by='net_id', all.x=F)
pca_conn_scores_shp_df <- as.data.frame(pca_conn_scores_shp@data)
pca_conn_scores_shp_df$xCor <- pca_conn_scores_shp@coords[,1]
pca_conn_scores_shp_df$yCor <- pca_conn_scores_shp@coords[,2]

pca_conn_scores.point3<-ggplot(pca_conn_scores_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=PCconnall), size=2) +
  ggtitle('Network conn score')
pca_conn_scores.point3$labels$colour = 'Score' # change legend title
pca_conn_scores.point3 + geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  #scale_colour_brewer(palette = 'Set1') +
  scale_color_continuous(low='firebrick', high='dodgerblue')+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

#write.csv(pca_conn_scores, file='Data/Networks/pca_network_conn_scores.csv')

# analyze protection vs conn scores
protection <- read.csv("Data/Networks/network_protection.csv")
protection <- protection[,-1]

protection_pca <- merge(protection, pca_conn_scores, by='net_id', all=F)
protection_pca$logscore <- log(protection_pca$PCconnall)

plot(protection_pca$GAP12_ctr_pct ~ protection_pca$PCconnall)
plot(protection_pca$GAP123_ctr_pct ~ protection_pca$PCconnall)
plot(protection_pca$GAP12_80pct_pct ~ protection_pca$PCconnall)
plot(protection_pca$GAP123_80pct_pct ~ protection_pca$PCconnall)

cor(protection_pca[,c(7:10, 17:18)])

### 3d plotting of PCA results
library(rgl)
protection_pca$WSA9 <- as.factor(protection_pca$WSA9)
net_colors <- c('orange','lightcoral','khaki','lightgreen','gray60','dodgerblue','lightskyblue','forestgreen','yellow')
protection_pca$color <- net_colors[ as.numeric(protection_pca$WSA9)]

plot3d(protection_pca[,c(12:14)], col=protection_pca$color, pch=16)

# trying another method
library(scatterplot3d)

s3d <- scatterplot3d(protection_pca[,c(12:14)], main='Network connectivity scores', 
              color=protection_pca$color, pch=16, angle=55)
legend(s3d$xyz.convert(-5.5,-3,9), legend=levels(protection_pca$WSA9), col=net_colors, pch=16, bty='n')

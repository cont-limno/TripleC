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
nets <- read.csv('../Data/Networks/network_scale_metrics.csv') 
head(nets)
str(nets)
between_cent <- read.csv("Data/Networks/betweenness_out_full.csv")

# Select network variables for clustering
dat <- nets %>% 
  dplyr::select(net_id, edge_dens, artic_count, min_cut_lat, maxkmNS, net_lakes_n, net_averagelakedistance_km,
                net_rangeorder)
head(dat)

# optionally add in betweenness centrality metric
dat <- merge(dat, between_cent[,c(1,4)], by='net_id')

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
dat <- dat[,-1]

# Standardize variables
clus_dat <- scale(dat)
# Correlations
cor(clus_dat)


########### Hierarchical clustering
hc <- eclust(clus_dat, "hclust") 
str(hc)
# dendrogam
fviz_dend(hc, rect = TRUE) 
# scatter plot
fviz_cluster(hc) 

# Size of clusters
hc$size

dat %>%
  mutate(cluster = hc$cluster) %>%
  group_by(cluster) %>%
  summarise_all("median")

# save 2 cluster output
output2 <- as.data.frame(hc$cluster)
colnames(output2) <- c('cluster')
output2$net_id <- rownames(output2)
#write.csv(output2, "Data/clustering/cluster2_output.csv")

# save 8 cluster output
output8 <- as.data.frame(hc$cluster)
colnames(output8) <- c('cluster')
output8$net_id <- rownames(output8)
#write.csv(output8, "Data/clustering/cluster8_output.csv")

### Mapping cluster output

# USA basemap
usa<-map_data("usa")  #pull out the usa map
p<-ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

# merge cluster output to lake point shapefile and get coordinates for mapping
lake_network_pts_cluster <- merge(lake_network_pts, output8, by='net_id')
lake_network_pts_cluster$clustFac <- as.factor(lake_network_pts_cluster$cluster)
lake_network_pts_cluster_df <- as.data.frame(lake_network_pts_cluster@data)
lake_network_pts_cluster_df$xCor <- lake_network_pts_cluster@coords[,1]
lake_network_pts_cluster_df$yCor <- lake_network_pts_cluster@coords[,2]

lake_network_pts_cluster_xy <- as.data.frame(coordinates(lake_network_pts_cluster)[,1:2])
lake_network_pts_cluster_lagoslakeids <- lake_network_pts_cluster@data$lagoslakei
lake_network_pts_cluster_xy <- data.frame(Coord_X = lake_network_pts_cluster_xy$coords.x1, Coord_Y = lake_network_pts_cluster_xy$coords.x2, lagoslakeid=lake_network_pts_cluster_lagoslakeids)
coordinates(lake_network_pts_cluster_xy) = ~Coord_X+Coord_Y

# mapping 8 clusters
clust8_points <-ggplot(lake_network_pts_cluster_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=clustFac), size=0.75) +
  ggtitle('8 clusters')+
  scale_color_manual(values=c("firebrick", "dodgerblue","darkgreen","gray40","goldenrod","cyan","darkorchid1","khaki"), 
                     labels=c("1","2","3","4","5","6","7","8"))+
  geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin=unit(c(0.02,0.02,0.02,0.02), "cm"),
        legend.position=c(0.95,0.30))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size
clust8_points


#### Update: iterative hierarchical clustering ####
# with help from: https://www.statmethods.net/advstats/cluster.html
# Determine number of clusters
wss <- (nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dat,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

# try analysis with different numbers of clusters
distmat <- dist(dat, method='euclidean')
fit <- hclust(distmat, method='ward.D')
#fit <- diana(distmat) #divsive approach
plot(fit) #massive, useless dendrogram

# 3 clusters
par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=3, border="red")

sub_grp <- cutree(fit, k=3)
table(sub_grp)
cluster_results <- dat %>%
  mutate(cluster = sub_grp)

fviz_cluster(list(data = dat, cluster = sub_grp))

# summary table
dat %>%
  mutate(cluster = sub_grp) %>%
  group_by(cluster) %>%
  summarize_all("median")

# boxplots of conn variables across clusters
par(mfrow=c(2,4))
par(mar=c(4, 4, 2, 2)) #bot, left, top, right
boxplot_names <- colnames(cluster_results[,c(1:8)])
for (i in 1:8){
  boxplot(cluster_results[,i] ~ cluster_results[,9], xlab='Cluster number', ylab=boxplot_names[i])
}

# mapping
output3 <- as.data.frame(sub_grp)
colnames(output3) <- c('cluster')
output3$net_id <- rownames(output3)

lake_network_pts_cluster <- merge(lake_network_pts, output3, by='net_id')
lake_network_pts_cluster$clustFac <- as.factor(lake_network_pts_cluster$cluster)
lake_network_pts_cluster_df <- as.data.frame(lake_network_pts_cluster@data)
lake_network_pts_cluster_df$xCor <- lake_network_pts_cluster@coords[,1]
lake_network_pts_cluster_df$yCor <- lake_network_pts_cluster@coords[,2]

lake_network_pts_cluster_xy <- as.data.frame(coordinates(lake_network_pts_cluster)[,1:2])
lake_network_pts_cluster_lagoslakeids <- lake_network_pts_cluster@data$lagoslakei
lake_network_pts_cluster_xy <- data.frame(Coord_X = lake_network_pts_cluster_xy$coords.x1, Coord_Y = lake_network_pts_cluster_xy$coords.x2, lagoslakeid=lake_network_pts_cluster_lagoslakeids)
coordinates(lake_network_pts_cluster_xy) = ~Coord_X+Coord_Y

clust3_points <-ggplot(lake_network_pts_cluster_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=clustFac), size=0.75) +
  ggtitle('3 clusters')+
  scale_color_manual(values=c("firebrick", "dodgerblue","darkgreen","gray40","goldenrod","cyan","darkorchid1","khaki"), 
                     labels=c("1","2","3","4","5","6","7","8"))+
  geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin=unit(c(0.02,0.02,0.02,0.02), "cm"),
        legend.position=c(0.95,0.30))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size
clust3_points

# 4 clusters
par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=4, border="red")

sub_grp <- cutree(fit, k=4)
table(sub_grp)
cluster_results <- dat %>%
  mutate(cluster = sub_grp)

fviz_cluster(list(data = dat, cluster = sub_grp))

# summary table
dat %>%
  mutate(cluster = sub_grp) %>%
  group_by(cluster) %>%
  summarize_all("median")

# boxplots of conn variables across clusters
par(mfrow=c(2,4))
par(mar=c(4, 4, 2, 2)) #bot, left, top, right
boxplot_names <- colnames(cluster_results[,c(1:8)])
for (i in 1:8){
  boxplot(cluster_results[,i] ~ cluster_results[,9], xlab='Cluster number', ylab=boxplot_names[i])
}

# mapping
output4 <- as.data.frame(sub_grp)
colnames(output4) <- c('cluster')
output4$net_id <- rownames(output4)

lake_network_pts_cluster <- merge(lake_network_pts, output4, by='net_id')
lake_network_pts_cluster$clustFac <- as.factor(lake_network_pts_cluster$cluster)
lake_network_pts_cluster_df <- as.data.frame(lake_network_pts_cluster@data)
lake_network_pts_cluster_df$xCor <- lake_network_pts_cluster@coords[,1]
lake_network_pts_cluster_df$yCor <- lake_network_pts_cluster@coords[,2]

lake_network_pts_cluster_xy <- as.data.frame(coordinates(lake_network_pts_cluster)[,1:2])
lake_network_pts_cluster_lagoslakeids <- lake_network_pts_cluster@data$lagoslakei
lake_network_pts_cluster_xy <- data.frame(Coord_X = lake_network_pts_cluster_xy$coords.x1, Coord_Y = lake_network_pts_cluster_xy$coords.x2, lagoslakeid=lake_network_pts_cluster_lagoslakeids)
coordinates(lake_network_pts_cluster_xy) = ~Coord_X+Coord_Y

clust4_points <-ggplot(lake_network_pts_cluster_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=clustFac), size=0.75) +
  ggtitle('4 clusters')+
  scale_color_manual(values=c("firebrick", "dodgerblue","darkgreen","gray40","goldenrod","cyan","darkorchid1","khaki"), 
                     labels=c("1","2","3","4","5","6","7","8"))+
  geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin=unit(c(0.02,0.02,0.02,0.02), "cm"),
        legend.position=c(0.95,0.30))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size
clust4_points

# 5 clusters
par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=5, border="red")

sub_grp <- cutree(fit, k=5)
table(sub_grp)
cluster_results <- dat %>%
  mutate(cluster = sub_grp)

fviz_cluster(list(data = dat, cluster = sub_grp))

# summary table
dat %>%
  mutate(cluster = sub_grp) %>%
  group_by(cluster) %>%
  summarize_all("median")

# boxplots of conn variables across clusters
par(mfrow=c(2,4))
par(mar=c(4, 4, 2, 2)) #bot, left, top, right
boxplot_names <- colnames(cluster_results[,c(1:8)])
for (i in 1:8){
  boxplot(cluster_results[,i] ~ cluster_results[,9], xlab='Cluster number', ylab=boxplot_names[i])
}

# mapping
output5 <- as.data.frame(sub_grp)
colnames(output5) <- c('cluster')
output5$net_id <- rownames(output5)

lake_network_pts_cluster <- merge(lake_network_pts, output5, by='net_id')
lake_network_pts_cluster$clustFac <- as.factor(lake_network_pts_cluster$cluster)
lake_network_pts_cluster_df <- as.data.frame(lake_network_pts_cluster@data)
lake_network_pts_cluster_df$xCor <- lake_network_pts_cluster@coords[,1]
lake_network_pts_cluster_df$yCor <- lake_network_pts_cluster@coords[,2]

lake_network_pts_cluster_xy <- as.data.frame(coordinates(lake_network_pts_cluster)[,1:2])
lake_network_pts_cluster_lagoslakeids <- lake_network_pts_cluster@data$lagoslakei
lake_network_pts_cluster_xy <- data.frame(Coord_X = lake_network_pts_cluster_xy$coords.x1, Coord_Y = lake_network_pts_cluster_xy$coords.x2, lagoslakeid=lake_network_pts_cluster_lagoslakeids)
coordinates(lake_network_pts_cluster_xy) = ~Coord_X+Coord_Y

clust5_points <-ggplot(lake_network_pts_cluster_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=clustFac), size=0.75) +
  ggtitle('5 clusters')+
  scale_color_manual(values=c("firebrick", "dodgerblue","darkgreen","gray40","goldenrod","cyan","darkorchid1","khaki"), 
                     labels=c("1","2","3","4","5","6","7","8"))+
  geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin=unit(c(0.02,0.02,0.02,0.02), "cm"),
        legend.position=c(0.95,0.30))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size
clust5_points

# 6 clusters
par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=6, border="red")

sub_grp <- cutree(fit, k=6)
table(sub_grp)
cluster_results <- dat %>%
  mutate(cluster = sub_grp)

fviz_cluster(list(data = dat, cluster = sub_grp))

# summary table
dat %>%
  mutate(cluster = sub_grp) %>%
  group_by(cluster) %>%
  summarize_all("median")

# boxplots of conn variables across clusters
par(mfrow=c(2,4))
par(mar=c(4, 4, 2, 2)) #bot, left, top, right
boxplot_names <- colnames(cluster_results[,c(1:8)])
for (i in 1:8){
  boxplot(cluster_results[,i] ~ cluster_results[,9], xlab='Cluster number', ylab=boxplot_names[i])
}

# mapping
output6 <- as.data.frame(sub_grp)
colnames(output6) <- c('cluster')
output6$net_id <- rownames(output6)

lake_network_pts_cluster <- merge(lake_network_pts, output6, by='net_id')
lake_network_pts_cluster$clustFac <- as.factor(lake_network_pts_cluster$cluster)
lake_network_pts_cluster_df <- as.data.frame(lake_network_pts_cluster@data)
lake_network_pts_cluster_df$xCor <- lake_network_pts_cluster@coords[,1]
lake_network_pts_cluster_df$yCor <- lake_network_pts_cluster@coords[,2]

lake_network_pts_cluster_xy <- as.data.frame(coordinates(lake_network_pts_cluster)[,1:2])
lake_network_pts_cluster_lagoslakeids <- lake_network_pts_cluster@data$lagoslakei
lake_network_pts_cluster_xy <- data.frame(Coord_X = lake_network_pts_cluster_xy$coords.x1, Coord_Y = lake_network_pts_cluster_xy$coords.x2, lagoslakeid=lake_network_pts_cluster_lagoslakeids)
coordinates(lake_network_pts_cluster_xy) = ~Coord_X+Coord_Y

clust6_points <-ggplot(lake_network_pts_cluster_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=clustFac), size=0.75) +
  ggtitle('6 clusters')+
  scale_color_manual(values=c("firebrick", "dodgerblue","darkgreen","gray40","goldenrod","cyan","darkorchid1","khaki"), 
                     labels=c("1","2","3","4","5","6","7","8"))+
  geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin=unit(c(0.02,0.02,0.02,0.02), "cm"),
        legend.position=c(0.95,0.30))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size
clust6_points

# 7 clusters
par(mfrow=c(1,1))
plot(fit)
rect.hclust(fit, k=7, border="red")

sub_grp <- cutree(fit, k=7)
table(sub_grp)
cluster_results <- dat %>%
  mutate(cluster = sub_grp)

fviz_cluster(list(data = dat, cluster = sub_grp))

# summary table
dat %>%
  mutate(cluster = sub_grp) %>%
  group_by(cluster) %>%
  summarize_all("median")

# boxplots of conn variables across clusters
par(mfrow=c(2,4))
par(mar=c(4, 4, 2, 2)) #bot, left, top, right
boxplot_names <- colnames(cluster_results[,c(1:8)])
for (i in 1:8){
  boxplot(cluster_results[,i] ~ cluster_results[,9], xlab='Cluster number', ylab=boxplot_names[i])
}

# mapping
output7 <- as.data.frame(sub_grp)
colnames(output7) <- c('cluster')
output7$net_id <- rownames(output7)

lake_network_pts_cluster <- merge(lake_network_pts, output7, by='net_id')
lake_network_pts_cluster$clustFac <- as.factor(lake_network_pts_cluster$cluster)
lake_network_pts_cluster_df <- as.data.frame(lake_network_pts_cluster@data)
lake_network_pts_cluster_df$xCor <- lake_network_pts_cluster@coords[,1]
lake_network_pts_cluster_df$yCor <- lake_network_pts_cluster@coords[,2]

lake_network_pts_cluster_xy <- as.data.frame(coordinates(lake_network_pts_cluster)[,1:2])
lake_network_pts_cluster_lagoslakeids <- lake_network_pts_cluster@data$lagoslakei
lake_network_pts_cluster_xy <- data.frame(Coord_X = lake_network_pts_cluster_xy$coords.x1, Coord_Y = lake_network_pts_cluster_xy$coords.x2, lagoslakeid=lake_network_pts_cluster_lagoslakeids)
coordinates(lake_network_pts_cluster_xy) = ~Coord_X+Coord_Y

clust7_points <-ggplot(lake_network_pts_cluster_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=clustFac), size=0.75) +
  ggtitle('7 clusters')+
  scale_color_manual(values=c("firebrick", "dodgerblue","darkgreen","gray40","goldenrod","cyan","darkorchid1","khaki"), 
                     labels=c("1","2","3","4","5","6","7","8"))+
  geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin=unit(c(0.02,0.02,0.02,0.02), "cm"),
        legend.position=c(0.95,0.30))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size
clust7_points

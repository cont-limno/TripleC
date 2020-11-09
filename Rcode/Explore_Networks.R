######################## Explore LAGOS Network data ###########################################
#- Date: 10-29-20
# updated: 11-9-20
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(dplyr)
library(raster)
library(ggplot2)
library(rgdal)
library(vegan)
library(factoextra)

#### Input data ####
# network data
unidirectional <- read.csv("Data/Networks/LAGOSUS_NETSv1.0_MedRes_UniDirectionalDist.csv")
metrics_dams <- read.csv("Data/Networks/LAGOSUS_NETSv1.0_MedRes_Metrics_Dams.csv")

network_metrics <- read.csv("Data/Networks/lagosnet_metrics.csv") #from Patrick

# lake points
lakes_1ha_all_pts <- shapefile("C:/Ian_GIS/LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_pts_v0.5.shp")

# US lower 48 states
states_shp <- shapefile("Data/lower48/lower48.shp")
states_shp <- spTransform(states_shp, CRSobj=crs(lakes_1ha_all_pts))

# LAGOS-US LOCUS
lake_char <- read.csv("C:/Users/FWL/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_characteristics.csv")
lake_info <- read.csv("C:/Users/FWL/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_information.csv")

################### Main program ######################

hist(unidirectional$Total_Length_KM)

## Some lakes have multiple connected lakes

# Distance to nearest lake, any direction (up or downstream)
total_dist_summary <- unidirectional %>%
  group_by(lagoslakeid_1) %>%
  summarize(nLakes=n(), min=min(Total_Length_KM), max=max(Total_Length_KM), median=median(Total_Length_KM),
                                                                                          mean=mean(Total_Length_KM))

summary(total_dist_summary)

hist(total_dist_summary$nLakes, main='Number of network-connected lakes', xlab='Number of lakes',
     breaks=seq(0,7500,10), xlim=c(0,1000))
nrow(subset(total_dist_summary, nLakes < 10))/nrow(total_dist_summary)

hist(total_dist_summary$min, main='Distance to nearest lake, any direction (km)', xlab='km',
     breaks=seq(0,2500,50), ylim=c(0,55000))

# Distance to nearest lake, downstream
downstream_dist_summary <- unidirectional %>%
  group_by(lagoslakeid_1) %>%
  summarize(nLakes=n(), min=min(Down_Length_KM), max=max(Down_Length_KM), median=median(Down_Length_KM),
            mean=mean(Down_Length_KM))

hist(downstream_dist_summary$min, main='Distance to nearest lake, downstream (km)', xlab='km',
     breaks=seq(0,2500,50), ylim=c(0,55000))

# prepare GIS data for merging to table
US_lakes_xy <- as.data.frame(coordinates(lakes_1ha_all_pts)[,1:2])
US_lakes_lagoslakeids <- lakes_1ha_all_pts@data$lagoslakei
US_lakes_xy <- data.frame(Coord_X = US_lakes_xy$coords.x1, Coord_Y = US_lakes_xy$coords.x2, lagoslakeid=US_lakes_lagoslakeids)
coordinates(US_lakes_xy) = ~Coord_X+Coord_Y

### Mapping
# merging
downstream_dist_pts_shp <- merge(US_lakes_xy, downstream_dist_summary, by.x='lagoslakeid',by.y='lagoslakeid_1', all.x=F)
downstream_dist_shp_df <- as.data.frame(downstream_dist_pts_shp@data)
downstream_dist_shp_df$xCor <- downstream_dist_pts_shp@coords[,1]
downstream_dist_shp_df$yCor <- downstream_dist_pts_shp@coords[,2]

# map of distance to nearest downstream lake
nearest_downstream_lake_points <-ggplot(downstream_dist_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(color=min), size=0.5) +
  ggtitle('Distance to nearest downstream lake (km)')+
  scale_color_gradient(low="dodgerblue", high="firebrick")+
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
        legend.position=c('right'))+
  guides(color = guide_legend(override.aes = list(size=3.5)))#increase legend point size
nearest_downstream_lake_points

dsnname <- "C:/Users/FWL/Documents/TripleC/Data/nearest_downstream_dist"
layername <- "nearest_downstream_dist"
#writeOGR(downstream_dist_pts_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

## By lake connectivity class?
lake_conn_fluctuate <- subset(lake_char, lake_connectivity_fluctuates=='Y')[,1]

lake_conn_fluctuate_counts <- lake_char %>%
  group_by(lake_connectivity_fluctuates) %>%
  summarize(nLakes=n())

lake_conn_variables <- lake_char[,c(1,15,16,17)]
lake_conn_variables$lake_connectivity_class <- as.factor(lake_conn_variables$lake_connectivity_class)
lake_conn_variables$lake_connectivity_fluctuates <- as.factor(lake_conn_variables$lake_connectivity_fluctuates)
lake_conn_variables$lake_connectivity_permanent <- as.factor(lake_conn_variables$lake_connectivity_permanent)

total_dist_summary_conn <- merge(total_dist_summary, lake_conn_variables, by.x='lagoslakeid_1',by.y='lagoslakeid')

boxplot(total_dist_summary_conn$min ~ total_dist_summary_conn$lake_connectivity_fluctuates, xlab='Fluctuating connectivity',
        ylab='Distance (km)', main='Distance to nearest lake, any direction')

boxplot(total_dist_summary_conn$min ~ total_dist_summary_conn$lake_connectivity_permanent, xlab='Permanent connectivity class',
        ylab='Distance (km)', main='Distance to nearest lake, any direction')


######## Exploring metrics/dams dataset ########
# What have we here?
hist(metrics_dams$lake_net_upstreamlake_km)
hist(metrics_dams$lake_net_downstreamlake_km)
hist(metrics_dams$lake_net_bidirectionallake_km)
hist(metrics_dams$lake_net_upstreamlake_n)
hist(metrics_dams$lake_net_downstreamlake_n)
hist(metrics_dams$lake_net_lakeorder)
hist(metrics_dams$lake_net_lnn)
hist(metrics_dams$net_lakes_n)
hist(metrics_dams$net_averagelakedistance_km)
hist(metrics_dams$net_averagelakearea_ha)
hist(metrics_dams$lake_net_nearestdamdown_km)
hist(metrics_dams$lake_net_totaldamdown_n)
hist(metrics_dams$lake_net_nearestdamup_km)
hist(metrics_dams$lake_net_totaldamup_n)
hist(metrics_dams$net_dams_n)

# retain numeric columns only + net_id
metrics_dams_keep <- metrics_dams[,c(1:13,15:16,18,21)]
# without MS river basin lakes
metrics_dams_keep_noMS <- subset(metrics_dams_keep, net_id > 1)

cor(metrics_dams_keep, method='pearson', use="pairwise.complete.obs")
cor(metrics_dams_keep_noMS, method='pearson', use="pairwise.complete.obs")

## PCA
pca_metrics_noMS <- princomp(~ lake_net_upstreamlake_km + lake_net_downstreamlake_km + lake_net_bidirectionallake_km +
                               lake_net_upstreamlake_n + lake_net_downstreamlake_n + lake_net_lakeorder + 
                               lake_net_lnn + net_lakes_n + net_averagelakedistance_km + net_averagelakearea_ha +
                               lake_net_nearestdamdown_km + lake_net_totaldamdown_n + lake_net_nearestdamup_km +
                               lake_net_totaldamup_n + net_dams_n,
                      data=metrics_dams_keep_noMS, cor=T, scores=T)
par(mfrow=c(1,1))
screeplot(pca_metrics_noMS, type='l')
summary(pca_metrics_noMS)
loadings(pca_metrics_noMS)
eigenvals(pca_metrics_noMS)

#help from: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
fviz_pca_var(pca_metrics_noMS,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


### Network metrics from Patrick
cor(network_metrics, method='pearson', use='pairwise.complete.obs')

network_metrics_noMS <- subset(network_metrics, net_id > 1) #get rid of MS river basin
cor(network_metrics_noMS, method='pearson', use='pairwise.complete.obs')

pca_network_noMS <- princomp(~ vertices_count + edges_count + artic_count +
                                max_cliques + mean_degree_norm + far_vert_2 + far_vert_dist + edge_conn +
                                edge_dens + min_cut + far_min_cut + strength,
                              data=network_metrics_noMS, cor=T, scores=T)

par(mfrow=c(1,1))
screeplot(pca_network_noMS, type='l')
summary(pca_network_noMS)
loadings(pca_network_noMS)
eigenvals(pca_network_noMS)

#help from: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
fviz_pca_var(pca_network_noMS,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


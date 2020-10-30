######################## Explore LAGOS Network data ###########################################
#- Date: 10-29-20
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(dplyr)
library(raster)
library(ggplot2)
library(rgdal)

#### Input data ####
# network data
unidirectional <- read.csv("Data/Networks/LAGOSUS_NETSv1.0_MedRes_UniDirectionalDist.csv")

# lake points
lakes_1ha_all_pts <- shapefile("C:/Ian_GIS/LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_pts_v0.5.shp")

# US lower 48 states
states_shp <- shapefile("Data/lower48/lower48.shp")
states_shp <- spTransform(states_shp, CRSobj=crs(lakes_1ha_all_pts))

################### Main program ######################

hist(unidirectional$Total_Length_KM)

## Some lakes have multiple connected lakes

# Distance to nearest lake, any direction (up or downstream)
total_dist_summary <- unidirectional %>%
  group_by(lagoslakeid_1) %>%
  summarize(nLakes=n(), min=min(Total_Length_KM), max=max(Total_Length_KM), median=median(Total_Length_KM),
                                                                                          mean=mean(Total_Length_KM))

summary(total_dist_summary)

hist(total_dist_summary$nLakes)
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

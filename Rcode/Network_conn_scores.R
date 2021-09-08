###################### Connectivity scores for LAGOS-US-NETWORKS ###############################
# Date: 5-4-21
# updated: 9-8-21
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/immcc/Documents/TripleC")
setwd("C:/Users/FWL/Documents/TripleC")

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
library(scales)

#### Input data ####
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
# King, K., Wang, Q., Rodriguez, L.K., and Cheruvelil, K.S. 2021. Lake networks and connectivity metrics
# for the conterminous U.S. (LAGOS-US NETWORKS v1). Limnology and Oceanography Letters. https://doi.org/10.1002/lol2.10204 

lake_network_pts <- shapefile("C:/Ian_GIS/TripleC_GIS/Networks/LAGOS_1ha_pts_networks.shp")
states_shp <- shapefile("Data/lower48/lower48.shp")
states_shp <- spTransform(states_shp, crs(lake_network_pts))

# network data
nets <- read.csv('Data/Networks/network_scale_metrics.csv') 
head(nets)
str(nets)
between_cent <- read.csv("Data/Networks/betweenness_out_full.csv")
net_lakes <- read.csv("Data/Networks/nets_networkmetrics_medres_dams.csv")

# get lake elevation data in LAGOS-US-LOCUS
# Smith, N.J., K.E. Webster, L.K. Rodriguez, K.S. Cheruvelil, and P.A. Soranno. 2021. 
# LAGOS-US LOCUS v1.0: Data module of location, identifiers, and physical characteristics of lakes and their watersheds in the conterminous U.S. ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/e5c2fb8d77467d3f03de4667ac2173ca (Accessed 2021-09-07).
lakeinfo <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_information.csv")
lakeinfo <- read.csv("C:/Users/FWL/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_information.csv")

lakeinfo_net <- subset(lakeinfo, lagoslakeid %in% net_lakes$lagoslakeid)
lakeinfo_net <- lakeinfo_net[,c('lagoslakeid','lake_elevation_m')]

net_lakes <- merge(net_lakes, lakeinfo_net, by='lagoslakeid')

net_elevation <- net_lakes %>%
  group_by(net_id) %>%
  summarize(elev_m_max=max(lake_elevation_m, na.rm=T),
            elev_m_min=min(lake_elevation_m, na.rm=T),
            elev_m_sd=sd(lake_elevation_m, na.rm=T))
net_elevation$elev_m_range <- net_elevation$elev_m_max-net_elevation$elev_m_min 
hist(net_elevation$elev_m_sd)
hist(net_elevation$elev_m_range)

nets <- merge(nets, net_elevation, by='net_id')

# Select network variables for analysis
dat <- nets %>% 
  dplyr::select(net_id, edge_dens, artic_count, min_cut_lat, maxkmNS, net_lakes_n, net_averagelakedistance_km,
                net_rangeorder, net_dams_n, elev_m_range)
head(dat)

# optionally add in betweenness centrality metric
dat <- merge(dat, between_cent[,c(1,4)], by='net_id')
dat$artic_pct <- dat$artic_count/dat$net_lakes_n
#dat$artic_pct_inv <- 1-dat$artic_pct # make it so higher number represents greater resistance to fragmentation
dat$artic_pct_inv <- scales::rescale(dat$artic_pct, to=c(1,0.01))
hist(dat$artic_pct)
hist(dat$artic_pct_inv)

# calcuate dam rate variable
dat$DamRate <- dat$net_dams_n/dat$net_lakes_n
hist(dat$DamRate)
dat$DamRate_inv <- scales::rescale(dat$DamRate, to=c(1,0.01))
#dat$DamRate_inv <- 1-dat$DamRate #make it so more dams is bad for score
hist(dat$DamRate_inv)

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
# pca_conn <- princomp(~ edge_dens + min_cut_lat + net_lakes_n + vert_btwn_centr_norm_mean + artic_pct_inv, 
#                       data=clus_dat, cor=T, scores=T)
# par(mfrow=c(1,1))
# screeplot(pca_conn, type='l')
# summary(pca_conn)
# loadings(pca_conn)
# eigenvals(pca_conn)
# 
# fviz_pca_var(pca_conn,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 
# fviz_pca_biplot(pca_conn, addEllipses = T)
# 
# fviz_pca_ind(pca_conn, addEllipses = T)
# 
# # To get a composite of first 2 components, can do pythagorean on scores for PCs 1 and 2, but also can extend pythagorean theorem to use all axes
# pca_conn_scores <- as.data.frame(scores(pca_conn))
# pca_conn_scores$PCconnall <- sqrt((pca_conn_scores$Comp.1 ^2) + (pca_conn_scores$Comp.2 ^2) + 
#                                      (pca_conn_scores$Comp.3 ^2) + (pca_conn_scores$Comp.4 ^2) + 
#                                      (pca_conn_scores$Comp.5 ^2))
# hist(pca_conn_scores$PCconnall, main='Network connectivity scores')
# pca_conn_scores$net_id <- rownames(clus_dat)
# 
# # save nice conn score histogram
# jpeg('Figures/conn_score_histogram.jpeg',width = 7,height = 5,units = 'in',res=300)
#   hist(pca_conn_scores$PCconnall, main='Network connectivity scores', 
#        xlab='Score', las=1, breaks=seq(0,14,1), cex.main=2, cex.lab=1.5, cex.axis=1.5,
#        col=c('khaki','khaki','mediumseagreen','mediumseagreen',
#              'dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3'))
# dev.off()

# # Mapping scores
# pca_conn_scores_shp <- merge(lake_network_pts, pca_conn_scores, by='net_id', all.x=F)
# pca_conn_scores_shp_df <- as.data.frame(pca_conn_scores_shp@data)
# pca_conn_scores_shp_df$xCor <- pca_conn_scores_shp@coords[,1]
# pca_conn_scores_shp_df$yCor <- pca_conn_scores_shp@coords[,2]
# 
# pca_conn_scores.point3<-ggplot(pca_conn_scores_shp_df, aes(x=xCor,y=yCor))+
#   geom_point(aes(colour=PCconnall), size=2) +
#   ggtitle('Network conn score')
# pca_conn_scores.point3$labels$colour = 'Score' # change legend title
# pca_conn_scores.point3 + geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
#   #scale_colour_brewer(palette = 'Set1') +
#   scale_color_continuous(low='firebrick', high='dodgerblue')+
#   theme_bw() + 
#   theme(axis.text = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank(),
#         #panel.border = element_blank(),
#         panel.grid = element_blank(),
#         axis.title = element_blank())

#write.csv(pca_conn_scores, file='Data/Networks/pca_network_conn_scores.csv')

# ## PCA with Dam Rate variable
# variables used in first submission
#pca_conn_DR <- princomp(~ edge_dens + min_cut_lat + net_lakes_n + vert_btwn_centr_norm_mean + artic_pct_inv + DamRate_inv + maxkmNS,
#                        data=clus_dat, cor=T, scores=T)
# revised set of variables: dropping net_lakes_n in favor of net_averagelakedistance_km (0.8 corr with net_lake_n anyway)
# also adding elev_m_range: strongest corr is with maxkmNS (0.60)
pca_conn_DR <- princomp(~ edge_dens + min_cut_lat + net_averagelakedistance_km + vert_btwn_centr_norm_mean + artic_pct_inv + DamRate_inv + maxkmNS + elev_m_range,
                        data=clus_dat, cor=T, scores=T)
par(mfrow=c(1,1))
screeplot(pca_conn_DR, type='l')
summary(pca_conn_DR)
loadings(pca_conn_DR)
eigenvals(pca_conn_DR)

fviz_pca_var(pca_conn_DR,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# parallel analysis to determine number of PCs to "retain":
# https://www.r-bloggers.com/2016/04/determining-the-number-of-factors-with-parallel-analysis-in-r/
library(relimp, pos = 4)
library(paran)

paran(clus_dat[c("edge_dens","min_cut_lat","net_averagelakedistance_km","vert_btwn_centr_norm_mean",
                 "artic_pct_inv","DamRate_inv", "maxkmNS", "elev_m_range")], iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = FALSE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      width = 640, height = 640, grdevice = "png", seed = 0)


# To get a composite of first 2 components, can do pythagorean on scores for PCs 1 and 2, but also can extend pythagorean theorem to use all axes
pca_conn_DR_scores <- as.data.frame(scores(pca_conn_DR))
pca_conn_DR_scores$PCconnall <- sqrt((pca_conn_DR_scores$Comp.1 ^2) + (pca_conn_DR_scores$Comp.2 ^2))# +
                                       #(pca_conn_DR_scores$Comp.3 ^2) + (pca_conn_DR_scores$Comp.4 ^2) +
                                       #(pca_conn_DR_scores$Comp.5 ^2) + (pca_conn_DR_scores$Comp.6 ^2))

hist(pca_conn_DR_scores$PCconnall, main='Network connectivity scores')
pca_conn_DR_scores$net_id <- rownames(clus_dat)

# save nice conn score histogram
tiff('Figures/conn_score_histogram_wDamRate_revised.tif',width = 7,height = 5,units = 'in',res=300)
hist(pca_conn_DR_scores$PCconnall, main='Network connectivity scores',
     xlab='Score', las=1, breaks=seq(0,13,1), cex.main=1.5, cex.lab=1, cex.axis=1, xlim=c(0,14),
     col=c('khaki','khaki','mediumseagreen','mediumseagreen',
           'dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3','dodgerblue3'))
dev.off()
# 
# 
pca_conn_DR_scores_shp <- merge(lake_network_pts, pca_conn_DR_scores, by='net_id', all.x=F)
pca_conn_DR_scores_shp_df <- as.data.frame(pca_conn_DR_scores_shp@data)
pca_conn_DR_scores_shp_df$xCor <- pca_conn_DR_scores_shp@coords[,1]
pca_conn_DR_scores_shp_df$yCor <- pca_conn_DR_scores_shp@coords[,2]

pca_conn_DR_scores.point3<-ggplot(pca_conn_DR_scores_shp_df, aes(x=xCor,y=yCor))+
  geom_point(aes(colour=PCconnall), size=2) +
  ggtitle('Network conn score')
pca_conn_DR_scores.point3$labels$colour = 'Score' # change legend title
pca_conn_DR_scores.point3 + geom_path(data=states_shp,aes(long,lat,group=group),colour='black') + coord_equal()+
  #scale_colour_brewer(palette = 'Set1') +
  scale_color_continuous(low='firebrick', high='dodgerblue')+
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

#write.csv(pca_conn_DR_scores, file='Data/Networks/pca_network_conn_scores_wDamRate.csv')
#write.csv(pca_conn_DR_scores, file='Data/Networks/pca_network_conn_scores_revised.csv')


#### analyze protection vs conn scores
protection <- read.csv("Data/Networks/network_protection.csv") #(from LakeNetworkProtectionAnalysis.R)
protection <- protection[,-1]

protection_pca <- merge(protection, pca_conn_DR_scores, by='net_id', all=F)
protection_pca$logscore <- log(protection_pca$PCconnall)

plot(protection_pca$GAP12_ctr_pct ~ protection_pca$PCconnall)
plot(protection_pca$GAP123_ctr_pct ~ protection_pca$PCconnall)
plot(protection_pca$GAP12_80pct_pct ~ protection_pca$PCconnall)
plot(protection_pca$GAP123_80pct_pct ~ protection_pca$PCconnall)

cor(protection_pca[,c(7:10, 18:19)])

# # same analysis with PCA with DamRate variable
# protection_pca <- merge(protection, pca_conn_DR_scores, by='net_id', all=F)
# protection_pca$logscore <- log(protection_pca$PCconnall)
#
# plot(protection_pca$GAP12_ctr_pct ~ protection_pca$PCconnall)
# plot(protection_pca$GAP123_ctr_pct ~ protection_pca$PCconnall)
# plot(protection_pca$GAP12_80pct_pct ~ protection_pca$PCconnall)
# plot(protection_pca$GAP123_80pct_pct ~ protection_pca$PCconnall)
#
# cor(protection_pca[,c(7:10, 18:19)])


# ### 3d plotting of PCA results
# library(rgl)
# protection_pca$WSA9 <- as.factor(protection_pca$WSA9)
# net_colors <- c('orange','lightcoral','khaki','lightgreen','gray60','dodgerblue','lightskyblue','forestgreen','yellow')
# protection_pca$color <- net_colors[ as.numeric(protection_pca$WSA9)]
#
# plot3d(protection_pca[,c(12:14)], col=protection_pca$color, pch=16)
#
# # trying another method
# library(scatterplot3d)
#
# s3d <- scatterplot3d(protection_pca[,c(12:14)], main='Network connectivity scores',
#               color=protection_pca$color, pch=16, angle=55)
# legend(s3d$xyz.convert(-5.5,-3,9), legend=levels(protection_pca$WSA9), col=net_colors, pch=16, bty='n')
#
# # and another
#
# library(pca3d)
# pca3d(pca_conn, show.ellipses = T, show.plane = F)
# groups=protection_pca$WSA9
# pca3d(pca_conn, group=groups, legend='topleft')


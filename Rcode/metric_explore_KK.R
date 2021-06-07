######################## Explore LAGOS Network data ###########################################
#- Date: 05-NOV-2020
# updated: 05-JAN-2021
# Author: Katelyn King
################################################################################################

#### R libraries ####
library(dplyr)
library(ggplot2)
library(igraph) #distance table and fun figures 
library(mapdata)

#### Input data ####
#LAGOS_NETWORKS_v1 all lakes 
lagos_networks_all <- read.csv("Data/Networks/nets_networkmetrics_medres.csv") 
#just network data
lagos_networks<-lagos_networks_all[!duplicated(paste(lagos_networks_all$net_id)),] %>% 
  dplyr::select(lagoslakeid, net_id, net_lakes_n, net_averagelakedistance_km, net_averagelakearea_ha, net_dams_n, lake_nets_lnn, lake_nets_lakeorder )
#graph network metrics
graph_networks<- read.csv("Data/Networks/lagosnet_metrics.csv") 
#metrics with geo location 
graph_geo<- read.csv("Data/Networks/lagosnet_geo.csv")
#hub lakes 
hub_lakes<- read.csv("Data/Networks/VIP_lakes.csv")

#bidirectional dist table - version 7Dec2020
bidir<-read.csv("/Users/katelynking/Desktop/Conny Network/med_res_complete/flowtable and distance/nets__binetworkdistance_medres.csv") %>%
  dplyr::select(binet_lagoslakeid1, binet_lagoslakeid2, binet_streamlength_total_km)
#downloaded from dropbox on 4Nov2020 version from 29May2020
lake_info<-read.csv( "/Users/katelynking/Desktop/Cont Limno/LAGOS_US/lake_information.csv") %>% 
  dplyr::select(lagoslakeid, lake_lat_decdeg, lake_lon_decdeg) 
#lake_characteristics 
lake_char<-read.csv( "/Users/katelynking/Desktop/Cont Limno/LAGOS_US/lake_characteristics.csv") %>% 
  dplyr::select(lagoslakeid, lake_totalarea_ha) 

################### Main program ######################

#* create a test dataset for igraph code  
test_dat <- data.frame(
  Lake1 = c("A", "B", "C", "C"),
  Lake2 = c("B", "C", "D", "E"),
  Dist = c(5, 6, 2, 10)
)


test_graph = graph_from_data_frame(test_dat, directed = FALSE)
plot(test_graph, vertex.size=40, edge.width=5*edge.attributes(test_graph)[["Dist"]])
distances(test_graph, weights = test_dat$Dist, mode="out")


### Exploring metrics for habitat availability #### 
#remove mississippi 
lagos_net_sub<-filter(lagos_networks, net_id != 1)
graph_net_sub<-filter(graph_networks, net_id !=1) 
graph_geo_sub<-filter(graph_geo, net_id !=1) 

#Number of edges in network = number of streams 
hist(graph_net_sub$edges_count)
#number of lakes
hist(lagos_net_sub$net_lakes_n)
plot(log(graph_networks$vertices_count), log(graph_networks$edges_count))

#*Avg dist between lakes (within network) ####
hist(lagos_net_sub$net_averagelakedistance_km)
hist(log(lagos_net_sub$net_averagelakedistance_km))

#distance to lake of similar size class ?

#*node density within network # note the Mississippi is not in here ####
graph_net_sub$desn_NS<-graph_net_sub$vertices_count /graph_geo_sub$maxkmNS
hist(graph_net_sub$desn_NS)
graph_net_sub$dens_EW<-graph_net_sub$vertices_count /graph_geo_sub$maxkmEW
hist(graph_net_sub$dens_EW)
plot(log(graph_net_sub$desn_NS),log(graph_net_sub$dens_EW))

#* average Lake Order per network ####
hist(lagos_networks_all$lake_net_lakeorder)
net_avgorder<-plyr::ddply(lagos_networks_all, "net_id", summarize,  net_avgorder=round(mean(lake_nets_lakeorder, na.rm=TRUE)) )
net_minorder<-plyr::ddply(lagos_networks_all, "net_id", summarize,  net_minorder=round(min(lake_nets_lakeorder, na.rm=TRUE)) )
net_maxorder<-plyr::ddply(lagos_networks_all, "net_id", summarize,  net_maxorder=round(max(lake_nets_lakeorder, na.rm=TRUE)) )

net_order <-left_join(net_avgorder, net_maxorder) %>%
  left_join(net_minorder)
net_order$net_rangeorder<-net_order$net_maxorder - net_order$net_minorder

lagos_net<-select(lagos_networks, net_id, net_lakes_n, net_averagelakedistance_km, net_averagelakearea_ha, net_dams_n)
network_scale_metrics<-left_join(lagos_net, net_order) %>%
                            left_join(graph_networks) %>%
                             left_join(graph_geo)

write.csv(network_scale_metrics, "Data/Networks/network_scale_metrics.csv", row.names = FALSE)
hist(net_avgorder$net_avgorder)
lagos_networks_plus<-left_join(lagos_networks, net_avgorder)
plot(lagos_networks_plus$net_avgorder,lagos_networks_plus$net_averagelakearea_ha)
plot(lagos_networks_plus$net_avgorder,log(lagos_networks_plus$net_averagelakearea_ha))

# average LNN per network 
net_avglnn<-plyr::ddply(lagos_networks_all, "net_id", summarize,  net_avglnn=round(mean(lake_nets_lnn)) )
hist(net_avglnn$net_avglnn)

#*graph visuals ####
# using igraph package 
net_id<-dplyr::select(lagos_networks_all, lagoslakeid, net_id)

#join distances with net_ids so that I can pull out specific networks to graph 
bidist_net<-left_join(bidir, net_id, by = c("binet_lagoslakeid1" = "lagoslakeid"))

#choose two networks with 5 lakes, but different distances, lakes sizes, and edges 
#net_id 821 
net_821<-filter(bidist_net, net_id == "821") %>% dplyr::select(binet_lagoslakeid1, binet_lagoslakeid2, binet_streamlength_total_km) 
#remove duplicate rows
net_821<-net_821[!duplicated(paste(net_821$binet_streamlength_total_km)),] 
graph_821 = graph_from_data_frame(net_821, directed = FALSE)
plot(graph_821, vertex.size=40, vertex.color = "lightblue", edge.color = "darkblue", edge.width=edge.attributes(graph_821)[["binet_streamlength_total_km"]])
plot(graph_821, vertex.size=40, vertex.color = "lightblue", edge.color = "darkblue")

#and net_id 264
net_264 <-filter(bidist_net, net_id == "264")%>% dplyr::select(binet_lagoslakeid1, binet_lagoslakeid2, binet_streamlength_total_km) 
net_264<-net_264[!duplicated(paste(net_264$binet_streamlength_total_km)),] 
net_264 = graph_from_data_frame(net_264, directed = FALSE)
plot(net_264, vertex.size=40, vertex.color = "lightblue", edge.color = "darkblue", edge.width=edge.attributes(net_264)[["binet_streamlength_total_km"]])
plot(net_264, vertex.size=40, vertex.color = "lightblue", edge.color = "darkblue")

#* maps #### 

networks_ll<-left_join(lagos_networks_all, lake_info, by = "lagoslakeid")

usa<-map_data("usa")  #pull out the usa map
p<-ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

#pull out networks with 2 lakes 
two_lake<-filter(networks_ll, net_lakes_n == 2)
two_lake$net_id<-as.factor(two_lake$net_id)
p+geom_point(data=two_lake, size = 2, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, colour=net_id), show.legend = FALSE)  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))

#pull out large lake networks 
big_net<-filter(networks_ll, net_lakes_n > 1000)
big_net$net_id<-as.factor(big_net$net_id)
p+geom_point(data=big_net, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, colour=net_id), show.legend = FALSE)  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))

#looks at the average distances to the nearest lake
p+geom_point(data=networks_ll, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, colour=net_averagelakedistance_km))  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))

#pull out mississippi 
ll_sub<-filter(networks_ll, net_id != 1)
p+geom_point(data=ll_sub, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, colour=net_averagelakedistance_km))  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA))

#### conceptual diagram figure for temperature #### 
#load data 
lagos_networks_all <- read.csv("Data/Networks/nets_networkmetrics_medres.csv") %>%
  dplyr::select(lagoslakeid, net_id, net_lakes_n) #all lakes
lake_info<-read.csv( "/Users/katelynking/Desktop/Cont Limno/lake_link_Jan2021.csv") %>% 
  dplyr::select(lagoslakeid, lake_nhdid, nhdplusv2_comid, lake_lat_decdeg, lake_lon_decdeg) 

nets_ll<-left_join(lagos_networks_all, lake_info,  by = "lagoslakeid")
nets_ll<-nets_ll[!duplicated(paste(nets_ll$lagoslakeid)),]  #remove duplicates 

### select out network in Michigan that Ian has in panel A 
michigan<-map_data(map="state", region = "michigan")  #pull out the usa map
MI<-ggplot(data = michigan) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

#* color based on temperature from Collins et al ####
#average maxtemp for Jun,Jul, Aug at the lake point location. Collins et al. Dec 23, 2020 download 
temp<-read.csv("/Users/katelynking/Desktop/MSU Research/Chap 4 Conny/air_temp_Ian/lagoslakeid_PRISM_Normals_1981_2010.csv") %>%
  dplyr::select(lagoslakeid, summer_tmax) 
net_28<-filter(nets_ll, net_id == 7) #select out MI network 
net_temp<-left_join(net_28, temp)
MI+geom_point(data=net_temp, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, col=summer_tmax))  +
  scale_color_gradient(low="blue", high="red") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) + 
  theme(legend.position = c(0.2, 0.4))

### try to use my own PRISM data since not all points match 
#* pull PRISM points ####
library(raster)
#downloaded Jan 13, 2021; 30 year normal for 1981-2010 tmax for jun, july, aug 
tamx_jun<-raster("/Users/katelynking/Desktop/PRISM/PRISM_tmax_30yr_normal_4kmM2_JUNE_bil/PRISM_tmax_30yr_normal_4kmM2_06_bil.bil")
tamx_jul<-raster("/Users/katelynking/Desktop/PRISM/PRISM_tmax_30yr_normal_4kmM2_JULY_bil/PRISM_tmax_30yr_normal_4kmM2_07_bil.bil")
tamx_aug<-raster("/Users/katelynking/Desktop/PRISM/PRISM_tmax_30yr_normal_4kmM2_AUG_bil/PRISM_tmax_30yr_normal_4kmM2_08_bil.bil")
crs(tamx_jun)
summer_means=stack(c(tamx_jun, tamx_jul,tamx_aug))
tmean=mean(summer_means)

#project to NAD 83 
net28_ll <- SpatialPointsDataFrame(coords=net_28[,c("lake_lon_decdeg","lake_lat_decdeg")], data=net_28,
                                    proj4string=CRS("+proj=longlat +datum=NAD83 +ellps=GRS80")) 

#extract(x, y, ...) #where x is the raster object and y are the points represented by a two column matrix or data.frame
net_28$summer_tmax<-raster::extract(tmean, net28_ll, na.rm=T)

jpeg('Figures/MI_net_temp.jpeg',width = 10,height = 7,units = 'in',res=600)
MI+geom_point(data=net_28, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, col=summer_tmax))  +
  scale_color_gradient(low="blue", high="red") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) + 
  theme(legend.position = c(0.2, 0.4))
dev.off()


### MN data 
MN_dat<-read.csv("/Users/katelynking/Desktop/static_lake_predictors.csv")
MN_info<-read.csv("/Users/katelynking/Desktop/mndow_nhdhr_xwalk.csv")
library(tidyr)
MN<-MN_info %>% 
  separate(DOW, c("title", "DOW"), "_") %>%
  separate(site_id, c("titlenhd", "nhdr"), "_") %>%
  select(DOW, nhdr)
MN$DOW<-as.integer(MN$DOW)
MN$nhdr<-as.factor(MN$nhdr)
MN_temp<-left_join(MN_dat, MN) %>%
            left_join(lake_info, by=c("nhdr"="lake_nhdid")) %>%
              left_join(nets_ll, by="lagoslakeid")

MN_temp<-MN_temp[!duplicated(paste(MN_temp$lagoslakeid)),]  #remove duplicates 

#try net 13 or 11
minn<-map_data(map="state", region = "minnesota")  #pull out the usa map
MN<-ggplot(data = minn) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

net_11<-filter(MN_temp, net_id == 11) #select out MI network 

jpeg('Figures/MN_net_temp.jpeg',width = 10,height = 7,units = 'in',res=600)
MN+geom_point(data=net_11, size = 1, aes(x = lake_lon_decdeg.x, y = lake_lat_decdeg.x, col=mean_gdd))  +
  scale_color_gradient(low="blue", high="red") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) + 
    theme(legend.position = c(0.8, 0.4))
dev.off()

#### MI data modeled surface water temp #### 
temp<-read.csv("/Users/katelynking/Desktop/UofM/CHANGES/SNT_data/lake_surface_temp.csv") %>%
  dplyr:: select(IHDLKID, TAVE.mean) %>%
  rename(ihdlkid = IHDLKID, mean_temp=TAVE.mean)
temp$ihdlkid<-as.factor(temp$ihdlkid)

dd_temp<-read.csv("/Users/katelynking/Desktop/UofM/CHANGES/SNT_data/lake_degree_days_year.csv") %>%
  dplyr:: select(IHDLKID, DD_2019) %>%
  rename(ihdlkid = IHDLKID, degree_days=DD_2019)
dd_temp$ihdlkid<-as.factor(dd_temp$ihdlkid)

#michigan map 
michigan<-map_data(map="state", region = "michigan")  #pull out the usa map
MI<-ggplot(data = michigan) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

net_28<-filter(nets_ll, net_id == 7) #select out MI network 
net_temp<-left_join(net_28, temp, by = c('lake_nhdid' = 'ihdlkid'))
net_dd_temp<-left_join(net_28, dd_temp, by = c('lake_nhdid' = 'ihdlkid'))

MI+geom_point(data=net_temp, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, col=mean_temp))  +
  scale_color_gradient(low="blue", high="red") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) + 
  theme(legend.position = c(0.2, 0.4))

MI+geom_point(data=net_dd_temp, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, col=degree_days))  +
  scale_color_gradient(low="blue", high="red") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(colour = "black", size=.5, fill=NA)) + 
  theme(legend.position = c(0.2, 0.4))


#### dams on hub lakes #### 
hub_char<-left_join(hub_lakes, lagos_networks_all)

#new columns for directly up or down dam 
hub_char <- hub_char %>%
  mutate(down_dam = case_when(is.na(lake_nets_nearestdamdown_km) ~ "N",   #if ~ then 
                            TRUE ~"Y"
  )) %>%
  mutate(up_dam = case_when(is.na(lake_nets_nearestdamup_km) ~ "N",   #if ~ then 
                            TRUE ~"Y"
  ))

# summary of hub_char table to see how many hub lakes have dams
hub_char$down_dam<-as.factor(hub_char$down_dam)
hub_char$up_dam<-as.factor(hub_char$up_dam)
summary(hub_char)

#figure of dams in relation to hub lakes 
category  <- c("dam_on_lake", "down_dam", "up_dam")
no <- c(1305, 512, 1007)
yes <- c(357, 1150, 655)
df <- data.frame(category, no, yes)
df_long <- reshape2::melt(df, id = "category")

#plot dams on lake, upstream dam, downstream dam 
ggplot(df_long, aes(x = category, y = value, fill = variable, label = value)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))


#number of dams on the network of the hub lake 
hist(hub_char$net_dams_n) #32 hub lakes have no dams in the network 

#number upstream dams 
hist(hub_char$lake_nets_totaldamup_n) 

#number downstream dams 
hist(hub_char$lake_nets_totaldamdown_n) 

summary(hub_char)
hub_nets<-hub_char[!duplicated(paste(hub_char$net_id)),] #hub networks 246 networks with hub lakes 

#### dams on networks with conny scores #### 
nets_with_conn<-filter(lagos_networks, net_lakes_n > 4)
net_with_no_dams<-filter(nets_with_conn, net_dams_n == 0)

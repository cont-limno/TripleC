######################## Explore LAGOS Network data ###########################################
#- Date: 05-NOV-2020
# updated: 05-JAN-2021
# Author: Katelyn King
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(dplyr)
library(ggplot2)
library(igraph) #distance table and fun figures 
library(mapdata)

#### Input data ####
#LAGOS_NETWORKS_v1
lagos_networks_all <- read.csv("Data/Networks/nets_networkmetrics_medres.csv") %>%
  dplyr::select(lagoslakeid, net_id, net_lakes_n, net_averagelakedistance_km, net_averagelakearea_ha, net_dams_n, lake_nets_lnn, lake_nets_lakeorder ) #all lakes
lagos_networks<-lagos_networks_all[!duplicated(paste(lagos_networks_all$net_id)),] #just network data
#graph network metrics 
graph_networks<- read.csv("Data/Networks/lagosnet_metrics.csv")
#metrics with geo location 
graph_geo<- read.csv("Data/Networks/lagosnet_geo.csv")

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

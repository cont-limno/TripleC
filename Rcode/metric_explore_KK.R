######################## Explore LAGOS Network data ###########################################
#- Date: 05-NOV-2020
# updated: 
# Author: Katelyn King, Patrick Hanley 
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(dplyr)
#library(raster)
library(ggplot2)
#library(rgdal)
library(igraph) #distance table and fun figures 

#### Input data ####
# network data
metrics <- read.csv("Data/Networks/LAGOSUS_NETSv1.0_MedRes_Metrics_Dams.csv")
unidirectional <- read.csv("Data/Networks/LAGOSUS_NETSv1.0_MedRes_UniDirectionalDist.csv")

################### Main program ######################

##### network summaries #### 
#select out network metrics 
nets<-dplyr::select(metrics, lagoslakeid, net_id, net_lakes_n, net_averagelakearea_ha, net_averagelakedistance_km, net_dams_n)

#look at how many lakes in a lake network 
sum_table<-nets %>%
  dplyr::group_by(net_id) %>%
  dplyr::summarise(n_distinct(lagoslakeid))

# all networks, how many lakes in a network freq plot  
ggplot(sum_table, aes(x=`n_distinct(lagoslakeid)`)) +  #frequency of networks with lake#
  geom_histogram() +
  labs(x="# lakes in a network", y = "frequency")+
  theme_classic()


networks<-nets[!duplicated(paste(nets$net_id)),] 
hist(networks$net_averagelakearea_ha, xlim=c(0, 10000), breaks=100,main='Avg lake area in a network')
hist(networks$net_averagelakedistance_km, xlim=c(0, 600), main='Avg dist between lakes in a network')
hist(networks$net_dams_n, xlim=c(0, 2000), breaks=100, main='# dams in a network')

#### try to stack dist #### 

#* create a test dataset for code 
test_dat <- data.frame(
  Lake1 = c("A", "B", "C", "C"),
  Lake2 = c("B", "C", "D", "E"),
  Dist = c(5, 6, 2, 10)
)


test_graph = graph_from_data_frame(test_dat, directed = FALSE)
plot(test_graph, vertex.size=40, edge.width=5*edge.attributes(test_graph)[["Dist"]])
distances(test_graph, weights = test_dat$Dist, mode="out")


# example using Patrick's code
target_network <- 6

targets <- nets[nets$net_id==target_network,]$lagoslakeid
length(targets)

lakenet_targets <- unidirectional[unidirectional$lagoslakeid_1 %in% targets,]

target_graph = graph_from_data_frame(lakenet_targets, directed = FALSE)
target_dist_mat <- distances(target_graph, weights = lakenet_targets$Total_Length_KM, mode="out")


### Exploring metrics for habitat availability #### 
lagos_networks_all <- read.csv("Data/Networks/LAGOSUS_NETSv1.0_MedRes_Metrics_Dams.csv") %>%
  dplyr::select(lagoslakeid, net_id, net_lakes_n, net_averagelakedistance_km, net_averagelakearea_ha)
graph_networks<- read.csv("Data/Networks/lagosnet_metrics.csv")
graph_geo<- read.csv("Data/Networks/lagosnet_geo.csv")

lagos_networks<-lagos_networks_all[!duplicated(paste(lagos_networks_all$net_id)),] 

#remove mississippi 
lagos_net_sub<-filter(lagos_networks, net_id != 1)
graph_net_sub<-filter(graph_networks, net_id !=1) 

#Number of edges in network = number of streams 
hist(graph_net_sub$edges_count)
#number of lakes
hist(lagos_net_sub$net_lakes_n)
plot(log(graph_networks$vertices_count), log(graph_networks$edges_count))

#Avg dist between lakes (within network)
hist(lagos_net_sub$net_averagelakedistance_km)

#distance to lake of similar size class 
#node density within network

#*graph visuals ####
# using igraph package 
#read in bidirectional dist table 
bidir<-read.csv("/Users/katelynking/Desktop/Conny Network/med_res_complete/flowtable and distance/nets__binetworkdistance_medres.csv") %>%
  dplyr::select(binet_lagoslakeid1, binet_lagoslakeid2, binet_streamlength_total_km)
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

#and 264
net_264 <-filter(bidist_net, net_id == "264")%>% dplyr::select(binet_lagoslakeid1, binet_lagoslakeid2, binet_streamlength_total_km) 
net_264<-net_264[!duplicated(paste(net_264$binet_streamlength_total_km)),] 
net_264 = graph_from_data_frame(net_264, directed = FALSE)
plot(net_264, vertex.size=40, vertex.color = "lightblue", edge.color = "darkblue", edge.width=edge.attributes(net_264)[["binet_streamlength_total_km"]])
plot(net_264, vertex.size=40, vertex.color = "lightblue", edge.color = "darkblue")

#* maps #### 
lake_info<-read.csv( "/Users/katelynking/Desktop/Cont Limno/LAGOS_US/lake_information.csv") %>% 
  dplyr::select(lagoslakeid, lake_lat_decdeg, lake_lon_decdeg) #downloaded from dropbox on 4Nov2020 version from 29May2020

networks_ll<-left_join(lagos_networks_all, lake_info, by = "lagoslakeid")
library(mapdata)
usa<-map_data("usa")  #pull out the usa map
p<-ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3) 

#pull out networks with 2 lakes 
two_lake<-filter(networks_ll, net_lakes_n == 2)
two_lake$net_id<-as.factor(two_lake$net_id)
p+geom_point(data=two_lake, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, colour=net_id), show.legend = FALSE)  +
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

#pull out large lake networks to compare with large stream 
two_lake<-filter(networks_ll, net_lakes_n == 2)
two_lake$net_id<-as.factor(two_lake$net_id)
p+geom_point(data=two_lake, size = 1, aes(x = lake_lon_decdeg, y = lake_lat_decdeg, colour=net_id), show.legend = FALSE)  +
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


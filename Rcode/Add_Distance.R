######################## Explore LAGOS Network data ###########################################
#- Date: 05-NOV-2020
# updated: 
# Author: Katelyn King, Patrick Hanley 
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(dplyr)
library(raster)
library(ggplot2)
library(rgdal)
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


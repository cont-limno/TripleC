######################## Explore LAGOS Network data ###########################################
#- Date: 05-NOV-2020
# updated: 
# Author: Katelyn King
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(dplyr)
library(raster)
library(ggplot2)
library(rgdal)
# install.packages("devtools")
devtools::install_github("kylebittinger/usedist")
library(usedist)

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

#join and only keep down dist, the up dist is just mirror image
dist<-left_join(unidirectional, nets, by = c("lagoslakeid_1" = "lagoslakeid")) %>%
            filter(Down_Length_KM > 0)

#create a test dataset for code 
test_dat <- data.frame(
  Lake1 = c("A", "B", "C", "C"),
  Lake2 = c("B", "C", "D", "E"),
  Dist = c(5, 6, 2, 10)
)


data_matrix <- pivot_to_numeric_matrix(
  test_dat, Lake1, Lake2, Dist)



 


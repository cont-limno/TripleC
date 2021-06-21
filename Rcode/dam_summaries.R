######################## Dam data summaries ###########################################
#created: 05-NOV-2020
#updated: 21-JN-2021
# Author: Katelyn King
################################################################################################

#### R libraries ####
library(dplyr)
library(ggplot2)

#### Input data ####
#LAGOS_US_NETWORKS_v1 all lakes (King et al. 2021)
lagos_networks_all <- read.csv("Data/Networks/nets_networkmetrics_medres.csv") 
#just network data
lagos_networks<-lagos_networks_all[!duplicated(paste(lagos_networks_all$net_id)),] %>% 
  dplyr::select(lagoslakeid, net_id, net_lakes_n, net_dams_n)
#hub lakes 
hub_lakes<- read.csv("Data/Networks/VIP_lakes.csv")

################### Main program ######################

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

# summary of hub_char table to see how many hub lakes have dams upstream or downstream or on the lake
hub_char$down_dam<-as.factor(hub_char$down_dam)
hub_char$up_dam<-as.factor(hub_char$up_dam)
summary(hub_char)


#number of dams on the network of the hub lake 
hist(hub_char$net_dams_n) #32 hub lakes have no dams in their entire network (27 unique networks)

hub_nets<-hub_char[!duplicated(paste(hub_char$net_id)),] #hub networks 246 networks with hub lakes 

#### dams on networks with conny scores #### 
nets_with_conn<-filter(lagos_networks, net_lakes_n > 4)
net_with_no_dams<-filter(nets_with_conn, net_dams_n == 0)


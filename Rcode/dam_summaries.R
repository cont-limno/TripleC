######################## Dam data summaries ###########################################
#created: 05-NOV-2020
#updated: 12-FEB-2022
# Author: Katelyn King
################################################################################################

#### R libraries ####
library(dplyr)

#### Input data ####
#LAGOS_US_NETWORKS_v1 all lakes (King et al. 2021)
lagos_networks_all <- read.csv("Data/Networks/nets_networkmetrics_medres_dams.csv") 
#just network data
lagos_networks<-read.csv('Data/Networks/network_scale_metrics.csv') %>% 
  dplyr::select(net_id, net_lakes_n, net_dams_n)
#hub lakes 
hub_lakes<- read.csv("Data/Networks/VIP_lakes.csv")#non MS river hub lakes

################### Main program ######################

#### dams on hub lakes #### 
hub_char<-left_join(hub_lakes, lagos_networks_all)

#new columns for directly up or down dam from a hub lake 
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
hub_char$lake_totalupdowndams <- hub_char$lake_nets_totaldamup_n + hub_char$lake_nets_totaldamdown_n
summary(hub_char) #total dams upstream and downstream and dams on lakes 
hist(hub_char$lake_totalupdowndams, main='Number of dams associated with given hub lake',
     breaks=seq(0,305,5))

#number of dams on the network of the hub lake 
hist(hub_char$net_dams_n) #32 hub lakes have no dams in their entire network (27 unique networks)

hub_nets<-hub_char[!duplicated(paste(hub_char$net_id)),] #hub networks: 246 networks with hub lakes 

#### dams on networks with conny scores #### 
nets_with_conn<-filter(lagos_networks, net_lakes_n > 4)
net_with_no_dams<-filter(nets_with_conn, net_dams_n == 0)


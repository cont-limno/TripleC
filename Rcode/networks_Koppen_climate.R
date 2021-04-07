######################## Climate of LAGOS Networks #############################################
# Date: 1-6-21
# updated: 4-7-21
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####

#### Input data ####
networks <- read.csv("Data/Networks/nets_networkmetrics_medres.csv") #export v1
climate_19812010 <- read.csv("C:/Users/FWL/Documents/ClimateExposure/Data/AdaptWest/AdaptWest_bioclim_1981_2010.csv")
koppen_class <- read.csv("C:/Users/FWL/Documents/ClimateExposure/Data/AdaptWest/all_periods_class_only.csv")
networks_geo <- read.csv("Data/Networks/lagosnet_geo.csv")

######### Main program ###########
# determine Koppen climate class change for each lagoslakeid
koppen_class$Hist <- as.character(koppen_class$Hist)
koppen_class$RCP85_2080s <- as.character(koppen_class$RCP85_2080s)
koppen_class$RCP85_2050s <- as.character(koppen_class$RCP85_2050s)
koppen_class$RCP45_2080s <- as.character(koppen_class$RCP45_2080s)
koppen_class$RCP45_2050s <- as.character(koppen_class$RCP45_2050s)

koppen_class$ChangeRCP85_2050s <- ifelse(koppen_class$Hist==koppen_class$RCP85_2050s, 'Same','Changed')
koppen_class$ChangeRCP85_2080s <- ifelse(koppen_class$Hist==koppen_class$RCP85_2080s, 'Same','Changed')
koppen_class$ChangeRCP45_2050s <- ifelse(koppen_class$Hist==koppen_class$RCP45_2050s, 'Same','Changed')
koppen_class$ChangeRCP45_2080s <- ifelse(koppen_class$Hist==koppen_class$RCP45_2080s, 'Same','Changed')

## desired network/climate outputs
# number of Koppen classes in each network
# proportion of lakes in each network with stable Koppen class
# proportion of lakes in each network with changing Koppen class
# 

netids <- unique(networks$net_id)
network_Koppen_list <- list()

for (i in 1:length(netids)){
  nettump <- subset(networks, net_id==netids[i])
  nettump_lagoslakeids <- nettump$lagoslakeid
  nettump_koppen <- subset(koppen_class, lagoslakeid %in% nettump_lagoslakeids)
  nettump_output <- data.frame(matrix(NA, nrow = 1, ncol = 1))
  nettump_output[1,1] <- netids[i]
  colnames(nettump_output) <- 'net_id' #make first column the network ID
  # add number of lakes column
  nettump_output$nLakes <- length(nettump_lagoslakeids)
  # count unique Koppen classes in each network
  nettump_output$hist_unique <- length(unique(nettump_koppen$Hist))
  nettump_output$RCP45_2050s_unique <- length(unique(nettump_koppen$RCP45_2050s))
  nettump_output$RCP45_2080s_unique <- length(unique(nettump_koppen$RCP45_2080s))
  nettump_output$RCP85_2050s_unique <- length(unique(nettump_koppen$RCP85_2050s))
  nettump_output$RCP85_2080s_unique <- length(unique(nettump_koppen$RCP85_2080s))
  # count number of lakes with changed or same Koppen class from historical to future periods
  nettump_output$RCP45_2050s_changed <- summary(as.factor(nettump_koppen$ChangeRCP45_2050s))[1]
  nettump_output$RCP45_2050s_same <- summary(as.factor(nettump_koppen$ChangeRCP45_2050s))[2]
  nettump_output$RCP45_2080s_changed <- summary(as.factor(nettump_koppen$ChangeRCP45_2080s))[1]
  nettump_output$RCP45_2080s_same <- summary(as.factor(nettump_koppen$ChangeRCP45_2080s))[2]
  nettump_output$RCP85_2050s_changed <- summary(as.factor(nettump_koppen$ChangeRCP85_2050s))[1]
  nettump_output$RCP85_2050s_same <- summary(as.factor(nettump_koppen$ChangeRCP85_2050s))[2]
  nettump_output$RCP85_2080s_changed <- summary(as.factor(nettump_koppen$ChangeRCP85_2080s))[1]
  nettump_output$RCP85_2080s_same <- summary(as.factor(nettump_koppen$ChangeRCP85_2080s))[2]
  # calculate proportion of lakes with changed or same Koppen class from historical to future periods
  nettump_output$RCP45_2050s_changed_pct <- nettump_output$RCP45_2050s_changed/length(nettump_lagoslakeids)
  nettump_output$RCP45_2050s_same_pct <- nettump_output$RCP45_2050s_same/length(nettump_lagoslakeids)
  nettump_output$RCP45_2080s_changed_pct <- nettump_output$RCP45_2080s_changed/length(nettump_lagoslakeids)
  nettump_output$RCP45_2080s_same_pct <- nettump_output$RCP45_2080s_same/length(nettump_lagoslakeids)
  nettump_output$RCP85_2050s_changed_pct <- nettump_output$RCP85_2050s_changed/length(nettump_lagoslakeids)
  nettump_output$RCP85_2050s_same_pct <- nettump_output$RCP85_2050s_same/length(nettump_lagoslakeids)
  nettump_output$RCP85_2080s_changed_pct <- nettump_output$RCP85_2080s_changed/length(nettump_lagoslakeids)
  nettump_output$RCP85_2080s_same_pct <- nettump_output$RCP85_2080s_same/length(nettump_lagoslakeids)
  # store output in list created earlier
  network_Koppen_list[[i]] <- nettump_output
  nettump_output <- NA
}

# merge all loop output in list into new data frame
network_Koppen_summary <- do.call(rbind.data.frame, network_Koppen_list)
# get rid of NAs (represent true Os)
network_Koppen_summary[is.na(network_Koppen_summary)] <- 0

## summary statz and figurez!

# histograms of proportion of lakes in network that shift Koppen class
par(mfrow=c(2,2))
hist(network_Koppen_summary$RCP45_2050s_changed_pct, main='RCP 4.5, 2050s', xlab='Proportion of lakes in network that shift Koppen class', ylim=c(0,900))
hist(network_Koppen_summary$RCP45_2080s_changed_pct, main='RCP 4.5, 2080s', xlab='Proportion of lakes in network that shift Koppen class', ylim=c(0,900))
hist(network_Koppen_summary$RCP85_2050s_changed_pct, main='RCP 8.5, 2050s', xlab='Proportion of lakes in network that shift Koppen class', ylim=c(0,900))
hist(network_Koppen_summary$RCP85_2080s_changed_pct, main='RCP 8.5, 2080s', xlab='Proportion of lakes in network that shift Koppen class', ylim=c(0,900))

par(mfrow=c(1,1))
plot(network_Koppen_summary$nLakes ~ network_Koppen_summary$RCP45_2050s_changed_pct)

# Koppen class diversity by network
par(mfrow=c(2,3))
hist(network_Koppen_summary$RCP45_2050s_unique, main='RCP 4.5, 2050s', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))
hist(network_Koppen_summary$RCP45_2080s_unique, main='RCP 4.5, 2080s', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))
hist(network_Koppen_summary$hist_unique, main='Historical (1981-2010)', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))
hist(network_Koppen_summary$RCP85_2050s_unique, main='RCP 8.5, 2050s', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))
hist(network_Koppen_summary$RCP85_2080s_unique, main='RCP 8.5, 2080s', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))

# change in number of Koppen classes from historical to future periods
network_Koppen_summary$uniq_loss_RCP85_2080s <- network_Koppen_summary$hist_unique - network_Koppen_summary$RCP85_2080s_unique
network_Koppen_summary$uniq_loss_RCP45_2080s <- network_Koppen_summary$hist_unique - network_Koppen_summary$RCP45_2080s_unique
network_Koppen_summary$uniq_loss_RCP45_2050s <- network_Koppen_summary$hist_unique - network_Koppen_summary$RCP45_2050s_unique
network_Koppen_summary$uniq_loss_RCP85_2050s <- network_Koppen_summary$hist_unique - network_Koppen_summary$RCP85_2050s_unique

hist(network_Koppen_summary$uniq_loss_RCP45_2050s, main='Change in number of Koppen climates', xlab='RCP 4.5, 2050s', xlim=c(-5,5), ylim=c(0,800), breaks=seq(-5,5,1))
hist(network_Koppen_summary$uniq_loss_RCP45_2080s, main='Change in number of Koppen climates', xlab='RCP 4.5, 2080s', xlim=c(-5,5), ylim=c(0,800), breaks=seq(-5,5,1))
hist(network_Koppen_summary$uniq_loss_RCP85_2050s, main='Change in number of Koppen climates', xlab='RCP 8.5, 2050s', xlim=c(-5,5), ylim=c(0,800), breaks=seq(-5,5,1))
hist(network_Koppen_summary$uniq_loss_RCP85_2080s, main='Change in number of Koppen climates', xlab='RCP 8.5, 2080s', xlim=c(-5,5), ylim=c(0,800), breaks=seq(-5,5,1))

# What if remove MS river network?
network_Koppen_summary_noMS <- subset(network_Koppen_summary, net_id > 1)

par(mfrow=c(2,3))
hist(network_Koppen_summary_noMS$RCP45_2050s_unique, main='RCP 4.5, 2050s', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))
hist(network_Koppen_summary_noMS$RCP45_2080s_unique, main='RCP 4.5, 2080s', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))
hist(network_Koppen_summary_noMS$hist_unique, main='Historical (1981-2010)', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))
hist(network_Koppen_summary_noMS$RCP85_2050s_unique, main='RCP 8.5, 2050s', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))
hist(network_Koppen_summary_noMS$RCP85_2080s_unique, main='RCP 8.5, 2080s', xlab='Unique Koppen classes', ylim=c(0,900), xlim=c(0,15))

par(mfrow=c(2,2))
plot(network_Koppen_summary_noMS$nLakes ~ network_Koppen_summary_noMS$RCP45_2050s_changed_pct, main='RCP 4.5, 2050s', ylab='n lakes', xlab='Proportion of lakes in network that shift Koppen class')
plot(network_Koppen_summary_noMS$nLakes ~ network_Koppen_summary_noMS$RCP45_2080s_changed_pct, main='RCP 4.5, 2080s', ylab='n lakes', xlab='Proportion of lakes in network that shift Koppen class')
plot(network_Koppen_summary_noMS$nLakes ~ network_Koppen_summary_noMS$RCP85_2050s_changed_pct, main='RCP 8.5, 2050s', ylab='n lakes', xlab='Proportion of lakes in network that shift Koppen class')
plot(network_Koppen_summary_noMS$nLakes ~ network_Koppen_summary_noMS$RCP85_2080s_changed_pct, main='RCP 8.5, 2080s', ylab='n lakes', xlab='Proportion of lakes in network that shift Koppen class')

## latitudinal breadth of networks
summary(networks_geo$min_cut_lat)
summary(networks_geo$maxlat_dist)

par(mfrow=c(1,1))
hist(networks_geo$min_cut_lat, xlab='min_cut_lat', main='Min # of cuts to disconnect max latitudinal breadth of network')

networks_geo_min10_cut_lat <- subset(networks_geo, min_cut_lat <= 10)

hist(networks_geo_min10_cut_lat$min_cut_lat, xlab='min_cut_lat', main='Min # of cuts to disconnect max latitudinal breadth of network')
mtext(side=3, 'min_cut_lat <=10; 95% of networks')
summary(networks_geo_min10_cut_lat$min_cut_lat)

hist(networks_geo$maxlat_dist, xlab='maxlat_dist', main='Maximum latitudinal breadth of networks')

#### Apr 2021 update ####
network_climate_list <- list()

for (i in 1:length(netids)){
  nettump <- subset(networks, net_id==netids[i])
  nettump_lagoslakeids <- nettump$lagoslakeid
  nettump_climate <- subset(climate_19812010, lagoslakeid %in% nettump_lagoslakeids)
  nettump_output <- data.frame(matrix(NA, nrow = 1, ncol = 1))
  nettump_output[1,1] <- netids[i]
  colnames(nettump_output) <- 'net_id' #make first column the network ID
  # add number of lakes column
  nettump_output$nLakes <- length(nettump_lagoslakeids)
  # get climate stats for desired variables for all lakes within network
  nettump_output$MAT_min <- min(nettump_climate$MAT, na.rm=T)
  nettump_output$MAT_max <- max(nettump_climate$MAT, na.rm=T)
  nettump_output$MAT_median <- median(nettump_climate$MAT, na.rm=T)
  nettump_output$MAT_range <- nettump_output$MAT_max - nettump_output$MAT_min
  nettump_output$MAT_sd <- sd(nettump_climate$MAT, na.rm=T)
  # store output in list created earlier
  network_climate_list[[i]] <- nettump_output
  nettump_output <- NA
}

# merge all loop output in list into new data frame
network_climate_summary <- do.call(rbind.data.frame, network_climate_list)
# get rid of NAs (represent true Os)
network_climate_summary[is.na(network_climate_summary)] <- 0
#write.csv(network_climate_summary, "Data/Networks/networks_MAT_summary.csv")

## exploratory plots
summary(network_climate_summary$MAT_range)
hist(network_climate_summary$MAT_range)
summary(network_climate_summary$MAT_sd)
hist(network_climate_summary$MAT_sd)

plot(network_climate_summary$nLakes ~ network_climate_summary$MAT_range, pch=20)
plot(network_climate_summary$nLakes ~ network_climate_summary$MAT_range, pch=20, ylim=c(0,5000))

plot(network_climate_summary$nLakes ~ network_climate_summary$MAT_sd, pch=20)
plot(network_climate_summary$nLakes ~ network_climate_summary$MAT_sd, pch=20, ylim=c(0,5000))

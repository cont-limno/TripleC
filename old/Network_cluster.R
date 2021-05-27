# rm(list=ls())
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

#### Input data ####
# locus <- read.csv('../Data/locus/lake_information_20200520.csv', stringsAsFactors = FALSE, na.strings= c("NA", "NULL", "")) 
# names(locus)
# dim(locus)

# Lake characteristics
# char <- read.csv('../Data/locus/lake_characteristics.csv', stringsAsFactors = FALSE, na.strings= c("NA", "NULL", ""))
# head(char)
# dim(char)

# network data
nets <- read.csv('../Data/Networks/network_scale_metrics.csv') 
head(nets)
str(nets)

# Select network variables for clustering
dat <- nets %>% 
  dplyr::select(net_id, edge_dens, artic_count, min_cut_lat, maxkmNS, net_lakes_n, net_averagelakedistance_km,
                net_rangeorder)
head(dat)

# Remove 1 NA value
dat <- dat %>% filter(!is.na(maxkmNS))

# Add row names
row.names(dat) <- dat$net_id
dim(dat)
summary(dat)
# Remove net_id
dat <- dat[,-1]

# Remove networks with < 4 lakes
dat <- dat %>% 
  filter(net_lakes_n > 4)

# Standardize variables
clus_dat <- scale(dat)
# Correlations
cor(clus_dat)


########### Hierarchical clustering
hc <- eclust(clus_dat, "hclust") 
str(hc)
# dendrogam
fviz_dend(hc, rect = TRUE) 
# scatter plot
fviz_cluster(hc) 

# Size of clusters
hc$size

dat %>%
  mutate(cluster = hc$cluster) %>%
  group_by(cluster) %>%
  summarise_all("median")


######################## LAGOSUS Protected lakes ###############################################
# Date: 11-25-20
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

setwd("C:/Users/FWL/Documents/TripleC")

#### R libraries ####
library(raster)

#### Input data ####
# PADUS v 1.4 (2.0 exists but using 1.4 because that is in LAGOS)
# US Geological Survey (USGS) Gap Analysis Program (GAP), 20160505, Protected Areas Database of the United States (PAD-US): 
# USGS Gap Analysis Program (GAP), https://doi.org/10.5066/F7G73BSZ. 
ws_PADUS <- read.csv("Data/PADUS14/ws_gap_20201007.csv") 

# LAGOS-US lake points (v 0.5) selected by location in ArcGIS within protected areas by GAP status
GAP12_lake_pts <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP12_lake_pts.shp")
GAP123_lake_pts <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP123_lake_pts.shp")
GAP1234_lake_pts <- shapefile("C:/Ian_GIS/TripleC_GIS/ProtectedLakes/GAP1234_lake_pts.shp")

################### Main program #################
# calculate cumulative protection by GAP status
ws_PADUS$GAP12_pct <- ws_PADUS$ws_gap_status1_pct + ws_PADUS$ws_gap_status2_pct
ws_PADUS$GAP123_pct <- ws_PADUS$ws_gap_status1_pct + ws_PADUS$ws_gap_status2_pct + ws_PADUS$ws_gap_status3_pct
ws_PADUS$GAP1234_pct <- ws_PADUS$ws_gap_status1_pct + ws_PADUS$ws_gap_status2_pct + ws_PADUS$ws_gap_status3_pct + ws_PADUS$ws_gap_status4_pct

summary(ws_PADUS)
hist(ws_PADUS$ws_gap_status1_pct)
hist(ws_PADUS$ws_gap_status2_pct)
hist(ws_PADUS$ws_gap_status3_pct)
hist(ws_PADUS$ws_gap_status4_pct)
hist(ws_PADUS$GAP12_pct)
hist(ws_PADUS$GAP123_pct)
hist(ws_PADUS$GAP1234_pct)

# get lagoslakeids of lakes that fall within protected areas by GAP status
GAP12_lagoslakeids <- GAP12_lake_pts@data$lagoslakei
GAP123_lagoslakeids <- GAP123_lake_pts@data$lagoslakei
GAP1234_lagoslakeids <- GAP1234_lake_pts@data$lagoslakei

## calculate % of US lakes protected
# protection based on being within a protected area (lake center)
length(GAP12_lagoslakeids)/479950
length(GAP123_lagoslakeids)/479950
length(GAP1234_lagoslakeids)/479950

# protection based on at least 80% of watershed protected
nrow(subset(ws_PADUS, GAP12_pct >= 80))/479950
nrow(subset(ws_PADUS, GAP123_pct >= 80))/479950
nrow(subset(ws_PADUS, GAP1234_pct >= 80))/479950

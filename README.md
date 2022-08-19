# TripleC
Connectivity, climate and conservation

This repository contains data and code associated with the manuscript Freshwater corridor in the conterminous US: a coarse-filter approach based on lake-stream networks, which was accepted in the open-access journal Ecosphere at the time of publishing this repository. This readme is an overview of the contents of this repository. Subdirectories contain additional readme and metadata files where relevant. 

# Data
lower48: polygon shapefile of lower 48 US states (conterminous US)

Networks: input and output tables related to freshwater networks, GIS point shapefiles of network lakes

PADUS14: watershed lake protection from US Protected Areas Database v 2.0 (via LAGOS-US-GEO)

LAGOSUS_RSVR_v1.1_classes.csv: manual and residual neural network classifications of LAGOS lakes >= 4 hectares

Metadata file for all data tables

# Figures
Folder for figures in the manuscript or exploratory figures

# MS_materials
Folder for manuscript documents, including submitted and accepted documents. May have slight differences from final publication.

# Rcode
dam_summaries.R: summaries of dams on hub lakes and within networks 

LakeNetworkProtectionAnalysis.R: analysis of protection of lakes and networks in the conterminous US, freshwater connectivity and hub lakes

LakeProtection.R: statistics on lake protection in the conterminous US

Network_conn_scores.R: principal components analysis used to calculate network connectivity scores

Networks_analyze_ecoregion.R: network statistics by National Aquatic Resource Survey ecoregions 

Networks_assign_ecoregion.R: assign networks to National Aquatic Resource Survey ecoregions 

Scripts that use GIS files reference directories on our system. You can find zipped GIS files in Data and adjust scripts accordingly.




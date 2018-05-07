# calculate_gridmet_ecoregion_means.R

#-----------------------------Description---------------------------------------
# This script will be used to create handy arrays of mean and summed met 
# variables in ecoregions. 
library(sp)
library(maps)
library(ncdf4)

# Handle data and links
nc_data_dir <- "/Volumes/Brey_external/gridMET/"

# Get the ecoregions
load("Data/GIS/na_cec_eco_level_2.RData")
crs <- SPDF@proj4string

################################################################################
# First, if an ecoregion mask file does not exist, create one. We need to know
# what ecoregion each gridpoint in the gridmet data is most closely related to
# spatially.


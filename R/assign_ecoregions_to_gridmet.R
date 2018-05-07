#!/usr/bin/env Rscript

# assign_ecoregions_to_gridmet.R

#-----------------------------Description---------------------------------------
# This script assigns level 2 ecoregions to gridmet met data fields. There are
# a lot of calculations happenning here, so it takes forever..
# TODO: optimize forloop. 

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
nc_file <- paste0(nc_data_dir, "tmmn_2015.nc")
nc <- nc_open(nc_file)
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
tmmn <- ncvar_get(nc, "air_temperature") # [lat, lon, time]
nc_close(nc)

#quartz()

# Create an array to store the ecoregion name for each point in the grid
ecoregion_grid <- tmmn[,,1] # only need the spatial dimensions 
ecoregion_grid[] <- 0 # make all the values zero so they are easy to replace! 

# loop through each point and make an assignment
nLat <- length(lat)
nLon <- length(lon)
nCounts <- nLon * nLat
count <- 0

for (i in 1:nLat){ # loop starts in upper left of CONUS 
  for (j in 1:nLon){
    
    y <- lat[i] # lat proxy
    x <- lon[j] # lon proxy
    
    p <- sp::SpatialPoints(cbind(x,y), proj4string = crs)
    
    # Overlap calculation
    mask <- sp::over(p, SPDF)
    ecoregion_grid[i,j] <- as.numeric(as.character(mask$NA_L2CODE))
    
    count <- count + 1 
    if(count%%1000==0){
      print(paste(count/nCounts*100, "% complete"))
    }
    
  }
}

save(ecoregion_grid, file="ecoregion_grid_west.RData")
print("script_complete")
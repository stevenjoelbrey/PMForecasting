# calculate_gridmet_ecoregion_means.R

#-----------------------------Description---------------------------------------
# This script will be used to create handy arrays of mean and summed met 
# variables in ecoregions. The default environmental predictors are:
#   tmmx
#   rmin
#   pr

year1 <- 2018 # Can go to 1992 but only have PM data starting in 1999
year2 <- 2018
months_select <- c(2,3,4)
ecoregion_select <- 6.2

years <- year1:year2
nYears <- length(years)

library(sp)
library(maps)
library(ncdf4)
library(lubridate)

# Handle data and links
nc_data_dir <- "/Volumes/Brey_external/gridMET/"
grid_file <- function(nc_data_dir, VAR, YEAR){
  f <- paste0(nc_data_dir, VAR, "_", YEAR, ".nc")
  return(f)
}

# Get the ecoregions
load("Data/GIS/na_cec_eco_level_2.RData")
eco_crs <- SPDF@proj4string

################################################################################
# First, load the ecoregion mask created by R/assign_ecoregions_to_gridmet.R
if(FALSE){
  
  gridmet_ecoregions <- get(load("Data/GIS/gridmet_ecoregions.RData"))
  gridmet_ecoregions[is.na(gridmet_ecoregions)] <- 0
  ecoregion_mask <- gridmet_ecoregions == ecoregion_select
    
}else{

  # For while it does not exist 
  gridmet_ecoregions <- get(load("Data/GIS/ecoregion_grid_west.RData"))
  gridmet_ecoregions[is.na(gridmet_ecoregions)] <- 0
  ecoregion_mask <- gridmet_ecoregions == ecoregion_select
  
}


# Loop through each year of desired data and take the relevent spatial and 
# temporal mean for each desired met variable. Save these combined information
# in a simple R dataframe. 

# What days of the year do you need to load? Sets what days in yearly nc files
# will be loaded into the workspace. 
DOY <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) # array that shows the Jday of the first day of each month (non-leap years)
day1 <- DOY[min(months_select)]
day2 <- DOY[max(months_select+1)] - 1 # plus one gets the next month, -1 gives DOY of last day of that month

# nc does not want an index it wants a count! 
deltaDay <- length(day1:day2) 

# Create the dataframe where the summed gridmet variables will live. 
blank <- rep(NA, nYears)
tmmx_mean <- blank
rmin_mean <- blank
pr_sum <- blank
df <- data.frame(tmmx_mean=tmmx_mean, rmin_mean=rmin_mean, pr_sum=pr_sum,
                 row.names = years)

# Create means and sums for each year
for (i in 1:nYears){
  
  print(paste0("Getting data for year:", years[i]))
  
  # ----------------------------------------------------------------------------
  # Handle tmmx [585 1386  366] ------------------------------------------------
  f  <- grid_file(nc_data_dir, "tmmx", years[i])
  nc <- nc_open(f)
  lon <- ncvar_get(nc, varid="lon"); nLon <- length(lon)
  lat <- ncvar_get(nc, varid="lat"); nLat <- length(lat)
  tmmx  <- ncvar_get(nc, varid="air_temperature", 
                  start=c(1,1, day1), count = c(nLat, nLon, deltaDay) )
  # provide additional temporal sanity checks for the first var loaded 
  delta_seconds <- ncvar_get(nc, varid="day", start=day1, count=deltaDay) * 24*60^2
  nc_close(nc)
  t <- as.POSIXct(delta_seconds, origin="1900-01-01 00:00:00", tz="UTC")
  print(paste("Predictor days for year:", t[1], "-",t[length(t)]))
  # Take the mean of the time dimension (first and second)
  tmmx_t_mean <- apply(tmmx, 1:2, mean)
  # Take the mean of the ecoregion of interest and store it
  df$tmmx_mean[i] <- mean(tmmx_t_mean[ecoregion_mask], na.rm = T) # NA are edges of grid where not land
  
  # ----------------------------------------------------------------------------
  # Handle rmin [585 1386  366] ------------------------------------------------
  f  <- grid_file(nc_data_dir, "rmin", years[i])
  nc <- nc_open(f)
  rmin  <- ncvar_get(nc, varid="relative_humidity", 
                     start=c(1, 1, day1), count = c(nLat, nLon, deltaDay) )
  nc_close(nc)
  rmin_t_mean <- apply(rmin, 1:2, mean)
  df$rmin_mean[i] <- mean(rmin_t_mean[ecoregion_mask], na.rm = T) 
  
  # ----------------------------------------------------------------------------
  # Handle rmin [585 1386  366] ------------------------------------------------
  f  <- grid_file(nc_data_dir, "pr", years[i])
  nc <- nc_open(f)
  pr  <- ncvar_get(nc, varid="precipitation_amount", ## mm
                     start=c(1, 1, day1), count = c(nLat, nLon, deltaDay) )
  nc_close(nc)
  pr_t_sum <- apply(pr, 1:2, sum)
  df$pr_sum[i] <- mean(pr_t_sum[ecoregion_mask], na.rm = T) 
  
  
}

df$year <- years

# ------------------------ Save the data----------------------------------------
saveName <- paste0("Data/gridmet_summary_",year1,"-",year2,"_", min(months_select), "-", 
                   max(months_select), "_eco=", ecoregion_select, ".RData")
assign("gridmet_summary", df)

# Save these data for easy loading later
save(gridmet_summary, file=saveName)




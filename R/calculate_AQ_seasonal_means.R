# calculate_seasonal_means.R

# Subset the data to the months of interest for a given year. After that, figure
# out what unique monitors live in the desired ecoregions.

year1 <- 1999 # year one to train the model
year2 <- 2017 # last year to train the model
years <- year1:year2
nYears <- length(years)

month1 <- 6 # forecasted months
month2 <- 8
ecoregion_select <- 6.2 # forested mountains
statesToKeep <- c("Washington", "California","Colorado","Idaho", "Montana",
                  "Nevada", "New Mexico", "Oregon", "Utah", "Wyoming")# states that intersect forested mountains


library(stringr)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(grDevices)

# Load ecoregion map ahead of time and subset it by the selected ecoregion of
# interest
load("Data/GIS/na_cec_eco_level_2.RData")
m <- ecoregion_select == SPDF@data$NA_L2CODE
SPDF <- SPDF[m,]

# Create arrays to store summary information
yearMean           <- rep(NA, nYears)

for (i in 1:nYears){
  
  y <- years[i]
  
  print(paste("Working on processing", y, 
              "monitor data in ecoregion", ecoregion_select))
  
  # Load the first year of monitoring data  
  f <- paste0("Data/PM25_FRM_FEM/daily_88101_", y, ".csv")
  # load yearly data dataframe
  df_y <- read.csv(f, stringsAsFactors=F)
  
  # Get dates
  Date.Local.y <- as.POSIXlt(df_y$Date.Local, tz="UTC")
  df_y$Date.Local <- Date.Local.y # so it is nicely formatted
  monthMask    <- (month(Date.Local.y) >= month1) & (month(Date.Local.y) <= month2)
  
  # Subset dataframe to year (y) summer (s)
  df_ys <- df_y[monthMask, ]
  rm(df_y) # save working memory
  
  # Now we want to spatially subset these data, as they are for all US states
  # and territories. First eliminate all data that are not located within a 
  # state where ecoregion 6.2 intersects
  stateMask <- df_ys$State.Name %in% statesToKeep
  df_ys     <- df_ys[stateMask,]
  
  # Perform the overlap calculations
  coords <- cbind(df_ys$Longitude, df_ys$Latitude)
  SP <- SpatialPoints(coords, proj4string=SPDF@proj4string)
  print(paste("Working on overlap calculation..."))
  overlaps_df <- sp::over(SP, SPDF)
  monitorsToKeep <- !(is.na(overlaps_df$NA_L2CODE))
  
  # dataframe year(y) season(s) ecoregion(e)
  df_yse <- df_ys[monitorsToKeep, ]
  
  # Store these data as an appended dataframe
  if(i == 1){
    df_all <- df_yse
  } else{
    df_all <- rbind(df_all, df_yse)
  }
  
  # Get the yearly mean value
  yearMean[i] <- mean(df_yse$Arithmetic.Mean, na.rm=T)
  
}

# ------------------------ Save the data----------------------------------------
# Save dataframe the means, this is what we will use to make the model 
saveName <- paste0("Data/PM25_summary_", month1, "-", month2, 
                   "_eco=", ecoregion_select, ".RData")
# Create a dataframe so that data is stored in a more descriptive way
PM_summary <- data.frame(year=years, PM25_mean=yearMean)

save(PM_summary, file=saveName)

# ------------------------ plot simple map using base --------------------------

png(filename="Figures/monitor_map.png", width=1000, height=700, res=200)
map("state", xlim=c(-125,-100))
plot(SPDF, add=T, col="forestgreen")
points(df_all$Longitude, df_all$Latitude, pch=19, col="black", cex=0.5)
dev.off()

# ------------------------ plot the data with ggplot2() ------------------------
# Plot box plots of PM in region vs. year

# Make a year factor for all rows for easy ggplot2() plotting
df_all$year <- factor(year(df_all$Date.Local))


g <- ggplot(df_all, aes(year, Arithmetic.Mean))+
  geom_boxplot(notch=T)+
  theme_tufte(ticks=T, base_size = 20)+
  labs(x="Year (June, July, August",
       y=expression(paste(PM["2.5"], " [",mu, gm^-3, "]"))
       )


saveName <- paste0("Figures/annual_PM_boxplots.png")
png(filename=saveName, width=2800, height=1000, res=250)
print(g)
dev.off()

# Now make a cleaner version where no outliers are shown
p <- ggplot(df_all, aes(year, Arithmetic.Mean))+
  geom_boxplot(notch=T, outlier.shape=NA)+
  ylim(c(0, 25))+
  theme_tufte(ticks=T, base_size = 22)+
  labs(x="Year (June, July, August",
       y=expression(paste(PM["2.5"], " [",mu, gm^-3, "]"))
  )

saveName <- paste0("Figures/annual_PM_boxplots_no_outliers.png")
png(filename=saveName, width=2800, height=1000, res=250)
print(p)
dev.off()

# Plot the region, show the world what area you are talking about. 
png(filename=paste0("Figures/forested_mountains_map.png"), 
    width=1500, height=1500, res=250)
map("state", xlim=c(-125, -100))
plot(SPDF, add=T, col="forestgreen")
map("state", xlim=c(-125, -100), add=T)
dev.off()

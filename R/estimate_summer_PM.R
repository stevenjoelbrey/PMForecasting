# estimate_summer_PM.R

# Load this springs current predictors 
thisSpring <- get(load("Data/gridmet_summary_2018-2018_2-4_eco=6.2.RData"))

# Load the PM and met data and make them cover the same years (subset met)
load("Data/gridmet_summary_2-4_eco=6.2.RData")
load("Data/PM25_summary_6-8_eco=6.2.RData")
year <- PM_summary$year

# The variable to predict
PM <- PM_summary$PM25_mean

# The predictors 
tmmx <- gridmet_summary$tmmx_mean
rh   <- gridmet_summary$rmin_mean
pr   <- gridmet_summary$pr_sum

# Create linear model
lmfit <- lm(PM ~ tmmx + rh + pr)

# Estimate this summers PM value
coefs <- as.numeric(lmfit$coefficients)
thisSummerPM <- coefs[1] + coefs[2]*thisSpring[1,1] + coefs[3]*thisSpring[1,2] + 
                coefs[4]*thisSpring[1,3] 

# Plot the results
png(filename="Figures/forecasted_PM.png", width=3000, height=1500, res=200)
# TODO: Show standard deviation of seasonal PM mean
par(mar=c(4,7,2,1))

plot(year, PM, pch=19, las=1, 
     ylab=expression("Mean "*"PM"[2.5]*"ugm"^3),
     xlab="Summer (June - August)", cex.lab=2,
     xlim=c(min(year), max(year)+1)
     )
lines(year, fitted(lmfit), col="blue", lwd=3)
points(max(year)+1, thisSummerPM, pch=19, col="red", cex=2)
legend("top", 
       legend=paste("r-squared =", round(summary(lmfit)$r.squared,2)),
       cex=2,
       bty="n"
       )
legend("topleft", 
       legend=c("Measured", expression("Linear Model R"^2), "Predicted"),
       fill=c("black", "blue", "red"),
       cex=2,
       bty="n"
)
title(expression("Forested mountains seasonal PM"[2.5]*" & linear model estimate"),
      cex.main=2)
dev.off()

# Author: Amalia Castro
# Date: 11-11-2013
# Dependencies: library(rasta)
# Description: samples a temperature raster. Then it analyses statistically the values and plots them

library(rasta)

# Create a temperature raster and sample it
filepath <- system.file("extdata", "anom.2000.03.tiff", package ="rasta")
rastertemp <- raster(filepath)
samplepoints <- runifpoint(30, win = as.vector(extent(rastertemp)))  # generate 30 random samples with coordinates
coord <- data.frame(x= samplepoints$x, y=samplepoints$y)  # coordinates of the random points as a spatial data frame 

# analyse and plot sample temperature values
sampleTemp <- extract(rastertemp, coord)  # retrieve the temperature value of the raster for each sample location
meanTemps <- mean(sampleTemp)
stdevTemps <- sd(sampleTemp)

plotRaster <- plot(rastertemp, main = 'Temperature values and samples',col.main="red", sub = "Amalia Castro", col.sub="blue", xlab = "longitude", ylab = "latitude")
plotSamples <- plot(samplepoints, add =TRUE)
mtext(c("mean:-20.5","stdev: 44.89"), side=1, line=2, at=c(-20,20))



# Author: Amalia Castro
# Date: November 2013
# Dependencies: rasta, raster, rgdal, sp, ggplot2, rasterVis, RColorBrewer packages
# Description:
  
library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)

# Download the data
data(tura)

# Correct reflectance values
tura <- tura/10000

### Extract the date from the names of tura and 
# convert it to a date with a certan format:
dates <- substr(names(tura), 10, 16)
dates <- as.Date(dates, format = "%Y%j")
# print(as.Date(dates, format="%Y%j"))

### Extract layer names from the tura rasterBrick and
# analyse them to extract scene information
sceneID <- names(tura)
sceneinfo <- getSceneinfo(sceneID)


### Create a column in the sceneinfo data.frame to contain the year
sceneinfo$year <- factor(substr(sceneinfo$date, 1, 4), levels = c(1984:2013))


### Select layers of years 2000, 2005 and 2010 
lay2000 <- sceneinfo$year == 2000
a <- which(lay2000)  # info on the number of those layers 
year2000 <- subset(tura, a)  # extracts them (as a rasterBrick object)

lay2005 <- sceneinfo$year == 2005
b <- which(lay2005)
year2005 <- subset(tura, b)

lay2010 <- sceneinfo$year == 2010
c <- which(lay2010)
year2010 <- subset(tura, c)


### Calculate pixel-based mean of the layers
raster2000 <- calc(year2000, fun=mean, na.rm=TRUE)
raster2005 <- calc(year2005, fun=mean, na.rm=TRUE)
raster2010 <- calc(year2010, fun=mean, na.rm=TRUE)


### Create rasterBrick
rasterall <- brick(raster2000, raster2005, raster2010)


### Give a common scale to the 3 layers and plot together
cols <- brewer.pal(11, 'PiYG')  # find a proper color scale, not this one
rtheme <- rasterTheme(region=cols)
levelplot(rasterall[[1:3]], names.attr=sceneinfo$date[1:3], par.settings=rtheme)


### Produce RGB Composite
plotRGB(rasterall, 1,2,3, stretch='hist')


### Verify change regions by extracting time series vector drawing polygon objects:

# (1) Between 2000(red) and 2005(green): high NDVI in 2000 = bright red 
# e1 <- drawExtent()
# print(e1) 
e1 <- extent(c(823037.2, 823127.2, 830240.4, 830442.7))

# (2) Between 2005(green) and 2010(blue): high NDVI in 2010 = bright blue 
# e2 <- drawExtent()
# print(e2)
e2 <- extent(c(822587.6, 822789.9, 831982.7, 832128.8))

# Extract the mean NDVI for each layer of the tura brick, 
# within the selected extent
mean_e1 <- extract(tura, e1, fun = mean, na.rm=TRUE)
mean_e2 <- extract(tura, e2, fun = mean, na.rm=TRUE)


### Add the values to a new NDVI column of sceneinfo data.frame
sceneinfo$NDVI1 <- mean_e1
sceneinfo$NDVI2 <- mean_e2


### Plot the time series mean values of all the layers 
# in the two extents
ggplot(data = sceneinfo, aes(x = date, y = NDVI1)) +
  geom_point(size = 2) +
  labs(y = "% nodata") +
  theme_bw()

ggplot(data = sceneinfo, aes(x = date, y = NDVI2)) +
  geom_point(size = 2) +
  labs(y = "% nodata") +
  theme_bw()






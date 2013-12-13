# Author: Amalia Castro
# Date: December 2013
# Dependencies: sp,raster, rgdal,rasta, ggplot2, rasterVis, RColorBrewer, reshape
# Description: extracts and characterizes the Landsat time series 

## Note: the input is a .tiff file (not .grd) 
# so we read the acquisition dates from dates.csv
## Note: Plotting the rBrick layers with levelplot sometimes 
# causes the R session to crash. It could be related to small 
# space available in the plotting window.

library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)
library(reshape)

# Set working directory
setwd=getwd()

# Clean workspace
rm(list = ls())

## Loading data and preparing data.frame of acquisiton dates
# Download Landsat raster brick and acquisition dates file from Dropbox and load them

dl_from_dropbox <- function(x, key) {
  require(RCurl)
  bin <- getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
                      ssl.verifypeer = FALSE)
  con <- file(x, open = "wb")
  writeBin(bin, con)
  close(con)
  message(noquote(paste(x, "read into", getwd())))                        
}

# Download raster brick
dl_from_dropbox("NDVI_Landsat_Amalia_scene222062_sub.tif", "6sgo922h2fylj2f")
rBrick <- brick("NDVI_Landsat_Amalia_scene222062_sub.tif")
# The projection needs to longlat for some analyses 
rBrickLL <- projectRaster(rBrick, crs='+proj=longlat')

# Download acquisition dates
dl_from_dropbox("dates.csv", "i510xvtyaany813")
df_dates <- read.csv("dates.csv", header =TRUE, stringsAsFactors= FALSE)

# Extend the df_dates data.frame to have info on the year and layer
class(df_dates$date)  # character
df_dates$date <- as.Date(df_dates$date, format="%d/%m/%Y")
df_dates$year <- factor(substr(df_dates$date, 1, 4), levels = c(1984:2011))  # year as a Factor
df_dates$layer <- names(rBrick)


## I- Extract scene information 

# Show the number of scenes per year. 
# Having years as a factor allows to have an even scale.

ggplot(data=df_dates, aes(x=year))+
  geom_bar(width=0.5) +
  labs(x= " Time", y = "Number of scenes") +
  theme_bw() 

# Calculate data loss from masking process in preprocessing
  # (clouds, cloud sadows and SLC-off gaps)
# Per layer: 
num_na <- freq(rBrick, value=NA) / ncell(rBrick) * 100
df_dates$NoData <- round(num_na, 2)
# Visualize NAs
main_title <- "Percent of No Data values per scene"
ggplot(data = df_dates, main="", aes(x = year, y = NoData)) +
  geom_point(size = 2) +
  labs(y = "% nodata per layer", title = main_title) +
  theme_bw()


## II-Compare visually rasterBrick layers to find trends

# Visualize NDVI information (here example with the first 10 layers)
first_year <- df_dates$year[[1]]
last_year <- df_dates$year[[10]]
layers <- rBrickLL[[1:10]]
names_layers <- df_dates$date[c(1:10)]

map <- function(x, y, z){
  cols <- brewer.pal(5, 'RdYlGn')
  rtheme <- rasterTheme(region=cols)
  levelplot(x, main= paste("NDVI for the period",y,"-", z), names.attr=names_layers , par.settings=rtheme)
}

map(layers, first_year, last_year)

# Map the spatial distribution of data loss
percentNApixel <- function(x){
  length(x[is.na(x)]) / length(x) * 100
}

ALL_pixNA <- calc(rBrick, fun=percentNApixel)  # do not use rBrick reprojected here (rBrickLL)
# maxValue(ALL_pixNA)  # used to adapt the breaks below
# minValue(ALL_pixNA)
bks <- seq(5,55, by=5)
cols <- terrain.colors(11, alpha=1)
plot(ALL_pixNA, main = " Percent of NA values for the study period ", 
     breaks=bks, col=cols)  

# Compare two layers that reflect a change (layers 2 and 51)
# Convert dates as character (to add them to the legends)
first_date <- df_dates$date[[2]]
first_date <- as.character(first_date)
last_date <- df_dates$date[[51]]
last_date <- as.character(last_date)

cols <- brewer.pal(5, 'RdYlGn')
rtheme <- rasterTheme(region=cols)
levelplot(rBrickLL, layers=c(2,51), main=paste("NDVI Layers representing undisturbance (",first_date,") and deforestation (",last_date, ")"),, names.attr=df_dates$date[c(2,51)], par.settings=rtheme)

# Overlapping histograms  # do not use here the reprojected rBrick (rBrickLL)
ndvi_lay2 <- as.numeric(rBrick[2])
ndvi_lay51 <- as.numeric(rBrick[51])

# Find an appropriate range of x and y values
# minValue(rBrick[[2]])
# minValue(rBrick[[51]])
# maxValue(rBrick[[2]])
# maxValue(rBrick[[51]])

# Plots
hist(ndvi_lay2, freq=TRUE,col=rgb(1,0,0,0.5),xlim=c(0.1,1), ylim=c(0,15), main=paste("Histograms of undisturbance (",first_date,") and deforestation (",last_date, ")"), xlab="NDVI value", border="lightblue")
hist(ndvi_lay51, freq=TRUE, col=rgb(0,0,1,0.5), add=T)
legend("topright", c(first_date, last_date), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), lwd=10)
box()

bwplot(rBrick, layers=c(2,51), main=paste("Histograms of undisturbance (",first_date,") and deforestation (",last_date, ")"), 
       xlab="Undisturbed (left) and Deforested (right) scenes", ylab="NDVI value",
       scales=list(x=list(rot=0, cex=0.6)))


## III-Visualize in Google Earth the study area and 
# two pixels (deforested and undisturbed)

# Extract coordinates in decimal degrees
KML(x=rBrickLL, filename="Study_area.kml", overwrite=TRUE)

# Select representative pixels
# plot(rBrickLL[[51]])
# Click in two areas with different NDVI values
# cell_defor <- click()
# cell_undist <- click()

# Visualize those pixels in Google Earth
pnt_defor <- cbind(-47.03783, -3.017909)  # class numeric 
pnt_undist <- cbind(-47.02016, -3.023601)
coords <- rbind(pnt_defor,pnt_undist)  # in one single matrix
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
mypoints <- SpatialPoints(coords, proj4string=prj_string_WGS)
KML(x=mypoints, filename="Study_pixels.kml", zip='', overwrite=TRUE)


## IV-Extract time series

# Deforested pixel 
deforested <- matrix(data=pnt_defor, nrow=1,
                     dimnames=list(NULL, c('x', 'y')))  # class matrix
deforestedCell <- cellFromXY(rBrickLL, deforested)
print(deforestedCell)  ## pixel number 83205
rBrick[deforestedCell] # 1-row matrix with time series values

# Undisturbed pixel
undisturbed <- matrix(data=pnt_undist, nrow=1,
                     dimnames=list(NULL, c('x', 'y')))
undisturbedCell <- cellFromXY(rBrickLL, undisturbed)
print(undisturbedCell)  ## pixel number 90578
rBrick[undisturbedCell]

# Plot time series as facet wrap
# Create two separate data.frames

d1_undist <- data.frame(
  date = df_dates$date,
  ndvi = as.numeric(rBrick[undisturbedCell]),
  Landcover = "Undisturbed pixel"
)
d2_defor <- data.frame(
  date = df_dates$date,
  ndvi = as.numeric(rBrick[deforestedCell]),
  Landcover = "Deforested pixel"
)

# Combine them:
tss <- rbind(d1_undist, d2_defor)

# Plot both together as lines
ggplot(data = tss, aes(x = date, y = ndvi)) + 
  geom_line() +
  geom_point() +
  scale_x_date() +
  labs(title= paste("Time series of an undisturbed pixel (",first_date, ") and of a deforested one (", last_date, ")"),y = "NDVI") +
  facet_wrap(~ Landcover, nrow = 2) +
  theme_bw()

# Combine them in the same plot
ggplot(data = tss, aes(x = date, y = ndvi, colour = Landcover)) +
  labs(title= paste("Time series of an undisturbed pixel (",first_date, ") and of a deforested one (", last_date, ")"),y = "NDVI") +
  geom_line() +
  geom_point() +
  theme_bw()

# Analysing time series 
summary(d1_undist)
summary(d2_defor)










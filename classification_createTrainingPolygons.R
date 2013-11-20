#  Author: Amalia Castro
#  Date: November 2013
#  Dependencies: rasta, raster, rgdal, sp, ggplot2, rasterVis, maptools
#  Description: create training polygons (SpatialPolygonsDataFrame) 
    # for the following classes: forest, bamboo, coffee plantation, 
    # wetland, forest, baresoil

library(rasta)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(maptools)

### Exploring raster data
# load in the training classes and Look-Up Table (LUT)
data(lulcGewata)
data(LUTGewata)
# plot lulcGewata 
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
plot(lulcGewata, col=cols, legend=FALSE)
legend("topright", legend=LUTGewata$Class, fill=cols)


### Draw training polygons 
## Cropland 
plot(lulcGewata, col=cols, legend=FALSE)
cropland <- drawPoly(sp=TRUE)
# append another polygon onto the same object 
cropland <- gUnion(cropland, drawPoly(sp=TRUE))
# set and check the coordinate reference system (CRS) of the polygons
projection(cropland) <- projection(lulcGewata)
projection(cropland)
plot(lulcGewata)
plot(cropland, add=TRUE)
# convert polygons to a SpatialPolygonsDataFrame 
cropland <- SpatialPolygonsDataFrame(cropland, data=data.frame(
  class="cropland"), match.ID=FALSE)

## Bamboo
plot(lulcGewata, col=cols, legend=FALSE)
e <- drawExtent() # zoom in
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
# now define a training polygon
bamboo <- drawPoly(sp=TRUE)
# set and check projection of the polygon
projection(bamboo) <- projection(lulcGewata)
plot(lulcGewata)
plot(bamboo, add=TRUE)
# convert to SpatialPolygonsDataFrame
bamboo <- SpatialPolygonsDataFrame(bamboo, data=data.frame(
  class="bamboo"), match.ID=FALSE)

## Bare soil
plot(lulcGewata, col=cols, legend=FALSE)
e <- drawExtent() # zoom in
plot(lulcGewata, col=cols, legend=FALSE, ext=e)
# now define a training polygon
baresoil <- drawPoly(sp=TRUE)
# set and check projection of the polygon
projection(baresoil) <- projection(lulcGewata)
plot(lulcGewata)
plot(baresoil, add=TRUE)
# convert to SpatialPolygonsDataFrame
baresoil <- SpatialPolygonsDataFrame(baresoil, data=data.frame(
  class="bare soil"), match.ID=FALSE)

## Coffee plantation
plot(lulcGewata, col=cols, legend=FALSE)
e <- drawExtent() # zoom into a coffee area

plot(lulcGewata, col=cols, legend=FALSE, ext=e)
# now define a training polygon
coffee <- drawPoly(sp=TRUE)
# set and check projection of the polygon
projection(coffee) <- projection(lulcGewata)
plot(lulcGewata)
plot(coffee, add=TRUE)
# convert to SpatialPolygonsDataFrame
coffee <- SpatialPolygonsDataFrame(coffee, data=data.frame(
  class="coffee plantation"), match.ID=FALSE)

## Forest
plot(lulcGewata, col=cols, legend=FALSE)
forest <- drawPoly(sp=TRUE)
# append another polygon onto the same object 
forest <- gUnion(forest, drawPoly(sp=TRUE))
# set and check the coordinate reference system (CRS) of the polygons
projection(forest) <- projection(lulcGewata)
projection(forest)
plot(lulcGewata)
plot(forest, add=TRUE)
# convert polygons to a SpatialPolygonsDataFrame 
forest <- SpatialPolygonsDataFrame(forest, data=data.frame(
  class="forest"), match.ID=FALSE)

## Wetland
plot(lulcGewata, col=cols, legend=FALSE)
wetland <- drawPoly(sp=TRUE)
# set and check the coordinate reference system (CRS) of the polygons
projection(wetland) <- projection(lulcGewata)
projection(wetland)
plot(lulcGewata)
plot(wetland, add=TRUE)
# convert polygons to a SpatialPolygonsDataFrame 
wetland <- SpatialPolygonsDataFrame(wetland, data=data.frame(
  class="wetland"), match.ID=FALSE)


### Fuse all the polygons into one SpatialPolygons object
cropland <- spChFIDs(cropland, "cropland")
forest <- spChFIDs(forest, "forest")
coffee <- spChFIDs(coffee, "coffee")
bamboo <- spChFIDs(bamboo, "bamboo")
baresoil <- spChFIDs(baresoil, "baresoil")
wetland <- spChFIDs(wetland, "wetland")

# Bind them 2 at a time as one object using spRbind (maptools)
trainingPolygons <- spRbind(cropland, forest)
trainingPolygons <- spRbind(trainingPolygons, coffee)
trainingPolygons <- spRbind(trainingPolygons, bamboo)
trainingPolygons <- spRbind(trainingPolygons, baresoil)
trainingPolygons <- spRbind(trainingPolygons, wetland)
# check
trainingPolygons@data
plot(lulcGewata)
plot(trainingPolygons, add=TRUE)

### Save trainingPoly as a shapefile (reproducibility)
writeOGR(trainingPolygons, file.path("data"), "trainingPolygons", driver="ESRI Shapefile")

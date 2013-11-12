# Author:Amalia Castro
# Date: November 2013
# Description: Script to calculate the yield per block in hectares.
# Dependencies: library(rgeos), library(rasta), library(rgdal), library(sp)
      # Written for MAC OX. If working with Windows, look at the alternative code.

## CreateHarvestTrack
# The input is a temporally ordered data frame (where the GPS points are not referenced and are separated by a maximum distance).
# The path where the real track occurs depends on some parameters (working width and harvest date) and there is one harvesting
# path for each load number.
# The function considers those parameters and derives an estimated continuous track for all the GPS points (within the maximum distance)
# that correspond to one load number.

library(rgdal)
library(rasta)
library(rgeos)
library(sp)


## 1- Buffer the obtained cleaned lines to create harvest blocks 
block <- gBuffer(cln_lines_df,byid =TRUE,  width =1.0, quadsegs = 5, capStyle = "ROUND", joinStyle = "ROUND") 
plot(block)

## 2- Fill small holes within  
block <- gBuffer(block, byid=T,id=rownames(block), width = 2.0)
block <- gBuffer(block, byid=T,id=rownames(block), width = -2.0)
block_df <- SpatialPolygonsDataFrame(block, cln_lines_df@data)

## 3- Compute yield per hectare for each block and add the attribute to the poloygos data frame  
areablockHa <- gArea(block_df, byid=TRUE) /10000
yieldBlockHa <- block_df$loads/areablockHa
block_df$yieldBlockHa <- yieldBlockHa

## 4-Map of the yield per hectar of each block
spplot(block_df, zcol="yieldBlockHa", colorkey=T,zlim=c(0,100),
       col.regions=c(bpy.colors(25)), pch=19,
       cex=0.25, main="Yield of each harvesting block (ton/ha)")

## 5- Export to KML 
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")  # give a geographical projected coordinate reference system for KML
linesRD <- spTransform(block_df, prj_string_WGS)  # transform to a planar coordinate system (RD)
# For Mac 
writeOGR(linesRD, "blockBoundaries.kml","blockBoundaries.kml",driver = "KML")

# # For Windows 
# writeOGR(linesRD, file.path("data", "blockBoundaries.kml"),
#          "blockBoundaries", driver="KML", overwrite_layer=TRUE)
# 

  



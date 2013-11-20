#  Author: Amalia Castro
#  Date: November 2013
#  Dependencies: rasta, raster, rgdal, sp, ggplot2, rasterVis, maptools, igraph, randomForest
#  Description: Random Forest classification using the Gewata covariates
   
library(rasta)
library(igraph)
library(raster)
library(rgdal)
library(sp)
library(ggplot2)
library(rasterVis)
library(maptools)
library(randomForest)

# Clean workspace
rm(list = ls())

### Load and check the data
data(GewataB2)
data(GewataB3)
data(GewataB4)
data(vcfGewata)  # Vegetation Continuous Field product
data(lulcGewata)
data(LUTGewata)
trainingPolygons <-readOGR("data", "trainingPolygons")

### Prepare the data to be classified: 
# Create rasterBrick with bands 2,3,4
gewata <- brick(GewataB2, GewataB3, GewataB4)
# Assign NA to flagged pixels corresponding to water or clouds (>100)
vcfGewata[vcfGewata > 100] <- NA
#Calculate NDVI and correct the values
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)})
ndvi <- calc(ndvi, fun = function(x) floor(x*10000))
dataType(ndvi) <- "INT2U"  # integers save memory
names(ndvi) <- "NDVI"  # rename the layer for easy interpretation 


### Assemble ALL covariate layers (append NDVI AND VCF s to current rasterBrick)
covs <- addLayer(gewata, ndvi, vcfGewata)


### Superimpose lulcGewata raster with training polygons 
cols <- c("orange", "light green", "brown", "light pink", "dark green", "light blue")
plot(lulcGewata, col=cols, legend=FALSE)
legend("topright", legend=LUTGewata$Class, fill=cols)
plot(trainingPolygons, add=TRUE)


### Relabel training classes
trainingPolygons@data  
# Substitute character label by factor level (reclassification function)
# in each element of the 'class' column to create the new 'code' column
reclass <- function(x){
  which(x==levels(trainingPolygons@data$class))
  }
trainingPolygons@data$Code <- sapply(trainingPolygons@data$class, FUN=reclass)


### Convert training polygons to raster
classes <- rasterize(trainingPolygons, ndvi, field='Code')
dataType(classes) <- "INT1U"
cols <- c("light green", "red2","brown","orange","dark green","light blue")
plot(classes, col=cols, legend=FALSE)
legend("topright", legend=c("bamboo", "bare soil","coffee","cropland","forest","wetland"), fill=cols, bg="white")


### Create a table of values representing all layers with known classes:
# Create a version of our rasterBrick representing only the training pixels: mask
covmasked <- mask(covs, classes)
# Add the classes layer to this new brick
names(classes) <- "class"
trainingbrick <- addLayer(covmasked, classes)
# Add the classes values to a data.frame representing all training data (input for RandomForest)
  # Extract values from all layers in the training pixels
valuetable <- getValues(trainingbrick)
  # Convert to a data.frame and keep rows whith values 
valuetable <- as.data.frame(valuetable)
valuetable <- valuetable[!is.na(valuetable$class),]
  # convert values in the class column to factors
valuetable$class <- factor(valuetable$class, levels = c(1:6))


### Visualize the distribution of the covariates 
# Add a label column to the valuetable
valuetable$label <- with(valuetable, ifelse(class==1, "bamboo",
                                            ifelse(class==2, "baresoil",
                                                   ifelse(class==3,"coffee",
                                                          ifelse (class==4,"cropland",
                                                                  ifelse (class==5, "forest", "wetland"))))))

# Make the ggplots splitting the data into facets
# Interpret:
## With Band 3 over Band 2: Bamboo and Forest classes have lower pixel levels (darker because they reflect
# less in the VIS). Baresoil can reach very high pixel values so can be very bright in an RGB combination.
# Those classes are easily separable but Forest will be more difficult.There is a relatively high overlap 
# between forest coffee, cropland and wetland.
# This is consistent with the class.error result of the confusion matrix: they have the lowest values.

# 1. NDVI
p1 <- ggplot(data=valuetable, aes(x=NDVI)) +
  geom_histogram(binwidth=300) +
  facet_wrap(~ label) +
  theme_bw()
p1
# 2. VCF
p2 <- ggplot(data=valuetable, aes(x=vcf2000Gewata)) +
  geom_histogram(binwidth=5) +
  labs(x="% Tree Cover") +
  facet_wrap(~ label) +
  theme_bw()
p2

# 4. Bands 3 and 4
p3 <- ggplot(data=valuetable, aes(x=gewataB3, y=gewataB4)) +
  stat_bin2d() +
  facet_wrap(~ label) +
  theme_bw()
p3

  # 4. Bands 2 and 3
p4 <- ggplot(data = valuetable, aes(x=gewataB2, y=gewataB3)) +
  stat_bin2d() +
  facet_wrap(~ label) +
  theme_bw()
p4


### Build the RandomForest model based on a matrix of predictors 
  # (covariates: the first 5 columns of valuetable) related to the response (class column)
# Keep only rows of valuetable with no NAs 
valuetable <- na.omit(valuetable)
# Construct RandomForest model and save it
modelRF <- randomForest(x=valuetable[,c(1:5)], y=valuetable$class,
                          importance = TRUE)  #### CHANGE THIS TO TRUE AND RERUN
save(modelRF,file="modelRF.RData")
# summary of the randomForest model
summary(modelRF)
class(modelRF)
str(modelRF)
names(modelRF)


### Make the confusion matrix readable
# Highest accuracy:baresoil
# Lowest accuracy: cropland
colnames(modelRF$confusion) <- c("bamboo", "baresoil", "coffee", "cropland","forest", "wetland", "class.error")
rownames(modelRF$confusion) <- c("bamboo", "baresoil", "coffee", "cropland","forest", "wetland")
modelRF$confusion


### Statistical importance of each covariate
# Bands 4 and 2 have the highest impact on accuracy (as opposed to bands 3 and 4 in the tutorial classification)
# Bands 2 and 3 have the highest decrease in impurity (same result for the tutorial classification)
# Band 2 becomes more important because we added here training areas that reflect more in that part of the spectrum 
# than vegetation (wetlands and bare soil)
varImpPlot(modelRF)


### Apply the modelRF to the rest of the image and assign classes to all pixels
names(covs)
names(valuetable)
predLC <- predict(covs, model=modelRF, na.rm=TRUE)


### Plot resulting land cover map with legend
plot(predLC, col=cols, legend=FALSE)
legend("topleft", legend=c("bamboo", "baresoil", "coffee", "cropland","forest", "wetland"), fill=cols, bg="white")



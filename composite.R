# Author: Amalia Castro
# Date: November 2013
# Dependencies: library(rasta)
# Description: function to create a mean value cloud-free image composite
    # for RasterBrick or RasterStack objects of 9 bands. 


library(rasta)

# Download and visualize the data
data(taravao)
plotRGB(x=taravao, 4,5,3)

data(taravao2)
plotRGB(x=taravao2,4,5,3)

cloudComposite <- function(image1, image2){
  # Generate cloud mask
  cloud1 <- calc(x=image1[[9]], fun=QA2cloud)
  cloud2 <- calc(x=image2[[9]], fun=QA2cloud)
  
  # Remove layer 9 (QA info only)
  image1_8 <- dropLayer(x=image1, i=9)
  image2_8 <- dropLayer(x=image2, i=9)
  
  # Replace value of clouds by NA
  image1_8[cloud1 == 1] <- NA
  image2_8[cloud2 == 1] <- NA
  
  # Calculate pixel-based mean:
  # Raster mean function definition
  Rmean <- function(x, y) {
    z <- c(x,y)
    result <- mean(z, na.rm=TRUE)
    return(result)
  }
  
  # Pixel-based mean function definition
  Vectorize_mean <- function(a, b) {
    out <- mapply(FUN=Rmean, a, b)
    return(out)
  }
  
  # Apply pixel-based mean function to our data
  mean_pixel <- overlay (x=image1_8, y=image2_8, fun=Vectorize_mean)
  
  # Visualize cloud-free composite
  plotRGB (mean_pixel, 4,5,3)
  
}

# Apply cloudCompositing to Landsat8 data 
cloudComposite(image1=taravao, image2=taravao2)




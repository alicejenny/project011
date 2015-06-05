#' Transforms on the YZ axis
#' 
#' Orients the mandible on the yz axis.
#' @param sample Input data frame. 3 columns, named "x", "y", and "z".
#' @export

yztransform <- function(sample){
  # defining the bounding box
  yzmat <- as.matrix(data.frame("y" = sample$y, "z" = sample$z))
  bbox <- getMinBBox(yzmat)
  
  # calculating the slope of the bounding box
  ymax.z <- bbox$z[which.max(bbox$y)]
  ymax.y <- max(bbox$y)
  zmax.z <- max(bbox$z)
  zmax.y <- bbox$y[which.max(bbox$z)]
  slp.ypoints <- c(ymax.y, zmax.y)
  slp.zpoints <- c(ymax.z, zmax.z)
  slp <- diff(slp.zpoints)/diff(slp.ypoints)
  rad <- pi/2 - atan(slp)
  
  # transforming to align based on the bounding box
  yzt.r <- data.frame("x" = sample$x, "y" = (sample$y * cos(rad)) - (sample$z * sin(rad)), "z" = (sample$y * sin(rad)) + (sample$z * cos(rad)))
  
  # rotating to horizontal
  if ((max(yzt.r$y) - min(yzt.r$y)) < (max(yzt.r$z) - min(yzt.r$z))){
    yzt.r <- data.frame("x" = sample$x, "y" = (yzt.r$y * cos(pi/2)) - (yzt.r$z * sin(pi/2)), "z" = (yzt.r$y * sin(pi/2)) + (yzt.r$z * cos(pi/2)))
  }
  
  # right way up
  if (yzt.r$z[which.max(yzt.r$y)] < yzt.r$z[which.min(yzt.r$y)]){
    yzt.r <- data.frame("x" = sample$x, "y" = (yzt.r$y * cos(pi)) - (yzt.r$z * sin(pi)), "z" = (yzt.r$y * sin(pi)) + (yzt.r$z * cos(pi)))
  }
  
  #results
  yzt.r
}
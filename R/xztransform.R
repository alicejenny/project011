#' Transforms on the XZ axis
#' 
#' Orients the mandible on the xz axis.
#' @param sample Input data frame. 3 columns, named "x", "y", and "z".
#' @export

xztransform <- function(sample){
  # defining bounding box
  xzmat <- as.matrix(data.frame("x" = sample$x, "z" = sample$z))
  bbox <- getMinBBox(xzmat)
  
  # calculating skew of bounding box
  zmax.x <- bbox$x[which.max(bbox$z)]
  zmax.z <- max(bbox$z)
  xmax.x <- max(bbox$x)
  xmax.z <- bbox$z[which.max(bbox$x)]
  slp.zpoints <- c(zmax.z, xmax.z)
  slp.xpoints <- c(zmax.x, xmax.x)
  slp <- diff(slp.zpoints)/diff(slp.xpoints)
  rad <- pi - atan(slp)
  
  # rotating based on bounding box
  xzt.r <- data.frame("x" = (sample$x * cos(rad)) - (sample$z * sin(rad)), "y" = sample$y, "z" = (sample$x * sin(rad)) + (sample$z * cos(rad)))
  
  # rotating to horizontal
  if ((max(xzt.r$x) - min(xzt.r$x)) < (max(xzt.r$z) - min(xzt.r$z))){
    xzt.r <- data.frame("x" = (xzt.r$x * cos(pi/2)) - (xzt.r$z * sin(pi/2)), "y" = sample$y, "z" = (xzt.r$x * sin(pi/2)) + (xzt.r$z * cos(pi/2)))
  }
  
  # rotating so chin is down
  xmax.z <- xzt.r$z[which.max(xzt.r$x)]
  xmin.z <- xzt.r$z[which.min(xzt.r$x)]
  
  if ((xmax.z < 0) & (xmax.z < 0)){
    xzt.r <- data.frame("x" = (xzt.r$x * cos(pi)) - (xzt.r$z* sin(pi)), "y" = sample$y, "z" = (xzt.r$x * sin(pi)) + (xzt.r$z * cos(pi)))
  }
  
  #results
  xzt.r
}
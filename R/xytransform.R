#' Transforms on the XY axis
#' 
#' Orients the mandible on the xy axis so the chin is pointing downwards.
#' @param sample Input data frame. 3 columns, named "x", "y", and "z".
#' @export

xytransform <- function(sample){
  # definining bounding box
  xymat <- as.matrix(data.frame("x" = sample$x, "y" = sample$y))
  bbox <- getMinBBox(xymat)
  
  # calculating skew of bounding box
  ymax.x <- bbox$x[which.max(bbox$y)]
  ymax.y <- max(bbox$y)
  xmax.x <- max(bbox$x)
  xmax.y <- bbox$y[which.max(bbox$x)]
  slp.ypoints <- c(ymax.y, xmax.y)
  slp.xpoints <- c(ymax.x, xmax.x)
  slp <- diff(slp.ypoints)/diff(slp.xpoints)
  rad <- pi - atan(slp)
  
  # rotating based on skew
  xyt.r <- data.frame("x" = (sample$x * cos(rad)) - (sample$y * sin(rad)), "y" = (sample$x * sin(rad)) + (sample$y * cos(rad)), "z" = sample$z)
  
  # making vertical WHY WON'T YOU WORK
  xmax.y <- xyt.r$y[which.max(xyt.r$x)]
  xmin.y <- xyt.r$y[which.min(xyt.r$x)]
  if (!isTRUE(all.equal(xmax.y, xmin.y, tolerance = 0.1))){
    xyt.r <- data.frame("x" = (xyt.r$x * cos(pi/2)) - (xyt.r$y * sin(pi/2)), "y" = (xyt.r$x * sin(pi/2)) + (xyt.r$y * cos(pi/2)), "z" = sample$z)
  }
  
  # rotating so chin is down
  xmax.y <- xyt.r$y[which.max(xyt.r$x)]
  xmin.y <- xyt.r$y[which.min(xyt.r$x)]
  if ((xmax.y < 0) & (xmax.y < 0)){
    xyt.r <- data.frame("x" = (xyt.r$x * cos(pi)) - (xyt.r$y * sin(pi)), "y" = (xyt.r$x * sin(pi)) + (xyt.r$y * cos(pi)), "z" = sample$z)
  }
  
  # results
  xyt.r
}
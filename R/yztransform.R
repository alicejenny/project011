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

  # finding the longest edge
  hpts <- chull(x = yzt.r$y, y = yzt.r$z)
  hf <- data.frame("y" = yzt.r$y[hpts], "z" = yzt.r$z[hpts])
  npts <- nrow(hf)
  dist <- c(numeric(npts))
  for (i in 1:npts){
    if (i != npts){
      a <- abs(hf$y[i] - hf$y[i+1])
      b <- abs(hf$z[i] - hf$z[i+1])
    }
    else {
      a <- abs(hf$y[i] - hf$y[1])
      b <- abs(hf$z[i] - hf$z[1])
    }
    c2 <- (a^2) + (b^2)
    c <- sqrt(c2)
    dist[i] <- c
  }
  m <- which.max(dist)
  le <- data.frame("y" = c(hf$y[m], hf$y[m+1]), "z" = c(hf$z[m], hf$z[m+1]))

  # right way up
  if (le$z[2] < 0){
    yzt.r <- data.frame("x" = sample$x, "y" = (yzt.r$y * cos(pi)) - (yzt.r$z * sin(pi)), "z" = (yzt.r$y * sin(pi)) + (yzt.r$z * cos(pi)))
  }

  #results
  yzt.r
}

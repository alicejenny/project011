#' Aligns the chin and jaw angle
#' 
#' Orients the mandible so the chin and jaw angle are in alignment. To be used after yz transform and xy transform (in that order).
#' @param sample Input data frame. 3 columns, named "x", "y", and "z".
#' @export

yzalign <- function(sample){
  # aligning y min (chin) and z min (jaw corner)
  ymin.z <- sample$z[which.min(sample$y)]
  ymin.y <- min(sample$y)
  zmin.z <- min(sample$z)
  zmin.y <- sample$y[which.min(sample$z)]
  slp.ypoints <- c(ymin.y, zmin.y)
  slp.zpoints <- c(ymin.z, zmin.z)
  slp <- diff(slp.zpoints)/diff(slp.ypoints)
  rad <- pi * 2 - atan(slp)
  yza.r <- data.frame("x" = sample$x, "y" = (sample$y * cos(rad)) - (sample$z * sin(rad)), "z" = (sample$y * sin(rad)) + (sample$z * cos(rad)))
}

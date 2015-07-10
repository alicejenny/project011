bfcull <- function(sample){
  sample <- data.frame("x" = sample[,1], "y" = sample[,2], "z" = sample[,3])
  plot(sample$x, sample$y, asp = 1)
  plot(sample$y, sample$z, asp = 1)
  plot(sample$x, sample$z, asp = 1)
  mat <- as.matrix(sample)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0))$normals
  normdf <- data.frame("xn" = c(normals[1,]), "yn" = c(normals[2,]), "zn" = c(normals[3,]))
  sixcol <- cbind(sample, normdf)
  culled <- subset(sixcol, zn >= 0)
  finish <- data.frame("x" = culled$x, "y" = culled$y, "z" = culled$z)
  plot(finish$x, finish$y, asp = 1)
  plot(finish$y, finish$z, asp = 1)
  plot(finish$x, finish$z, asp = 1)
}

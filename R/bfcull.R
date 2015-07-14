#' Cull backfaces
#'
#' Deletes vertices pointing downwards.
#' @export

bfcull <- function(sample){
  mat <- as.matrix(sample)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0), silent = TRUE)$normals
  normdf <- data.frame("xn" = c(normals[1,]), "yn" = c(normals[2,]), "zn" = c(normals[3,]))
  sixcol <- cbind(sample, normdf)
  sixcol.ordered <- sixcol[ order(-sixcol$z, -sixcol$zn),]
  sixcol.ordered <- sixcol.ordered[1:100,]
  if (sum(sixcol.ordered$zn >= 0) < sum(sixcol.ordered$zn < 0)){
    culled <- subset(sixcol, zn <= 0)
  }

  else {
    culled <- subset(sixcol, zn >= 0)
  }
  finish <- data.frame("x" = culled$x, "y" = culled$y, "z" = culled$z)
  return(finish)
}

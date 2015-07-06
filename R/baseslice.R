#' Isolate the base
#'
#' Slices off the base (xmin + 10%), inverts it, and culls backfaces. Saves as a txt.
#' @export

baseslice <- function(sample, filename, folder){
  require(Morpho)
  require(Rvcg)
  menempt <- subset(sample, z < (sample$z[which.min(sample$y)] + (max(sample$z) * 0.1)))
  globname <- str_replace(filename, "VERT", "-base")
  flipped <- data.frame("x" = menempt$x, "y" = (menempt$y * cos(pi)) - (menempt$z * sin(pi)), "z" = (menempt$y * sin(pi)) + (menempt$z * cos(pi)))
  mat <- as.matrix(flipped)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0))$normals
  normdf <- data.frame("xn" = c(normals[1,]), "yn" = c(normals[2,]), "zn" = c(normals[3,]))
  sixcol <- cbind(flipped, normdf)
  culled <- subset(sixcol, zn < 0)
  finish <- data.frame("x" = culled$x, "y" = culled$y, "z" = culled$z)
  fullfile <- paste(globname, ".xyz", sep = "")
  write.table(finish, file = paste(folder, fullfile, sep = "/"), row.names = FALSE, col.names = FALSE)
}

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

  # saving
  shortname <- str_replace(filename, "VERT", "-base")
  fullfile <- paste(shortname, ".xlsx", sep = "")
  fileandpath <- paste(folder, fullfile, sep = "//")
  if (file.exists(fileandpath) == TRUE){
    file.remove(fileandpath)
  }
  wb <- createWorkbook()
  addWorksheet(wb, shortname)
  writeData(wb, shortname, finish, colNames = FALSE, rowNames = FALSE)
  saveWorkbook(wb, fileandpath, overwrite = TRUE)
}

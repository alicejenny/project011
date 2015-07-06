#' Left-Right Split & Flip
#'
#' Splits the mandible in half sagittally, then rotates it so the buccal side is on top and culls backfaces.
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

lrflip <- function(sample, filename, folder) {
  require(Morpho)
  require(Rvcg)

  # slice
  leftside <- subset(sample, x > 0)
  rightside <- subset(sample, x < 0)

  # flip
  leftside <- data.frame("x" = (leftside$x * cos(pi/2)) - (leftside$z * sin(pi/2)), "y" = leftside$y, "z" = (leftside$x * sin(pi/2)) + (leftside$z * cos(pi/2)))
  rightside <- data.frame("x" = (rightside$x * cos(pi*1.5)) - (rightside$z * sin(pi*1.5)), "y" = rightside$y, "z" = (rightside$x * sin(pi*1.5)) + (rightside$z * cos(pi*1.5)))

  # cull (left)
  lmat <- as.matrix(leftside)
  lnormals <- vcgUpdateNormals(lmat, type = 0, pointcloud = c(10,0))$normals
  lnormdf <- data.frame("xn" = c(lnormals[1,]), "yn" = c(lnormals[2,]), "zn" = c(lnormals[3,]))
  lsixcol <- cbind(leftside, lnormdf)
  lculled <- subset(lsixcol, zn < 0)
  leftside <- data.frame("x" = lculled$x, "y" = lculled$y, "z" = lculled$z)

  # cull (right)
  rmat <- as.matrix(rightside)
  rnormals <- vcgUpdateNormals(rmat, type = 0, pointcloud = c(10,0))$normals
  rnormdf <- data.frame("xn" = c(rnormals[1,]), "yn" = c(rnormals[2,]), "zn" = c(rnormals[3,]))
  rsixcol <- cbind(rightside, rnormdf)
  rculled <- subset(rsixcol, zn < 0)
  rightside <- data.frame("x" = rculled$x, "y" = rculled$y, "z" = rculled$z)

  # save
  lname <- str_replace(filename, "VERT", ".l")
  rname <- str_replace(filename, "VERT", ".r")
  lfullfile <- paste(lname, ".txt", sep = "")
  rfullfile <- paste(rname, ".txt", sep = "")
  write.table(leftside, file = paste(folder, lfullfile, sep = "/"), row.names = FALSE, col.names = FALSE)
  write.table(rightside, file = paste(folder, rfullfile, sep = "/"), row.names = FALSE, col.names = FALSE)
}

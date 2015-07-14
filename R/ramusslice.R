#' Posterior Ramus Slice
#'
#' Isolates the posterior ramus, flips to horizontal, and culls backfaces.
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

ramusslice <- function(sample, filename, folder, saveplots = TRUE){
  require(Morpho)
  require(Rvcg)

  msg <- paste("Isolating posterior ramus for mandible", str_replace(filename, "VERT", ""))
  message(msg)

  left <- subset(sample, x >= 0)
  right <- subset(sample, x < 0)

  # LEFT
  # calculating convex hull edge lengths
  topfiveedges(left$y, left$z)
  topfive <- data.frame("y" = topfive$x, "z" = topfive$y)
  topfivep1 <- data.frame("y" = topfivep1$x, "z" = topfivep1$y)
  topfivep2 <- data.frame("y" = topfivep2$x, "z" = topfivep2$y)

  # base
  minz <- which.min(topfivep1$z)
  base <- rbind(topfivep1[minz,],topfivep2[minz,])
  baseR <- base[which.max(base$y),]

  # condyle
  zmax <- max(topfive$z)
  trquad <- subset(topfive, y > 0 & z > (zmax/2))
  trquad <- trquad[ order(-trquad$y), ]
  toptwo <- trquad[1:2,]
  condyleR <- toptwo[which.max(toptwo$z),]

  # find slope
  slp.ypoints <- c(condyleR[1,1], baseR[1,1])
  slp.zpoints <- c(condyleR[1,2], baseR[1,2])
  slp <- diff(slp.zpoints)/diff(slp.ypoints)

  # find y intercept
  c <- condyleR[1,2] - (slp * condyleR[1,1])

  # slice
  ramus <- subset(left, z < (slp*y) + c)

  # rotate
  rad <- pi - atan(slp)
  ramusrot <- data.frame("x" = ramus$x, "y" = (ramus$y * cos(rad)) - (ramus$z * sin(rad)), "z" = (ramus$y * sin(rad)) + (ramus$z * cos(rad)))

  # left side
  leftside <- centre(ramusrot)

  ymin.y <- min(leftside$y)
  ymin.x <- leftside$x[which.min(leftside$y)]
  ymax.y <- max(leftside$y)
  ymax.x <- leftside$x[which.max(leftside$y)]
  slp.ypoints <- c(ymin.y, ymax.y)
  slp.xpoints <- c(ymin.x, ymax.x)
  slp <- diff(slp.ypoints)/diff(slp.xpoints)
  rad <- pi/2 - atan(slp)
  leftside <- data.frame("x" = (leftside$x * cos(rad)) - (leftside$y * sin(rad)), "y" = (leftside$x * sin(rad)) + (leftside$y * cos(rad)), "z" = leftside$z)

  # backface culling (left)
  mat <- as.matrix(leftside)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0), silent = TRUE)$normals
  normdf <- data.frame("xn" = c(normals[1,]), "yn" = c(normals[2,]), "zn" = c(normals[3,]))
  sixcol <- cbind(leftside, normdf)
  if (sixcol$zn[which.max(sixcol$z)] < 0){
    culled <- subset(sixcol, zn <= 0)
  }

  else {
    culled <- subset(sixcol, zn >= 0)
  }
  leftside <- data.frame("x" = culled$x, "y" = culled$y, "z" = culled$z)

  # saving left
  shortname <- str_replace(filename, "VERT", "-ramusL")
  fullfile <- paste(shortname, ".xlsx", sep = "")
  fileandpath <- paste(folder, fullfile, sep = "//")
  if (file.exists(fileandpath) == TRUE){
    file.remove(fileandpath)
  }
  wb <- createWorkbook()
  addWorksheet(wb, shortname)
  writeData(wb, shortname, leftside, colNames = TRUE, rowNames = FALSE)
  saveWorkbook(wb, fileandpath, overwrite = TRUE)

  #RIGHT
  # calculating convex hull edge lengths
  topfiveedges(right$y, right$z)
  topfive <- data.frame("y" = topfive$x, "z" = topfive$y)
  topfivep1 <- data.frame("y" = topfivep1$x, "z" = topfivep1$y)
  topfivep2 <- data.frame("y" = topfivep2$x, "z" = topfivep2$y)

  # base
  minz <- which.min(topfivep1$z)
  base <- rbind(topfivep1[minz,],topfivep2[minz,])
  baseR <- base[which.max(base$y),]

  # condyle
  zmax <- max(topfive$z)
  trquad <- subset(topfive, y > 0 & z > (zmax/2))
  trquad <- trquad[ order(-trquad$y), ]
  toptwo <- trquad[1:2,]
  condyleR <- toptwo[which.max(toptwo$z),]

  # find slope
  slp.ypoints <- c(condyleR[1,1], baseR[1,1])
  slp.zpoints <- c(condyleR[1,2], baseR[1,2])
  slp <- diff(slp.zpoints)/diff(slp.ypoints)

  # find y intercept
  c <- condyleR[1,2] - (slp * condyleR[1,1])

  # slice
  ramus <- subset(right, z < (slp*y) + c)

  # rotate
  rad <- pi - atan(slp)
  ramusrot <- data.frame("x" = ramus$x, "y" = (ramus$y * cos(rad)) - (ramus$z * sin(rad)), "z" = (ramus$y * sin(rad)) + (ramus$z * cos(rad)))

  # right side
  rightside <- centre(ramusrot)

  ymin.y <- min(rightside$y)
  ymin.x <- rightside$x[which.min(rightside$y)]
  ymax.y <- max(rightside$y)
  ymax.x <- rightside$x[which.max(rightside$y)]
  slp.ypoints <- c(ymin.y, ymax.y)
  slp.xpoints <- c(ymin.x, ymax.x)
  slp <- diff(slp.ypoints)/diff(slp.xpoints)
  rad <- pi/2 - atan(slp)
  rightside <- data.frame("x" = (rightside$x * cos(rad)) - (rightside$y * sin(rad)), "y" = (rightside$x * sin(rad)) + (rightside$y * cos(rad)), "z" = rightside$z)

  # backface culling (right)
  mat <- as.matrix(rightside)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0), silent = TRUE)$normals
  normdf <- data.frame("xn" = c(normals[1,]), "yn" = c(normals[2,]), "zn" = c(normals[3,]))
  sixcol <- cbind(rightside, normdf)
  if (sixcol$zn[which.max(sixcol$z)] < 0){
    culled <- subset(sixcol, zn <= 0)
  }

  else {
    culled <- subset(sixcol, zn >= 0)
  }
  rightside <- data.frame("x" = culled$x, "y" = culled$y, "z" = culled$z)

  # saving right
  shortname <- str_replace(filename, "VERT", "-ramusR")
  fullfile <- paste(shortname, ".xlsx", sep = "")
  fileandpath <- paste(folder, fullfile, sep = "//")
  if (file.exists(fileandpath) == TRUE){
    file.remove(fileandpath)
  }
  wb <- createWorkbook()
  addWorksheet(wb, shortname)
  writeData(wb, shortname, rightside, colNames = TRUE, rowNames = FALSE)
  saveWorkbook(wb, fileandpath, overwrite = TRUE)

  plot(leftside$x, leftside$y, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "left ramus", sep = " "), asp = 1)
  plot(leftside$y, leftside$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "left ramus", sep = " "), asp = 1)
  plot(leftside$x, leftside$z, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "left ramus", sep = " "), asp = 1)
  plot(rightside$x, rightside$y, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "right ramus", sep = " "), asp = 1)
  plot(rightside$y, rightside$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "right ramus", sep = " "), asp = 1)
  plot(rightside$x, rightside$z, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "right ramus", sep = " "), asp = 1)
}

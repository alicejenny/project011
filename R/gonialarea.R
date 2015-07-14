#' Gonial Area Slice
#'
#' Isolates the gonial area, flips to horizontal, and culls backfaces (in a weird glitchy kind of way).
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

gonialarea <- function(sample, filename, folder, saveplots = TRUE){
  require(Morpho)
  require(Rvcg)

  msg <- paste("Isolating gonial area for mandible", str_replace(filename, "VERT", ""))
  message(msg)

  left <- subset(sample, x >= 0)
  right <- subset(sample, x < 0)

  # LEFT
  topfiveedges(left$y, left$z)
  topfive <- data.frame("y" = topfive$x, "z" = topfive$y)
  topfivep1 <- data.frame("y" = topfivep1$x, "z" = topfivep1$y)
  topfivep2 <- data.frame("y" = topfivep2$x, "z" = topfivep2$y)

  # condyle
  zmax <- max(topfive$z)
  trquad <- subset(topfive, y > 0 & z > (zmax/2))
  trquad <- trquad[ order(-trquad$y), ]
  toptwo <- trquad[1:2,]
  condyleR <- toptwo[which.max(toptwo$z),]

  # ramus
  rhalf <- subset(topfive, y > 0)
  rhalf <- rhalf[ order(rhalf$z, rhalf$y), ]
  ramusB <- rhalf[2,]

  # min y
  toprquad <- subset(left, z >= max(left$z)*0.75 & y > 0)
  ymin <- toprquad[which.min(toprquad$y),]

  ydiff <- condyleR$y[1] - ymin$y[1]

  # slope
  slp.ypoints <- c(condyleR[1,1], ramusB[1,1])
  slp.zpoints <- c(condyleR[1,2], ramusB[1,2])
  slp <- diff(slp.zpoints)/diff(slp.ypoints)

  # find y intercept
  c <- condyleR[1,2] - (slp * (condyleR[1,1] - ydiff))

  # slice
  ramus <- subset(left, z < (slp*y) + c)

  # rotate
  rad <- pi/2 - atan(slp)
  ramusrot <- data.frame("x" = ramus$x, "y" = (ramus$y * cos(rad)) - (ramus$z * sin(rad)), "z" = (ramus$y * sin(rad)) + (ramus$z * cos(rad)))

  # left side
  leftside <- subset(ramusrot, x > 0)
  centre(leftside)
  rad <- pi/2
  leftside <- data.frame("x" = (leftside$x * cos(rad)) - (leftside$z * sin(rad)), "y" = leftside$y, "z" = (leftside$x * sin(rad)) + (leftside$z * cos(rad)))

  xmin.x <- min(leftside$x)
  xmin.z <- leftside$z[which.min(leftside$x)]
  xmax.x <- max(leftside$x)
  xmax.z <- leftside$z[which.max(leftside$x)]
  slp.zpoints <- c(xmin.z, xmax.z)
  slp.xpoints <- c(xmin.x, xmax.x)
  slp <- diff(slp.zpoints)/diff(slp.xpoints)
  rad <- pi*2 - atan(slp)
  leftside <- data.frame("x" = (leftside$x * cos(rad)) - (leftside$z * sin(rad)), "y" = leftside$y, "z" = (leftside$x * sin(rad)) + (leftside$z * cos(rad)))

  minpoint <- min(c(leftside$z[which.max(leftside$y)], leftside$z[which.max(leftside$x)]))
  leftside <- subset(leftside, z > minpoint)
  leftside <- centre(leftside)
  leftside <- subset(leftside, x >= 0)

  # backface culling (left)
  leftside <- bfcull(leftside)

  # saving left
  shortname <- str_replace(filename, "VERT", "-gonialL")
  fullfile <- paste(shortname, ".xlsx", sep = "")
  fileandpath <- paste(folder, fullfile, sep = "//")
  if (file.exists(fileandpath) == TRUE){
    file.remove(fileandpath)
  }
  wb <- createWorkbook()
  addWorksheet(wb, shortname)
  writeData(wb, shortname, leftside, colNames = TRUE, rowNames = FALSE)
  saveWorkbook(wb, fileandpath, overwrite = TRUE)


  # RIGHT
  topfiveedges(right$y, right$z)
  topfive <- data.frame("y" = topfive$x, "z" = topfive$y)
  topfivep1 <- data.frame("y" = topfivep1$x, "z" = topfivep1$y)
  topfivep2 <- data.frame("y" = topfivep2$x, "z" = topfivep2$y)

  # condyle
  zmax <- max(topfive$z)
  trquad <- subset(topfive, y > 0 & z > (zmax/2))
  trquad <- trquad[ order(-trquad$y), ]
  toptwo <- trquad[1:2,]
  condyleR <- toptwo[which.max(toptwo$z),]

  # ramus
  rhalf <- subset(topfive, y > 0)
  rhalf <- rhalf[ order(rhalf$z, rhalf$y), ]
  ramusB <- rhalf[2,]

  # min y
  toprquad <- subset(right, z >= max(right$z)*0.75 & y > 0)
  ymin <- toprquad[which.min(toprquad$y),]

  ydiff <- condyleR$y[1] - ymin$y[1]

  # slope
  slp.ypoints <- c(condyleR[1,1], ramusB[1,1])
  slp.zpoints <- c(condyleR[1,2], ramusB[1,2])
  slp <- diff(slp.zpoints)/diff(slp.ypoints)

  # find y intercept
  c <- condyleR[1,2] - (slp * (condyleR[1,1] - ydiff))

  # slice
  ramus <- subset(right, z < (slp*y) + c)

  # rotate
  rad <- pi/2 - atan(slp)
  ramusrot <- data.frame("x" = ramus$x, "y" = (ramus$y * cos(rad)) - (ramus$z * sin(rad)), "z" = (ramus$y * sin(rad)) + (ramus$z * cos(rad)))

  # right side
  rightside <- subset(ramusrot, x < 0)
  centre(rightside)
  rad <- pi*1.5
  rightside <- data.frame("x" = (rightside$x * cos(rad)) - (rightside$z * sin(rad)), "y" = rightside$y, "z" = (rightside$x * sin(rad)) + (rightside$z * cos(rad)))

  xmin.x <- min(rightside$x)
  xmin.z <- rightside$z[which.min(rightside$x)]
  xmax.x <- max(rightside$x)
  xmax.z <- rightside$z[which.max(rightside$x)]
  slp.zpoints <- c(xmin.z, xmax.z)
  slp.xpoints <- c(xmin.x, xmax.x)
  slp <- diff(slp.zpoints)/diff(slp.xpoints)
  rad <- pi*2 - atan(slp)
  rightside <- data.frame("x" = (rightside$x * cos(rad)) - (rightside$z * sin(rad)), "y" = rightside$y, "z" = (rightside$x * sin(rad)) + (rightside$z * cos(rad)))

  minpoint <- min(c(rightside$z[which.max(rightside$y)], rightside$z[which.max(rightside$x)]))
  rightside <- subset(rightside, z > minpoint)
  rightside <- centre(rightside)
  rightside <- subset(rightside, x <= 0)

  # backface culling (right)
  rightside <- bfcull(rightside)

  # saving right
  shortname <- str_replace(filename, "VERT", "-gonialR")
  fullfile <- paste(shortname, ".xlsx", sep = "")
  fileandpath <- paste(folder, fullfile, sep = "//")
  if (file.exists(fileandpath) == TRUE){
    file.remove(fileandpath)
  }
  wb <- createWorkbook()
  addWorksheet(wb, shortname)
  writeData(wb, shortname, rightside, colNames = TRUE, rowNames = FALSE)
  saveWorkbook(wb, fileandpath, overwrite = TRUE)

  plot(leftside$x, leftside$y, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "left gonial area", sep = " "), asp = 1)
  plot(leftside$y, leftside$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "left gonial area", sep = " "), asp = 1)
  plot(leftside$x, leftside$z, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "left gonial area", sep = " "), asp = 1)
  plot(rightside$x, rightside$y, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "right gonial area", sep = " "), asp = 1)
  plot(rightside$y, rightside$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "right gonial area", sep = " "), asp = 1)
  plot(rightside$x, rightside$z, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "right gonial area", sep = " "), asp = 1)
}

#' Gonial Area Slice
#'
#' Isolates the gonial area, flips to horizontal, and culls backfaces (in a weird glitchy kind of way).
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

gonialarea <- function(sample, filename, folder){
  require(Morpho)
  require(Rvcg)

  sample <- data.frame("x" = sample[,1], "y" = sample[,2], "z" = sample[,3])

  # calculating convex hull edge lengths
  hpts <- chull(x = sample$y, y = sample$z)
  hf <- data.frame("y" = sample$y[hpts], "z" = sample$z[hpts])
  npts <- nrow(hf)
  dist <- data.frame("dist" = numeric(npts), "p1y" = numeric(npts), "p1z" = numeric(npts), "p2y" = numeric(npts), "p2z" = numeric(npts))
  for (i in 1:npts){
    if (i != npts){
      a <- abs(hf$y[i] - hf$y[i+1])
      b <- abs(hf$z[i] - hf$z[i+1])
      dist$p1y[i] <- hf$y[i]
      dist$p2y[i] <- hf$y[i+1]
      dist$p1z[i] <- hf$z[i]
      dist$p2z[i] <- hf$z[i+1]
    }
    else {
      a <- abs(hf$y[i] - hf$y[1])
      b <- abs(hf$z[i] - hf$z[1])
      dist$p1y[i] <- hf$y[i]
      dist$p2y[i] <- hf$y[1]
      dist$p1z[i] <- hf$z[i]
      dist$p2z[i] <- hf$z[1]
    }

    c2 <- (a^2) + (b^2)
    c <- sqrt(c2)
    dist$dist[i] <- c
  }

  sorted <- dist[ order(-dist$dist, dist$p1y, dist$p2y), ]

  #longest
  one <- data.frame("y" = c(sorted$p1y[1],sorted$p2y[1]), "z" = c(sorted$p1z[1], sorted$p2z[1]))
  one <- one[ order(one$z, one$y), ]

  # second longest
  two <- data.frame("y" = c(sorted$p1y[2],sorted$p2y[2]), "z" = c(sorted$p1z[2], sorted$p2z[2]))
  two <- two[ order(two$z, two$y), ]

  # third longest
  three <- data.frame("y" = c(sorted$p1y[3],sorted$p2y[3]), "z" = c(sorted$p1z[3], sorted$p2z[3]))
  three <- three[ order(three$z, three$y), ]

  # fourth longest
  four <- data.frame("y" = c(sorted$p1y[4],sorted$p2y[4]), "z" = c(sorted$p1z[4], sorted$p2z[4]))
  four <- four[ order(four$z, four$y), ]

  # fifth longest
  five <- data.frame("y" = c(sorted$p1y[5],sorted$p2y[5]), "z" = c(sorted$p1z[5], sorted$p2z[5]))
  five <- five[ order(five$z, five$y), ]

  # top five
  topfivep1 <- rbind(one[1,],two[1,],three[1,],four[1,],five[1,])
  topfivep2 <- rbind(one[2,],two[2,],three[2,],four[2,],five[2,])
  topfive <- rbind(topfivep1, topfivep2)

  # condyle
  maxz <- which.max(topfive$z)
  if (maxz > 5){
    maxz <- maxz - 5
    maxzline <- rbind(topfivep2[maxz,], topfivep1[maxz,])
  }
  else {
    maxzline <- rbind(topfivep1[maxz,], topfivep2[maxz,])
  }
  if (maxzline[2,1] > 0){
    condyle <- maxzline
  }
  else {
    rem <- subset(topfive, (y != maxzline[1,1] & z != maxzline[1,2]) | (y != maxzline[2,1] & z != maxzline[2,2]))
    maxz <- which.max(rem$z)
    if (maxz > 5){
      maxz <- maxz - 5
      maxzline <- rbind(topfivep2[maxz,], topfivep1[maxz,])
    }
    else {
      maxzline <- rbind(topfivep1[maxz,], topfivep2[maxz,])
    }
    if (maxzline[2,1] > 0){
      condyle <- maxzline
    }
    else {
      message("Can't find the condyle.")
      condyle <- NULL
    }
  }
  condyleR <- condyle[which.max(condyle$y),]

  # ramus
  rhalf <- subset(topfive, y > 0)
  rhalf <- rhalf[ order(rhalf$z, rhalf$y), ]
  ramusB <- rhalf[2,]

  # min y
  top <- subset(sample, z >= max(sample$y))
  ymin <- top[which.min(top$y),]

  ydiff <- condyleR$y[1] - ymin$y[1]

  # slope
  slp.ypoints <- c(condyleR[1,1], ramusB[1,1])
  slp.zpoints <- c(condyleR[1,2], ramusB[1,2])
  slp <- diff(slp.zpoints)/diff(slp.ypoints)

  # find y intercept
  c <- condyleR[1,2] - (slp * (condyleR[1,1] - ydiff))

  # slice
  ramus <- subset(sample, z < (slp*y) + c)

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
  mat <- as.matrix(leftside)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0))$normals
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

  rightside <- centre(rightside)
  rightside <- subset(rightside, x <= 0)
  minpoint <- min(c(rightside$z[which.max(rightside$y)], rightside$z[which.max(rightside$x)]))
  rightside <- subset(rightside, z > minpoint)
  rightside <- centre(rightside)

  # backface culling (right)
  mat <- as.matrix(rightside)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0))$normals
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
}

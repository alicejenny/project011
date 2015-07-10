#' Ramus Slice
#'
#' Isolates the posterior ramus, flips to horizontal, and culls backfaces.
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

ramusslice <- function(sample, filename, folder){
  require(Morpho)
  require(Rvcg)
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

  # base
  minz <- which.min(topfivep1$z)
  base <- rbind(topfivep1[minz,],topfivep2[minz,])
  baseR <- base[which.max(base$y),]

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
      cat("Can't find the condyle.")
      condyle <- NULL
    }
  }
  condyleR <- condyle[which.max(condyle$y),]

  # find slope
  slp.ypoints <- c(condyleR[1,1], baseR[1,1])
  slp.zpoints <- c(condyleR[1,2], baseR[1,2])
  slp <- diff(slp.zpoints)/diff(slp.ypoints)

  # find y intercept
  c <- condyleR[1,2] - (slp * condyleR[1,1])

  # slice
  ramus <- subset(sample, z < (slp*y) + c)

  # rotate
  rad <- pi - atan(slp)
  ramusrot <- data.frame("x" = ramus$x, "y" = (ramus$y * cos(rad)) - (ramus$z * sin(rad)), "z" = (ramus$y * sin(rad)) + (ramus$z * cos(rad)))

  # centring
  ramusrot <- centre(ramusrot)

  # backface culling
  mat <- as.matrix(ramusrot)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0))$normals
  normdf <- data.frame("xn" = c(normals[1,]), "yn" = c(normals[2,]), "zn" = c(normals[3,]))
  sixcol <- cbind(ramusrot, normdf)

  if (sixcol$zn[which.max(sixcol$z)] < 0){
    culled <- subset(sixcol, zn <= 0)
  }

  else {
    culled <- subset(sixcol, zn >= 0)
  }

  finish <- data.frame("x" = culled$x, "y" = culled$y, "z" = culled$z)

  # saving
  shortname <- str_replace(filename, "VERT", "-ramus")
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

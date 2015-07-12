#' Mental Eminence Slice
#'
#' Isolates the mental eminence, flips to horizontal, and culls backfaces.
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

menemslice <- function(sample){
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

  # finding chin top
  if (min(topfivep1$y) < min(topfivep2$y)){
    chintop <- topfivep2[which.min(topfivep1$y),]
  }
  if (min(topfivep2$y) < min(topfivep1$y)){
    chintop <- topfivep1[which.min(topfivep2$y),]
  }

  # isolate base
  base <- subset(sample, z <= chintop$z[1])

  # centre
  cenbase <- data.frame("x" = base$x, "y" = base$y - chintop$y[1], "z" = base$z)

  # 10%
  yrange <- max(base$y) - chintop$y[1]
  tenpc <- yrange * 0.1

  # slice
  menem <- subset(cenbase, y <= tenpc)

  # rotate
  rad <- pi * 1.5
  menemrot <- data.frame("x" = menem$x, "y" = (menem$y * cos(rad)) - (menem$z * sin(rad)), "z" = (menem$y * sin(rad)) + (menem$z * cos(rad)))

  par(mfrow=c(1,3))
  #plot(sample$y, sample$z, asp = 1)
  #points(chintop$y, chintop$z, col = "red")
  plot(menemrot$x, menemrot$y, asp = 1)
  edgelength(menemrot$x, menemrot$y)
  plot(menemrot$y, menemrot$z, asp = 1)
  plot(menemrot$x, menemrot$z, asp = 1)
}

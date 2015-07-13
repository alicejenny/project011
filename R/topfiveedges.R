#' Find the five longest edges of convex hull
#'
#' Find the five longest edges of convex hull. Returns topfivep1 (one set of points), topfivep2 (the corresponding points), and topfive (topfivep1, then topfivep2) to parent environment.
#' @export

topfiveedges <- function(x,y){
  # calculating convex hull edge lengths
  hpts <- chull(x = x, y = y)
  hf <- data.frame("x" = x[hpts], "y" = y[hpts])
  npts <- nrow(hf)
  dist <- data.frame("dist" = numeric(npts), "p1y" = numeric(npts), "p1z" = numeric(npts), "p2y" = numeric(npts), "p2z" = numeric(npts))
  for (i in 1:npts){
    if (i != npts){
      a <- abs(hf$x[i] - hf$x[i+1])
      b <- abs(hf$y[i] - hf$y[i+1])
      dist$p1y[i] <- hf$x[i]
      dist$p2y[i] <- hf$x[i+1]
      dist$p1z[i] <- hf$y[i]
      dist$p2z[i] <- hf$y[i+1]
    }
    else {
      a <- abs(hf$x[i] - hf$x[1])
      b <- abs(hf$y[i] - hf$y[1])
      dist$p1y[i] <- hf$x[i]
      dist$p2y[i] <- hf$x[1]
      dist$p1z[i] <- hf$y[i]
      dist$p2z[i] <- hf$y[1]
    }

    c2 <- (a^2) + (b^2)
    c <- sqrt(c2)
    dist$dist[i] <- c
  }

  sorted <- dist[ order(-dist$dist, dist$p1y, dist$p2y), ]

  #longest
  one <- data.frame("x" = c(sorted$p1y[1],sorted$p2y[1]), "y" = c(sorted$p1z[1], sorted$p2z[1]))
  one <- one[ order(one$y, one$x), ]

  # second longest
  two <- data.frame("x" = c(sorted$p1y[2],sorted$p2y[2]), "y" = c(sorted$p1z[2], sorted$p2z[2]))
  two <- two[ order(two$y, two$x), ]

  # third longest
  three <- data.frame("x" = c(sorted$p1y[3],sorted$p2y[3]), "y" = c(sorted$p1z[3], sorted$p2z[3]))
  three <- three[ order(three$y, three$x), ]

  # fourth longest
  four <- data.frame("x" = c(sorted$p1y[4],sorted$p2y[4]), "y" = c(sorted$p1z[4], sorted$p2z[4]))
  four <- four[ order(four$y, four$x), ]

  # fifth longest
  five <- data.frame("x" = c(sorted$p1y[5],sorted$p2y[5]), "y" = c(sorted$p1z[5], sorted$p2z[5]))
  five <- five[ order(five$y, five$x), ]

  # top five
  topfivep1 <- rbind(one[1,],two[1,],three[1,],four[1,],five[1,])
  topfivep2 <- rbind(one[2,],two[2,],three[2,],four[2,],five[2,])
  topfive <- rbind(topfivep1, topfivep2)

  # return to parent env
  assign("topfivep1", topfivep1, envir = parent.frame())
  assign("topfivep2", topfivep2, envir = parent.frame())
  assign("topfive", topfive, envir = parent.frame())
}

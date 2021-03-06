concavehull <- function(sample, k = 3){
  # required packages
  library(FNN)
  library(project011)
  library(SDMTools)

  kk <- max(c(k, 3))
  allInside <- FALSE
  l <- 0
  prog <- 0
  dataset <- data.frame("x" = sample$x, "y" = sample$y)
  if (nrow(dataset) < 3){
    return(NULL)
  }
  if (nrow(dataset) == 3){
    return(dataset)
  }
  kk <- min(c(kk, nrow(dataset) - 1))
  firstPoint <- dataset[which.min(dataset$y),]
  hull <- firstPoint
  currentPoint <- firstPoint
  dataset <- subset(dataset, x != firstPoint$x[1] & y != firstPoint$y[1])
  step <- 0
  pcdist <- 1
  continue <- TRUE
  previousPoint <- data.frame("x" = firstPoint$x[1]-1, "y" = firstPoint$y[1])
  plot(dataset, asp = 1)

  while (continue == TRUE){
    if (step == 5){
      dataset <- rbind(dataset, firstPoint)
    }
    its <- TRUE
    # loops until a non-intersecting point is found
    while (its == TRUE){
      dataset <- subset(dataset, x != currentPoint$x[1] & y != currentPoint$y[1])
      # find the nearest neighbours
      cl <- 1:nrow(dataset)
      knn.index <- attr(knn(dataset, currentPoint, cl, k = kk), "nn.index")
      knn.dist <- attr(knn(dataset, currentPoint, cl, k = kk), "nn.dist")
      kNearestPoints <- dataset[knn.index,]

      # calculate right-hand turn in radians
      knn.angle <- matrix(ncol = kk)
      for (i in 1:kk) {
        ccx <- kNearestPoints[i,1] - currentPoint[1,1]
        ccy <- kNearestPoints[i,2] - currentPoint[1,2]
        pcx <- previousPoint$x[1] - currentPoint$x[1]
        pcy <- previousPoint$y[1] - currentPoint$y[1]
        rad <- abs(atan2(ccy,ccx) - atan2(pcy,pcx))
        deg <-  rad * (180/pi)
        if (((pcx*ccy) - (pcy*ccx) > 0) & deg < 180){
          deg <- 360 - deg
        }
        if (((pcx*ccy) - (pcy*ccx) <= 0) & deg > 180){
          deg <- 360 - deg
        }
        knn.angle[,i] <- deg
      }

      # sorting by angle (decreasing)
      knn.angle.sorted <- data.frame("deg" = c(knn.angle), "x" = c(kNearestPoints[,1]), "y" = c(kNearestPoints[,2]), "dist" = c(knn.dist))
      knn.angle.sorted <- knn.angle.sorted[ order(-knn.angle.sorted$deg, knn.angle.sorted$x, knn.angle.sorted$y, knn.angle.sorted$dist), ]
      cPoints <- data.frame("x" = knn.angle.sorted$x, "y" = knn.angle.sorted$y)

      # if it's the first point, select the top one
      if (nrow(hull) == 1){
        hull <- rbind(hull, cPoints[1,])
        its <- FALSE
        previousPoint <- currentPoint
        currentPoint <- cPoints[1,]
      }

      # if not, select the first candidate that does not intersect any of the polygon edges
      if (nrow(hull) > 1 & its == TRUE){
        i <- 0
        while (its == TRUE & i < nrow(cPoints)){
          i <- i + 1
          ccline <- rbind(cPoints[i,], currentPoint)
          itslog <- c(numeric(nrow(hull)-1))
          for (j in 1:(nrow(hull)-1)) {
            line2 <- rbind(hull[j,],hull[j+1,])
            itslog[j] <- intersects(ccline, line2, TRUE)
          }
          if (sum(itslog) == 0){
            its <- FALSE
          }
        }
        # if a valid candidate was found
        if (its == FALSE){
          hull <- rbind(hull, cPoints[i,])
          kk <- max(c(k, 3))
          kk <- min(c(kk, nrow(dataset) - 1))
          previousPoint <- currentPoint
          currentPoint <- cPoints[i,]
          pcx <- currentPoint$x[1] - previousPoint$x[1]
          pcy <- currentPoint$y[1] - previousPoint$y[1]
          lines(hull, col = "blue")
        }

        # if no valid candidate found, increase k
        if (its == TRUE){
          kk <- kk + 1
          print("kk increased")
          print(kk)
        }
      }
    }
    step <- step + 1
    prog <- prog + 1
    continue <- (currentPoint[1,1] != firstPoint[1,1]) & (currentPoint[1,2] != firstPoint[1,2]) & (nrow(dataset) > 0)
  }
  if ((nrow(sample) - sum(pnt.in.poly(sample, hull)[,3])) > 0){
    print("at least one point is outside. you might want to re-run with a higher k.")
  }
  if ((nrow(sample) - sum(pnt.in.poly(sample, hull)[,3])) == 0){
    print("all points are inside the polygon!")
  }
  lines(hull, col = "red")
}

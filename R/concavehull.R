concavehull <- function(sample, k = 3){
  # required packages
  library(FNN)
  library(project011)
  library(SDMTools)

  kk <- max(c(k, 3))
  allInside <- FALSE
  l <- 0
  prog <- 0
  while (allInside == FALSE){
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
    step <- 2
    pcdist <- 1
    pcx <- -1
    pcy <- 0

    while (nrow(dataset) > 0){
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
          ccdist <- knn.dist[,i]
          ccx <- kNearestPoints[i,1] - currentPoint[1,1]
          ccy <- kNearestPoints[i,2] - currentPoint[1,2]
          dotprod <- (pcx * ccx) + (pcy * ccy)
          leng <- pcdist * ccdist
          rad <- dotprod / leng
          deg <-  rad * (180/pi)
          knn.angle[,i] <- deg
        }

        # sorting by angle (decreasing)
        knn.angle.sorted <- data.frame("deg" = c(knn.angle), "x" = c(kNearestPoints[,1]), "y" = c(kNearestPoints[,2]), "dist" = c(knn.dist))
        knn.angle.sorted <- knn.angle.sorted[ order(-knn.angle.sorted$deg, knn.angle.sorted$x, knn.angle.sorted$y, knn.angle.sorted$dist), ]
        cPoints <- data.frame("x" = knn.angle.sorted$x, "y" = knn.angle.sorted$y)
        print("sorted by angle")
        print(knn.angle.sorted)

        # if it's the first point, select the top one
        if (nrow(hull) == 1){
          hull <- rbind(hull, cPoints[1,])
          its <- FALSE
          currentPoint <- cPoints[1,]
          print("top selected")
          print(its)
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
            pcdist <- knn.angle.sorted$dist[i]
            pcx <- cPoints$x[i]
            pcy <- cPoints$y[i]
            kk <- max(c(k, 3))
            kk <- min(c(kk, nrow(dataset) - 1))
            currentPoint <- cPoints[i,]
            print("valid candidate found")
            print(its)
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
    }
    if ((nrow(sample) - sum(pnt.in.poly(sample, hull)[,3])) > 0){
      allInside <- FALSE
    }
    if ((nrow(sample) - sum(pnt.in.poly(sample, hull)[,3])) == 0){
      allInside <- TRUE
    }
    l <- l + 1
  }
  plot(hull)
}

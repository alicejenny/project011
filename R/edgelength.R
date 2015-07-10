#' Finding the five longest edges
#'
#' Finds and plots (on a pre-existing graph) the five longest edges of the convex hull.
#' @export
#'
edgelength <- function(x,y){

  # finding the longest edge
  hpts <- chull(x = x, y = y)
  hf <- data.frame("x" = x[hpts], "y" = y[hpts])
  npts <- nrow(hf)
  dist <- data.frame("dist" = numeric(npts), "p1x" = numeric(npts), "p1y" = numeric(npts), "p2x" = numeric(npts), "p2y" = numeric(npts))
  for (i in 1:npts){
    if (i != npts){
      a <- abs(hf$x[i] - hf$x[i+1])
      b <- abs(hf$y[i] - hf$y[i+1])
      dist$p1x[i] <- hf$x[i]
      dist$p2x[i] <- hf$x[i+1]
      dist$p1y[i] <- hf$y[i]
      dist$p2y[i] <- hf$y[i+1]
    }
    else {
      a <- abs(hf$x[i] - hf$x[1])
      b <- abs(hf$y[i] - hf$y[1])
      dist$p1x[i] <- hf$x[i]
      dist$p2x[i] <- hf$x[1]
      dist$p1y[i] <- hf$y[i]
      dist$p2y[i] <- hf$y[1]
    }

    c2 <- (a^2) + (b^2)
    c <- sqrt(c2)
    dist$dist[i] <- c
  }

  sorted <- dist[ order(-dist$dist, dist$p1x, dist$p2x), ]

  #longest
  one <- data.frame("x" = c(sorted$p1x[1],sorted$p2x[1]), "y" = c(sorted$p1y[1], sorted$p2y[1]))
  one <- one[ order(one$y, one$x), ]
  lines(one, col = "red")

  # second longest
  two <- data.frame("x" = c(sorted$p1x[2],sorted$p2x[2]), "y" = c(sorted$p1y[2], sorted$p2y[2]))
  two <- two[ order(two$y, two$x), ]
  lines(two, col = "orange")

  # third longest
  three <- data.frame("x" = c(sorted$p1x[3],sorted$p2x[3]), "y" = c(sorted$p1y[3], sorted$p2y[3]))
  three <- three[ order(three$y, three$x), ]
  lines(three, col = "yellow")

  # fourth longest
  four <- data.frame("x" = c(sorted$p1x[4],sorted$p2x[4]), "y" = c(sorted$p1y[4], sorted$p2y[4]))
  four <- four[ order(four$y, four$x), ]
  lines(four, col = "green")

  # fifth longest
  five <- data.frame("x" = c(sorted$p1x[5],sorted$p2x[5]), "y" = c(sorted$p1y[5], sorted$p2y[5]))
  five <- five[ order(five$y, five$x), ]
  lines(five, col = "blue")
}

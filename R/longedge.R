#' Find longest edge of convex hull
#'
#' Finds the longest edge in the convex hull. Returns a data frame with the two points on the line.
#' @param x Values on the x axis.
#' @param y Values on the y axis.
#' @export

longedge <- function(x, y){
  hpts <- chull(x = x, y = y)
  hf <- data.frame("x" = x[hpts], "y" = y[hpts])
  npts <- nrow(hf)
  dist <- c(numeric(npts))
  for (i in 1:npts){
    if (i != npts){
      a <- abs(hf$x[i] - hf$x[i+1])
      b <- abs(hf$y[i] - hf$y[i+1])
    }
    else {
      a <- abs(hf$x[i] - hf$x[1])
      b <- abs(hf$y[i] - hf$y[1])
    }
    c2 <- (a^2) + (b^2)
    c <- sqrt(c2)
    dist[i] <- c
  }
  m <- which.max(dist)
  data.frame("x" = c(hf$x[m], hf$x[m+1]), "y" = c(hf$y[m], hf$y[m+1]))
}

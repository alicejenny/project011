longedge <- function(y, z){
  hpts <- chull(x = y, y = z)
  hf <- data.frame("y" = y[hpts], "z" = z[hpts])
  npts <- nrow(hf)
  dist <- c(numeric(npts))
  for (i in 1:npts){
    if (i != npts){
      a <- abs(hf$y[i] - hf$y[i+1])
      b <- abs(hf$z[i] - hf$z[i+1])
    }
    else {
      a <- abs(hf$y[i] - hf$y[1])
      b <- abs(hf$z[i] - hf$z[1])
    }
    c2 <- (a^2) + (b^2)
    c <- sqrt(c2)
    dist[i] <- c
  }
  m <- which.max(dist)
  data.frame("y" = c(hf$y[m], hf$y[m+1]), "z" = c(hf$z[m], hf$z[m+1]))
}

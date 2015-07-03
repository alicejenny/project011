toothend <- function(sample) {
  hpts <- chull(x = sample$y, y = sample$z)
  hf <- data.frame("y" = sample$y[hpts], "z" = sample$z[hpts])
  yzframe <- data.frame("y" = sample$y, "z" = sample$z)
  middlebit <- subset(yzframe, )
  npts <- nrow(middlebit)
  closestpoint <- data.frame("dist" = numeric(npts))
  for (i in 1:npts) {
    distlist <- data.frame("dist" = numeric(nrow(hf)))
    for (j in 1:nrow(hf)) {
      distlist[j] <- dist(rbind(yzframe[i,], hf[j,]))
    }
    closestpoint[i] <- min(distlist)
  }
  thepoint <- data.frame("y" = sample$y[which.max(closestpoint)], "z" = sample$z[which.max(closestpoint)])
  plot(sample$y, sample$z, xlab = "y", ylab = "z", asp = 1)
  points(thepoint$y, thepoint$z, col = "red")
}

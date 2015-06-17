pch <- function(y, z){
  hpts <- chull(x = y, y = z)
  hpts <- c(hpts, hpts[1])
  lines(y[hpts], z[hpts], col = "red")
}

#' Plot convex hull
#'
#' Plots a graph of the sample, then draws the convex hull around it.
#' @param x Values on the x axis.
#' @param y Values on the y axis.
#' @export

pch <- function(x, y){
  plot(x, y, asp = 1)
  hpts <- chull(x = x, y = y)
  hpts <- c(hpts, hpts[1])
  lines(x[hpts], y[hpts], col = "red")
}

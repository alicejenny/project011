#' Translate centrally
#' 
#' Translate model so mid-x, mid-y, and min-z are on 0,0,0.
#' @param sample The input data. Three column data frame with lowercase headings "x", "y", and "z".
#' @keywords centre
#' @export
#' @examples
#' centre()

centre <- function(sample){
  mid.x <- mean(sample$x)
  mid.y <- mean(sample$y)
  min.z <- min(sample$z)
  centredresults <- data.frame("x" = sample$x - mid.x, "y" = sample$y - mid.y, "z" = sample$z - min.z)
  centredresults
}
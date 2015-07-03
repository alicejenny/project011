#' Left-Right Split
#'
#' Splits the mandible in half sagittally.
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

lrsplit <- function(sample, filename) {
  leftside <- subset(sample, x > 0)
  rightside <- subset(sample, x < 0)
  lglobname <- str_replace(filename, "VERT", ".l")
  assign(lglobname, leftside, envir = .GlobalEnv)
  rglobname <- str_replace(filename, "VERT", ".r")
  assign(rglobname, rightside, envir = .GlobalEnv)
  plot(leftside$x, leftside$y, xlab = "x", ylab = "y", asp = 1)
  plot(rightside$x, rightside$y, xlab = "x", ylab = "y", asp = 1)
}

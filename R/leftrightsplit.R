#' Left-Right Split
#'
#' Splits the mandible in half sagittally.
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

lrsplit <- function(sample, filename, folder) {
  leftside <- subset(sample, x > 0)
  rightside <- subset(sample, x < 0)
  lname <- str_replace(filename, "VERT", ".l")
  rname <- str_replace(filename, "VERT", ".r")
}

#' Mental Eminence Slice Part 2
#'
#' Isolates the mental eminence, flips to horizontal, and culls backfaces. Second part for calling from batch().
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export
#'
menemslicept2 <- function(sample, filename, folder){
  par(mfrow=c(2,3))

  # backface culling
  menemfin <- bfcull(sample)

  # saving
  shortname <- str_replace(filename, "VERT", "-menem")
  fullfile <- paste(shortname, ".xlsx", sep = "")
  fileandpath <- paste(folder, fullfile, sep = "//")
  if (file.exists(fileandpath) == TRUE){
    file.remove(fileandpath)
  }
  wb <- createWorkbook()
  addWorksheet(wb, shortname)
  writeData(wb, shortname, menemfin, colNames = TRUE, rowNames = FALSE)
  saveWorkbook(wb, fileandpath, overwrite = TRUE)

  plot(menemfin$x, menemfin$y, asp = 1, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
  edgelength(menemfin$x, menemfin$y)
  plot(menemfin$y, menemfin$z, asp = 1, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
  plot(menemfin$x, menemfin$z, asp = 1, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
}

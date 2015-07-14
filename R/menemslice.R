#' Mental Eminence Slice
#'
#' Isolates the mental eminence, flips to horizontal, and culls backfaces.
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

menemslice <- function(sample, filename, folder, saveplots = TRUE){
  require(Morpho)
  require(Rvcg)

  msg <- paste("Isolating mental eminence for mandible", str_replace(filename, "VERT", ""))
  message(msg)

  # calculating convex hull edge lengths
  topfiveedges(sample$y, sample$z)

  # finding chin top
  if (min(topfivep1$x) < min(topfivep2$x)){
    chintop <- topfivep2[which.min(topfivep1$x),]
    chintop <- data.frame("y" = chintop$x[1], "z" = chintop$y[1])
  }
  if (min(topfivep2$x) < min(topfivep1$x)){
    chintop <- topfivep1[which.min(topfivep2$x),]
    chintop <- data.frame("y" = chintop$x[1], "z" = chintop$y[1])
  }

  # isolate base
  base <- sample

  # centre
  cenbase <- data.frame("x" = base$x, "y" = base$y - chintop$y[1], "z" = base$z)

  # 10%
  yrange <- max(cenbase$y) - min(cenbase$y)
  tenpc <- yrange * 0.1

  # slice
  menem <- subset(cenbase, y <= tenpc)

  # rotate
  rad <- pi * 1.5
  menemrot <- data.frame("x" = menem$x, "y" = (menem$y * cos(rad)) - (menem$z * sin(rad)), "z" = (menem$y * sin(rad)) + (menem$z * cos(rad)))
  plot(menemrot$x, menemrot$y, asp = 1, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
  edgelength(menemrot$x, menemrot$y)
  plot(menemrot$y, menemrot$z, asp = 1, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
  plot(menemrot$x, menemrot$z, asp = 1, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))


  # remove teeth
  topfiveedges(menemrot$x, menemrot$y)
  #top5tophalf <- subset(topfive, y > (max(menemrot$y) * 0.75))
  top5tophalf <- topfive[ order(-topfive$y, topfive$x),]
  top5tophalf <- top5tophalf[1:5,]
  # xmin top
  xmintop <- top5tophalf[which.min(top5tophalf$x),]
  # xmax top
  xmaxtop <- top5tophalf[which.max(top5tophalf$x),]
  # lowest y
  menem.noteeth <- subset(menemrot, y < min(xmintop$y[1], xmaxtop$y[1]))

  # backface culling
  menemfin <- bfcull(menem.noteeth)

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

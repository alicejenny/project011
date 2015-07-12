#' Mental Eminence Slice
#'
#' Isolates the mental eminence, flips to horizontal, and culls backfaces.
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

menemslice <- function(sample){
  require(Morpho)
  require(Rvcg)

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
  base <- subset(sample, z <= chintop$z[1])

  # centre
  cenbase <- data.frame("x" = base$x, "y" = base$y - chintop$y[1], "z" = base$z)

  # 10%
  yrange <- max(base$y) - chintop$y[1]
  tenpc <- yrange * 0.1

  # slice
  menem <- subset(cenbase, y <= tenpc)

  # rotate
  rad <- pi * 1.5
  menemrot <- data.frame("x" = menem$x, "y" = (menem$y * cos(rad)) - (menem$z * sin(rad)), "z" = (menem$y * sin(rad)) + (menem$z * cos(rad)))

  # remove teeth
  topfiveedges(menemrot$x, menemrot$y)
  top5tophalf <- subset(topfive, y > (max(menemrot$y) * 0.75))
  # xmin top
  xmintop <- top5tophalf[which.min(top5tophalf$x),]
  # xmax top
  xmaxtop <- top5tophalf[which.max(top5tophalf$x),]
  # lowest y
  menem.noteeth <- subset(menemrot, y < min(xmintop$y[1], xmaxtop$y[1]))

  # backface culling
  mat <- as.matrix(menem.noteeth)
  normals <- vcgUpdateNormals(mat, type = 0, pointcloud = c(10,0), silent = TRUE)$normals
  normdf <- data.frame("xn" = c(normals[1,]), "yn" = c(normals[2,]), "zn" = c(normals[3,]))
  sixcol <- cbind(menem.noteeth, normdf)
  if (sixcol$zn[which.max(sixcol$z)] < 0){
    culled <- subset(sixcol, zn <= 0)
  }

  else {
    culled <- subset(sixcol, zn >= 0)
  }
  menemfin <- data.frame("x" = culled$x, "y" = culled$y, "z" = culled$z)

  par(mfrow=c(1,3))
  #plot(sample$y, sample$z, asp = 1)
  #points(chintop$y, chintop$z, col = "red")
  plot(menemrot$x, menemrot$y, asp = 1, main = "rotated xy")
  plot(menemrot$y, menemrot$z, asp = 1, main = "rotated yz")
  plot(menemrot$x, menemrot$z, asp = 1, main = "rotated xz")

  plot(menem.noteeth$x, menem.noteeth$y, asp = 1, main = "no teeth xy")
  plot(menem.noteeth$y, menem.noteeth$z, asp = 1, main = "no teeth yz")
  plot(menem.noteeth$x, menem.noteeth$z, asp = 1, main = "no teeth xz")

  plot(menemfin$x, menemfin$y, asp = 1, main = "final xy")
  plot(menemfin$y, menemfin$z, asp = 1, main = "final yz")
  plot(menemfin$x, menemfin$z, asp = 1, main = "final xz")
}

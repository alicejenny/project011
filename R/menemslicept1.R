#' Mental Eminence Slice Part 1
#'
#' Isolates the mental eminence, flips to horizontal, and culls backfaces. First part for calling from batch().
#' @param sample The input data frame. 3 named columns (x, y, and z, in that order).
#' @export

menemslicept1 <- function(sample, filename){
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

  # align
  leftme <- subset(menemrot, x > 0)
  rightme <- subset(menemrot, x < 0)
  rmin.y <- min(rightme$y)
  rmin.x <- rightme$x[which.min(rightme$y)]
  lmin.y <- min(leftme$y)
  lmin.x <- leftme$x[which.min(leftme$y)]
  slp.xpoints <- c(rmin.x, lmin.x)
  slp.ypoints <- c(rmin.y, lmin.y)
  slp <- diff(slp.ypoints)/diff(slp.xpoints)
  rad <- 2*pi - atan(slp)
  menemrot <- data.frame("x" = (menemrot$x * cos(rad)) - (menemrot$y * sin(rad)), "y" = (menemrot$x * sin(rad)) + (menemrot$y * cos(rad)), "z" = menemrot$z)
  assign("menempt1", menemrot, envir = parent.frame())
}

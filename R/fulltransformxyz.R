#' Align Mandible
#'
#' For calling from batch().
#' @param sample The input data frame. 3 columns (x, y, and z, in that order).
#' @export

align2 <- function(sample, filename) {
  # start time for calculating run time
  starttime <- Sys.time()

  # progress bar
  pb <- winProgressBar(title = "Aligning...", label = "Initialising...", max = 9)

  # initial plots
  par(mfrow=c(2,3))
  plot(sample$x, sample$y, xlab = "x", ylab = "y", main = "start xy", asp = 1)
  plot(sample$y, sample$z, xlab = "y", ylab = "z", main = "start yz", asp = 1)
  plot(sample$x, sample$z, xlab = "x", ylab = "z", main = "start xz", asp = 1)

  #empty objects
  src <- nrow(sample)
  finish <- data.frame("x" = numeric(src), "y" = numeric(src), "z" = numeric(src))
  l <- 0
  stable <- FALSE

  # centering
  setWinProgressBar(pb, 1, label = "Centering...")
  loopstart <- centre(sample)

  while (!stable){
    l <- l + 1

    stable <- isTRUE(all.equal(loopstart, finish, tolerance = 0.01))

    if (sum(finish) != 0){
      loopstart <- finish
    }

    # xy transform
    setWinProgressBar(pb, 2, label = "Transforming XY...")
    xytransform.res <- xytransform(loopstart)
    xytransform.res <- centre(xytransform.res)

    # yz transform
    setWinProgressBar(pb, 3, label = "Transforming YZ...")
    yztransform.res <- yztransform(xytransform.res)
    yztransform.res <- centre(yztransform.res)

    # xy transform again
    setWinProgressBar(pb, 4, label = "Transforming XY again...")
    xytransform.res <- xytransform(yztransform.res)
    xytransform.res <- centre(xytransform.res)

    # yz align
    setWinProgressBar(pb, 5, label = "Aligning YZ...")
    yzalign.res <- yzalign(xytransform.res)
    yzalign.res <- centre(yzalign.res)

    # xz transform
    setWinProgressBar(pb, 6, label = "Transforming XZ...")
    xztransform.res <- xztransform(yzalign.res)
    xztransform.res <- centre(xztransform.res)

    finish <- xztransform.res
  }

  #plotting graphs
  setWinProgressBar(pb, 7, label = "Transforming XZ...")
  plot(finish$x, finish$y, xlab = "x", ylab = "y", main = "finish xy", asp = 1)
  plot(finish$y, finish$z, xlab = "y", ylab = "z", main = "finish yz", asp = 1)
  plot(finish$x, finish$z, xlab = "x", ylab = "z", main = "finish xz", asp = 1)

  # saving as a txt file
  setWinProgressBar(pb, 8, label = "Writing to file...")
  fullfile <- paste(filename, "-results", ".xyz", sep = "")
  write.table(finish, file = fullfile, row.names = FALSE, col.names = FALSE)

  # closing progress bar
  setWinProgressBar(pb, 9)
  close(pb)

  # calculating change made
  changemade <- all.equal(finish, sample)

  # calculating run time
  endtime <- Sys.time()

  # returning results
  returnlist <- list("saved as" = fullfile, "runtime" = endtime - starttime, "loops" = l, "adjustment made" = changemade)
  print(returnlist)
}

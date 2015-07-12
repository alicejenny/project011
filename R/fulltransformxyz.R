#' Align Mandible
#'
#' For calling from batch().
#' @param sample The input data frame. 3 columns (x, y, and z, in that order).
#' @export

align2 <- function(sample, filename, folder, slice = TRUE) {
  library(stringr)
  require(openxlsx)
  # start time for calculating run time
  starttime <- Sys.time()
  mandiblename <- str_replace(filename, "VERT", ".")

  # progress bar
  pb <- winProgressBar(title = "Aligning...", label = "Initialising...", max = 12)

  #empty objects
  src <- nrow(sample)
  finish <- data.frame("x" = numeric(src), "y" = numeric(src), "z" = numeric(src))
  l <- 0
  stable <- FALSE

  # centering
  setWinProgressBar(pb, 1, label = paste("Centering", mandiblename))
  loopstart <- centre(sample)

  while (!stable){
    l <- l + 1

    stable <- isTRUE(all.equal(loopstart, finish, tolerance = 0.01))

    if (sum(finish) != 0){
      loopstart <- finish
    }

    # xy transform
    setWinProgressBar(pb, 2, label = paste("Transforming XY for", mandiblename))
    xytransform.res <- xytransform(loopstart)
    xytransform.res <- centre(xytransform.res)

    # yz transform
    setWinProgressBar(pb, 3, label = paste("Transforming YZ for", mandiblename))
    yztransform.res <- yztransform(xytransform.res)
    yztransform.res <- centre(yztransform.res)

    # xy transform again
    setWinProgressBar(pb, 4, label = paste("Transforming XY again for", mandiblename))
    xytransform.res <- xytransform(yztransform.res)
    xytransform.res <- centre(xytransform.res)

    # yz align
    setWinProgressBar(pb, 6, label = paste("Aligning YZ for", mandiblename))
    yzalign.res <- yzalign(xytransform.res)
    yzalign.res <- centre(yzalign.res)

    # xz transform
    setWinProgressBar(pb, 7, label = paste("Transforming XZ for", mandiblename))
    xztransform.res <- xztransform(yzalign.res)
    xztransform.res <- centre(xztransform.res)

    finish <- xztransform.res

    if (l == 200){
      breakmsg <- paste("Maximum loops reached. Aligning process terminated for mandible", str_replace(filename, "VERT", "."))
      message(breakmsg)
      break
    }
  }

  # rotating so chin is down
  setWinProgressBar(pb, 8, label = paste("Checking orientation for", mandiblename))
  xmax.y <- finish$y[which.max(finish$x)]
  if (xmax.y < 0){
    finish <- data.frame("x" = (finish$x * cos(pi)) - (finish$y * sin(pi)), "y" = (finish$x * sin(pi)) + (finish$y * cos(pi)), "z" = finish$z)
  }

  # initial plots
  par(mfrow=c(2,3))
  setWinProgressBar(pb, 10, label = paste("Plotting graphs for", mandiblename))
  plot(sample$x, sample$y, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "start xy", sep = " "), asp = 1)
  plot(sample$y, sample$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "start yz", sep = " "), asp = 1)
  plot(sample$x, sample$z, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "start xz", sep = " "), asp = 1)

  # finished plots
  plot(finish$x, finish$y, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "finish xy", sep = " "), asp = 1)
  plot(finish$y, finish$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "finish yz", sep = " "), asp = 1)
  edgelength(finish$y, finish$z)
  plot(finish$x, finish$z, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "finish xz", sep = " "), asp = 1)

  # slicing
  if (slice == TRUE){
    par(mfrow=c(3,3))
    setWinProgressBar(pb, 9, label = paste("Slicing", mandiblename))
    baseslice(finish, filename, folder)
    gonialarea(finish, filename, folder)
    ramusslice(finish, filename, folder)
    menemslice(finish, filename, folder)
  }

  #returning to global environment
  globname <- str_replace(filename, "VERT", ".al")
  assign(globname, finish, envir = .GlobalEnv)

  # saving as an xyz file
  setWinProgressBar(pb, 11, label = paste("Writing", mandiblename, "to file"))
  fullfile <- paste(str_replace(filename, "VERT", "-aligned"), ".xyz", sep = "")
  fileandpath <- paste(folder, fullfile, sep = "//")
  write.table(finish, fileandpath, row.names = FALSE, col.names = FALSE)

  # closing progress bar
  setWinProgressBar(pb, 12)
  close(pb)

  # calculating change made
  changemade <- all.equal(finish, sample)

  # calculating run time
  endtime <- Sys.time()
  runtime <- round(difftime(endtime, starttime, units = "mins"))
  if (runtime < 2){
    minunit <- "minute"
  }
  if (runtime >= 2) {
    minunit <- "minutes"
  }

  # returning results
  returnlist <- data.frame("saved.as" = fullfile, "runtime" = runtime, "loops" = l, "x.diff" = as.integer(unlist(strsplit(changemade[1], " "))[6]), "y.diff" = as.integer(unlist(strsplit(changemade[2], " "))[6]), "z.diff" = as.integer(unlist(strsplit(changemade[3], " "))[6]))
  assign("returnlist", returnlist, envir = parent.frame())

  msg <- paste("Finished mandible ", str_replace(filename, "VERT", ""))
  message(msg)
}

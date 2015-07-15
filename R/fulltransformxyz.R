#' Align Mandible
#'
#' For calling from batch().
#' @param sample The input data frame. 3 columns (x, y, and z, in that order).
#' @param filename A string representing the generic filename, e.g. "m01178MXVERT".
#' @param folder The folder in which the processed files will be saved.
#' @param slice Logical value. If TRUE, the aligned mandible will be further processed to isolate specific areas (the base, gonial areas, rami, and mental eminence).
#' @param saveplots Logical value. If TRUE, plots will be cleared from the console and saved as .png files.
#' @export

align2 <- function(sample, filename, folder, slice = TRUE, saveplots = TRUE) {
  library(stringr)
  require(openxlsx)
  # start time for calculating run time
  starttime <- Sys.time()
  mandiblename <- str_replace(filename, "VERT", "")
  msg <- paste("Starting mandible", mandiblename)
  message(msg)

  # progress bar
  pb <- winProgressBar(title = "Aligning...", label = "Initialising...", max = 8)

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

  # user input for yz rotation
  close(pb)
  plot.new()
  par(mfrow=c(1,1))
  plot(finish$y, finish$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "yz rotation test", sep = " "), asp = 1)
  edgelength(finish$y, finish$z)

  n <- (as.numeric(readline("Rotation (degrees): ")) / 180)

  dev.off()

  if (n != 0){
    rot <- n * pi
    finish <- data.frame("x" = finish$x, "y" = (finish$y * cos(rot)) - (finish$z * sin(rot)), "z" = (finish$y * sin(rot)) + (finish$z * cos(rot)))

    pb <- winProgressBar(title = "Aligning again...", label = "Initialising...", max = 4)

    dev.new()
    par(mfrow=c(1,1))

    # running align again
    # xy transform again
    setWinProgressBar(pb, 1, label = paste("Transforming XY for", mandiblename))
    xytransform.res <- xytransform(finish)
    xytransform.res <- centre(xytransform.res)
    plot(xytransform.res$y, xytransform.res$z, xlab = "y", ylab = "z", main = "xy", asp = 1)
    edgelength(xytransform.res$y, xytransform.res$z)

    # yz align
    setWinProgressBar(pb, 2, label = paste("Aligning YZ for", mandiblename))
    yzalign.res <- yzalign(xytransform.res)
    yzalign.res <- centre(yzalign.res)
    plot(yzalign.res$y, yzalign.res$z, xlab = "y", ylab = "z", main = "yz", asp = 1)
    edgelength(yzalign.res$y, yzalign.res$z)

    # xz transform
    setWinProgressBar(pb, 3, label = paste("Transforming XZ for", mandiblename))
    xztransform.res <- xztransform(yzalign.res)
    xztransform.res <- centre(xztransform.res)

    finish <- xztransform.res

    setWinProgressBar(pb, 4, label = paste("Checking orientation for", mandiblename))
    xmax.y <- finish$y[which.max(finish$x)]
    if (xmax.y < 0){
      finish <- data.frame("x" = (finish$x * cos(pi)) - (finish$y * sin(pi)), "y" = (finish$x * sin(pi)) + (finish$y * cos(pi)), "z" = finish$z)
    }

    close(pb)

    # okay to continue?
    ok <- readline("Okay to continue? (Y/N): ")
    if (ok == "N"){
      break
    }
    dev.off()
  }

  pb <- winProgressBar(title = "Finishing up...", label = "Initialising...", max = 4)

  # plot save info
  plotpath <- paste(folder, "plots\\", sep = "\\")
  if (dir.exists(plotpath) == FALSE){
    dir.create(plotpath)
  }

  # initial plots
  plot.new()
  if (saveplots == TRUE){
    png(filename = paste(plotpath, mandiblename, "-aligned", ".png", sep = ""), width = 1000, height = 1000)
  }
  par(mfrow=c(2,3))
  setWinProgressBar(pb, 1, label = paste("Plotting graphs for", mandiblename))
  plot(sample$x, sample$y, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "start xy", sep = " "), asp = 1)
  plot(sample$y, sample$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "start yz", sep = " "), asp = 1)
  plot(sample$x, sample$z, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "start xz", sep = " "), asp = 1)

  # finished plots
  plot(finish$x, finish$y, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "finish xy", sep = " "), asp = 1)
  plot(finish$y, finish$z, xlab = "y", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "finish yz", sep = " "), asp = 1)
  edgelength(finish$y, finish$z)
  plot(finish$x, finish$z, xlab = "x", ylab = "z", main = paste(str_replace(filename, "VERT", ""), "finish xz", sep = " "), asp = 1)
  if (saveplots == TRUE){
    dev.off()
  }

  #returning to global environment
  globname <- str_replace(filename, "VERT", ".al")
  assign(globname, finish, envir = .GlobalEnv)

  # slicing
  if (slice == TRUE){
    setWinProgressBar(pb, 2, label = paste("Slicing", mandiblename))

    # base
    if (saveplots == TRUE){
      png(filename = paste(plotpath, mandiblename, "-base", ".png", sep = ""), width = 1000, height = 1000)
    }
    plot.new()
    par(mfrow=c(1,3))
    baseslice(finish, filename, folder)
    if (saveplots == TRUE){
      dev.off()
    }

    # gonial areas
    if (saveplots == TRUE){
      png(filename = paste(plotpath, mandiblename, "-gonialarea", ".png", sep = ""), width = 1000, height = 1000)
    }
    plot.new()
    par(mfrow=c(2,3))
    gonialarea(finish, filename, folder)
    if (saveplots == TRUE){
      dev.off()
    }

    # rami
    if (saveplots == TRUE){
      png(filename = paste(plotpath, mandiblename, "-rami", ".png", sep = ""), width = 1000, height = 1000)
    }
    plot.new()
    par(mfrow=c(2,3))
    ramusslice(finish, filename, folder)
    if (saveplots == TRUE){
      dev.off()
    }

    # mental eminence
    close(pb)
    menemslicept1(finish, filename)
    topfiveedges(menempt1$x, menempt1$y)
    teeth.removed <- "R"
    while(teeth.removed == "R"){
      plot.new()
      par(mfrow=c(1,1))
      plot(menempt1$x, menempt1$y, asp = 1, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
      points(topfive, col = "red", pch = 16)
      text(topfive, labels = c(1:nrow(topfive)), pos = 2, col = "blue")
      cutpoint <- as.integer(readline("Above which point should be discarded? (0 for none): "))
      if (cutpoint != 0){
        menem.noteeth <- subset(menempt1, y < topfive$y[cutpoint])
        plot(menem.noteeth$x, menem.noteeth$y, asp = 1, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
        teeth.removed <- readline("Okay to continue? (Y [yes]/R [restart]/A [again]: ")
      }
      if (cutpoint == 0){
        teeth.removed <- "Y"
      }
      dev.off()
    }
    while (teeth.removed == "A"){
      teeth.removed <- "R"
      topfiveedges(menem.noteeth$x, menem.noteeth$y)
      while(teeth.removed == "R"){
        plot.new()
        par(mfrow=c(1,1))
        plot(menem.noteeth$x, menem.noteeth$y, asp = 1, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
        points(topfive, col = "red", pch = 16)
        text(topfive, labels = c(1:nrow(topfive)), pos = 2, col = "blue")
        cutpoint <- as.integer(readline("Above which point should be discarded? (0 for none): "))
        if (cutpoint != 0){
          menem.noteeth <- subset(menem.noteeth, y < topfive$y[cutpoint])
          plot(menem.noteeth$x, menem.noteeth$y, asp = 1, xlab = "x", ylab = "y", main = paste(str_replace(filename, "VERT", ""), "mental eminence", sep = " "))
          teeth.removed <- readline("Okay to continue? (Y [yes]/R [restart]/A [again]): ")
        }
        if (cutpoint == 0){
          teeth.removed <- "Y"
        }
        dev.off()
      }
    }
    if (saveplots == TRUE){
      png(filename = paste(plotpath, mandiblename, "-menem", ".png", sep = ""), width = 1000, height = 1000)
    }
    plot.new()
    par(mfrow=c(1,3))
    menemslicept2(menempt1, filename, folder)
    if (saveplots == TRUE){
      dev.off()
    }
    pb <- winProgressBar(title = "Finishing up...", label = "Initialising...", max = 4, initial = 3)
  }

  # saving as an xyz file
  setWinProgressBar(pb, 3, label = paste("Writing", mandiblename, "to file"))
  fullfile <- paste(str_replace(filename, "VERT", "-aligned"), ".xyz", sep = "")
  fileandpath <- paste(folder, fullfile, sep = "//")
  write.table(finish, fileandpath, row.names = FALSE, col.names = FALSE)

  # closing progress bar
  setWinProgressBar(pb, 4)
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

  msg <- paste("Finished mandible", mandiblename)
  message(msg)
}

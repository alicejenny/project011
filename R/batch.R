#' Batch import and align
#'
#' Calls align2 (and thus various other functions) in order to import and align a series of.xyz files. Files must be 6 columns until I have the time to change it.
#' Will be updated later to include other functions/make it run a bit better.
#' No variables needed (see example).
#' @export
#' @examples batch()

batch <- function(align = TRUE, slice = TRUE){
  if (align == TRUE){
    slice <- FALSE
  }
# Selecting the files
  file.filter <- matrix(c("xyz files", "*.xyz"), ncol = 2, byrow = TRUE)
  filelist <- choose.files(caption = "Select the files to import", filters = file.filter)
  nfiles <- length(filelist)
  startTime <- Sys.time()
  startmsg <- paste("WARNING: This could take a while. There are", nfiles, "files to process.", sep = " ")
  cat(startmsg)

# Empty objects
  errorlist <- c()
  vertenv <- new.env()

# Filters for file import
  vertc <- c(rep(NA, 3), rep("NULL", 3))

# Importing files
  pb <- winProgressBar(title = "Importing...", max = nfiles)
  time1 <- Sys.time()
  for (i in 1:nfiles){
    if (i == 1){
      time2 <- as.POSIXct(60, origin = Sys.time())
    }
    runTime <- difftime(time2, time1, units = "mins")
    eachTime <- runTime/i
    remfiles <- nfiles - i
    eta <- eachTime * remfiles
    pblab <- paste("Importing ", i, " of ", nfiles, ".", " Estimated time remaining: ", eta, " minutes.")
    setWinProgressBar(pb, i, label = pblab)
    noext <- sapply(strsplit(basename(filelist[i]), "\\."), function(x) paste(x[1:(length(x)-1)]))
    objname <- paste("m", gsub("[^[:alnum:]]", "", noext), sep = "")
    vertname <- paste(objname, "VERT", sep = "")

# Files are only imported if they have 6 columns.
    if ((ncol(read.table(filelist[i]))) == 6){

# For files with no header
      if (class(read.table(filelist[i], header = FALSE)[1,1]) == "numeric"){
        assign(objname, read.table(filelist[i], header = FALSE, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = .GlobalEnv)

# If models are to be aligned
        if (align == TRUE){
          assign(vertname, read.table(filelist[i], header = FALSE, colClasses = vertc, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = vertenv)
        }
      }

# For files with a header
      else {
        assign(objname, read.table(filelist[i], header = TRUE, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = .GlobalEnv)

# If models are to be aligned
        if (align == TRUE){
          assign(vertname, read.table(filelist[i], header = TRUE, colClasses = vertc, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = vertenv)
        }
      }
    }

# If it doesn't have 6 columns, it won't be read.
    else {
      error <- basename(filelist[i])
      errorlist <- c(errorlist, error)
    }
  time2 <- Sys.time()
  }

  close(pb)
  cat("\n", "File import complete.")

# Printing the error list
  if (length(errorlist) != 0){
    print(data.frame("The following files were not imported:" = errorlist))
  }

  if (align == TRUE){

    savedir <- choose.dir(default = getwd(), caption = "Select Save Folder")
    impfiles <- ls(vertenv)
    resFrame <- data.frame("saved.as" = numeric(length(impfiles)), "runtime" = numeric(length(impfiles)), "loops" = numeric(length(impfiles)), "x.diff" = numeric(length(impfiles)), "y.diff" = numeric(length(impfiles)), "z.diff" = numeric(length(impfiles)))

    for (i in 1:length(impfiles)){
      obj <- get(impfiles[i], envir = vertenv)
      fn <- impfiles[i]
      align2(sample = obj, filename = fn, folder = savedir)
      resFrame$saved.as[i] <- as.character(returnlist$saved.as[1])
      resFrame$runtime[i] <- as.character(returnlist$runtime[1])
      resFrame$loops[i] <- as.integer(returnlist$loops[1])
      resFrame$x.diff[i] <- as.integer(returnlist$x.diff[1])
      resFrame$y.diff[i] <- as.integer(returnlist$y.diff[1])
      resFrame$z.diff[i] <- as.integer(returnlist$z.diff[1])
      if (slice == TRUE){
        baseslice(obj, fn, savedir)
        lrflip(obj, fn, savedir)
        ramusslice(obj, fn, savedir)
      }
    }

    print(resFrame)
  }

  endTime <- Sys.time()
  cat(paste("The process took about", round(difftime(endTime, startTime, units = "mins")), "minute(s).", sep = " "))
}

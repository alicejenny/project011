#' Batch import and align
#'
#' Calls align2 (and thus various other functions) in order to import and align a series of.xyz files. Files must be 3 columns (x, y, z) with no normals.
#' Will be updated later to include other functions/make it run a bit better.
#' No variables needed (see example).
#' @export
#' @examples batch()

batch <- function(){
  file.filter <- matrix(c("xyz files", "*.xyz"), ncol = 2, byrow = TRUE)
  filelist <- choose.files(caption = "Select the files to import", filters = file.filter)
  nfiles <- length(filelist)
  errorlist <- c()
  tempenv <- new.env()
  for (i in 1:nfiles){
    noext <- sapply(strsplit(basename(filelist[i]), "\\."), function(x) paste(x[1:(length(x)-1)]))
    objname <- gsub("[^[:alnum:]]", "", noext)
    if ((ncol(read.table(filelist[i]))) == 3){
      if (class(read.table(filelist[i], header = FALSE)[1,1]) == "numeric"){
        assign(objname, read.table(filelist[i], header = FALSE, col.names = c("x", "y", "z")), envir = tempenv)
      }
      else {
        assign(objname, read.table(filelist[i], header = TRUE, col.names = c("x", "y", "z")), envir = tempenv)
      }
    }
    else {
      error <- basename(filelist[i])
      errorlist <- c(errorlist, error)
    }
  }

  if (length(errorlist) != 0){
    print(data.frame("The following files were not imported:" = errorlist))
  }
  impfiles <- ls(tempenv)
  for (i in 1:length(impfiles)){
    fn <- as.character(substitute(impfiles[i]))
    obj <-  get(impfiles[i], envir = tempenv)
    align2(obj, fn[2])
  }
}

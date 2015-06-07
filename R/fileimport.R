#' Import xyz and txt files
#' 
#' Imports xyz and txt files. Must be six columns: coordinates (x, y, z) and vertex normals (x, y, z).
#' @export

importfiles <- function(){
  file.filter <- matrix(c("(preferred) xyz files", "*.xyz", "(not preferred) txt files", "*txt"), ncol = 2, byrow = TRUE)
  filelist <- choose.files(caption = "Select the files to import", filters = file.filter)
  nfiles <- length(filelist)
  errorlist <- c()
  for (i in 1:nfiles){
    if ((ncol(read.table(filelist[i]))) == 6){
      if (class(read.table(filelist[i], header = FALSE)[1,1]) == "numeric"){
        assign(make.names(basename(filelist[i])), read.table(filelist[i], header = FALSE, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = .GlobalEnv)
      }
      assign(make.names(basename(filelist[i])), read.table(filelist[i], header = TRUE, col.names = c("x", "y", "z", "xn", "yn", "zn")), envir = .GlobalEnv)
    }
    else {
      error <- basename(filelist[i])
      errorlist <- c(errorlist, error)
    }
  }
  if (length(errorlist) != 0){
    data.frame("The following files were not imported:" = errorlist)
  }
}

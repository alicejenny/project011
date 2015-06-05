#' Cull Backfaces
#'
#' Cull the backfaces of a point cloud based on vertex normals.
#' @export
#' @examples
#' cull.backfaces()
#'
cull.backfaces <- function(){
  sample <- read.table(file.choose())
  znorm.pos <- sample$V6 >= 0
  src <- nrow(sample)
  rem <- sum(znorm.pos)
  points <- data.frame("x" = numeric(rem), "y" = numeric(rem), "z" = numeric(rem))
  r <- 1
  pb <- winProgressBar(title = "Culling Backfaces...", label = "Initialising...", max = rem)
  pblab <- paste(0, " vertices culled so far ", "(", src - rem, " to go)", sep = "")
  for (i in 1:src){
    if (sample$V6[i] >= 0){
      points$x[r] <- sample$V1[i]
      points$y[r] <- sample$V2[i]
      points$z[r] <- sample$V3[i]
      r <- r + 1
      pblab <- paste(i - r, " vertices culled so far ", "(", (src - rem) - (i - r), " to go)", sep = "")
    }
    setWinProgressBar(pb, value = r, label = pblab)
  }

  close(pb)

  points

}

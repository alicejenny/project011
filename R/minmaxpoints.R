minmaxpoints <- function(x, y){
  start <- Sys.time()
  frame <- data.frame("x" = x, "y" = y)
  par(mfrow = c(1,1), bg = "transparent")
  plot(frame, xlab = "", ylab = "", asp = 1, axes = FALSE)
  xmin <- frame[which.min(frame$x),]
  points(xmin, col = "red", pch = 16)
  text(xmin, labels = "xmin", col = "red", pos = 4)
  xmax <- frame[which.max(frame$x),]
  points(xmax, col = "green", pch = 16)
  text(xmax, labels = "xmax", col = "green", pos = 2)
  ymin <- frame[which.min(frame$y),]
  points(ymin, col = "blue", pch = 16)
  text(ymin, labels = "ymin", col = "blue", pos = 3)
  ymax <- frame[which.max(frame$y),]
  points(ymax, col = "purple", pch = 16)
  text(ymax, labels = "ymax", col = "purple", pos = 1)
  end <- Sys.time()
  end - start
}

cwangle <- function(fixed, candidates){
  plot(candidates, col = "yellow")
  fx <- fixed[1,1]
  fy <- fixed[1,2]
  for (i in 1:nrow(candidates)){
    cx <- candidates$x[i]
    cy <- candidates$y[i]
    rad <- abs(atan2(cy,cx) - atan2(fy,fx))
    deg <-  rad * (180/pi)
    if (((fx*cy) - (fy*cx) > 0) & deg < 180){
      deg <- 360 - deg
    }
    if (((fx*cy) - (fy*cx) <= 0) & deg > 180){
      deg <- 360 - deg
    }
    text(cx, cy, round(deg), cex = 0.7)
  }
}

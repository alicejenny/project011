intersects <- function(a, b, countshared = FALSE, plot = FALSE){
  if (plot == TRUE){
    fivebyfive <- data.frame("x" = c(rep(0,6), rep(1,6), rep(2,6), rep(3,6), rep(4,6), rep(5,6)), "y" = c(rep(0:5,6)))
    plot(fivebyfive, asp = 1)
    lines(a, col = "red")
    lines(b, col = "green")
  }
  a.1.x <- a[1,1]
  a.1.y <- a[1,2]
  a.2.x <- a[2,1]
  a.2.y <- a[2,2]
  b.1.x <- b[1,1]
  b.1.y <- b[1,2]
  b.2.x <- b[2,1]
  b.2.y <- b[2,2]

  one <- ((a.1.x <= b.1.x) & (a.2.x >= b.1.x)) | ((a.1.x >= b.1.x) & (a.2.x <= b.1.x))
  two <- ((b.1.x <= a.1.x) & (b.2.x >= a.1.x)) | ((b.1.x >= a.1.x) & (b.2.x <= a.1.x))
  three <- ((a.1.x <= b.2.x) & (a.2.x >= b.2.x)) | ((a.1.x >= b.2.x) & (a.2.x <= b.2.x))
  four <- ((b.1.x <= a.2.x) & (b.2.x >= a.2.x)) | ((b.1.x >= a.2.x) & (b.2.x <= a.2.x))
  five <- ((a.1.y <= b.1.y) & (a.2.y >= b.1.y)) | ((a.1.y >= b.1.y) & (a.2.y <= b.1.y))
  six <- ((b.1.y <= a.1.y) & (b.2.y >= a.1.y)) | ((b.1.y >= a.1.y) & (b.2.y <= a.1.y))
  seven <- ((a.1.y <= b.2.y) & (a.2.y >= b.2.y)) | ((a.1.y >= b.2.y) & (a.2.y <= b.2.y))
  eight <- ((b.1.y <= a.2.y) & (b.2.y >= a.2.y)) | ((b.1.y >= a.2.y) & (b.2.y <= a.2.y))

  nine <- one | two
  ten <- three | four
  eleven <- five | six
  twelve <- seven | eight

  truecount <- sum(c(nine, ten, eleven, twelve))

  if (countshared == FALSE){
    a1.b1 <- identical(a[1,1], b[1,1]) & identical(a[1,2], b[1,2])
    a1.b2 <- identical(a[1,1], b[2,1]) & identical(a[1,2], b[2,2])
    a2.b1 <- identical(a[2,1], b[1,1]) & identical(a[2,2], b[1,2])
    a2.b2 <- identical(a[2,1], b[2,1]) & identical(a[2,2], b[2,2])
    if (sum(c(a1.b1, a1.b2, a2.b1, a2.b2)) > 0){
      return(FALSE)
    }
    else {
      if (truecount == 4) {
        return(TRUE)
      }

      else {
        return(FALSE)
      }
    }
  }

  else {
    if (truecount == 4) {
      return(TRUE)
    }

    else {
      return(FALSE)
    }
  }
}

#' Find Minimum Bounding Box
#' 
#' Find the minimum bounding box of a set of points defined by a 2 column matrix. Adapted from code by Daniel Wollschlaeger:
#' https://github.com/dwoll/RExRepos/blob/master/content/posts/diagBounding.md
#' @param xy The 2 column matrix.
#' @keywords bounding
#' @export
#' @examples
#' getMinBBox()

getMinBBox <- function(xy) {
  stopifnot(is.matrix(xy), is.numeric(xy), nrow(xy) >= 2, ncol(xy) == 2)
  
  ## rotating calipers algorithm using the convex hull
  H    <- chull(xy)                    # hull indices, vertices ordered clockwise
  n    <- length(H)                    # number of hull vertices
  hull <- xy[H, ]                      # hull vertices
  
  ## unit basis vectors for all subspaces spanned by the hull edges
  hDir  <- diff(rbind(hull, hull[1,])) # account for circular hull vertices
  hLens <- sqrt(rowSums(hDir^2))       # length of basis vectors
  huDir <- diag(1/hLens) %*% hDir      # scaled to unit length
  
  ## unit basis vectors for the orthogonal subspaces
  ## rotation by 90 deg -> y' = x, x' = -y
  ouDir <- cbind(-huDir[ , 2], huDir[ , 1])
  
  ## project hull vertices on the subspaces spanned by the hull edges, and on
  ## the subspaces spanned by their orthogonal complements - in subspace coords
  projMat <- rbind(huDir, ouDir) %*% t(hull)
  
  ## range of projections and corresponding width/height of bounding rectangle
  rangeH  <- matrix(numeric(n*2), ncol=2)   # hull edge
  rangeO  <- matrix(numeric(n*2), ncol=2)   # orth subspace
  widths  <- numeric(n)
  heights <- numeric(n)
  for(i in seq(along=H)) {
    rangeH[i, ] <- range(projMat[  i, ])
    rangeO[i, ] <- range(projMat[n+i, ])  # orth subspace is in 2nd half
    widths[i]   <- abs(diff(rangeH[i, ]))
    heights[i]  <- abs(diff(rangeO[i, ]))
  }
  
  ## extreme projections for min-area rect in subspace coordinates
  eMin  <- which.min(widths*heights)   # hull edge leading to minimum-area
  hProj <- rbind(   rangeH[eMin, ], 0)
  oProj <- rbind(0, rangeO[eMin, ])
  
  ## move projections to rectangle corners
  hPts <- sweep(hProj, 1, oProj[ , 1], "+")
  oPts <- sweep(hProj, 1, oProj[ , 2], "+")
  
  ## corners in standard coordinates, rows = x,y, columns = corners
  ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
  basis <- cbind(huDir[eMin, ], ouDir[eMin, ])  # basis formed by hull edge and orth
  hCorn <- basis %*% hPts
  oCorn <- basis %*% oPts
  pts   <- t(cbind(hCorn, oCorn[ , c(2, 1)]))
  
  data.frame(pts)
}
#' A simple helper function to plot projected Hilbert curves
#'
#' Using a hilbert curve or a subset of it and the set of projection coordinates returned by \code{\link{hilbertProjection}},
#' plot the number of points matching a particular index as a colored dot chart
#'
#' @param hc the hilbert index returned by \code{\link{do.hilbert}}
#' @param proj the projected reference matrix returned by \code{\link{hilbertProjection}}
#' @param pch the point shape (see \code{\link{par}})
#' @param cex the value used for character expansion (see \code{\link{par}})
#' @param col that color on which the colorscale will be built (see \code{\link{colorRampPalette}})
#' @param fun the function used to transform the count data for easier visualization (defaults to \code{\link{log10}})
#' @param add if add is TRUE (the default) then the projected Hilbert curve is added to the previous plot
#'
#' @return a matrix with \code{target} columns, corresponding to
#' the projection of each Hilbert index to \code{target} dimensions
#' @details
#' Based on the maximum index and the targeted number of dimensions the number of target bins is computed and used
#' to generate a reference matrix and a reference index. The reference matrix is returned, ordered by the reference index.
#'
#' @example examples/example.projection.R
#'
#' @author Yann Abraham
#'
#' @importFrom grDevices blues9 colorRampPalette
#' @importFrom graphics par
#'
#' @export
plotHilbertProjection <- function(hc,proj,pch='.',cex=2,col=blues9,fun='log10',add=FALSE) {
  plot(proj,
       cex=cex,
       pch=pch,
       col=colorRampPalette(col)(24)[cut(get(fun)(table(hc)),breaks=24,labels=FALSE)],
       add=add)
}

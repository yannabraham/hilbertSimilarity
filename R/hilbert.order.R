#' Estimate the Hilbert order for a given matrix
#'
#' Estimate the Hilbert order, or the number of bins in each dimension, so that if the matrix was
#' random every row in the matrix would correspond to a single bin.
#'
#' @param mat the matrix for which to estimate the Hilbert order
#'
#' @details Assuming the matrix is fully random, there is no need to generate more voxels
#'            (the combination of bins over all dimensions) than there are rows in the matrix. The number can be
#'            derived from the following formula:
#'            \eqn{c^{d} < N}
#'            where $c$ is the number of bins, $d$ is the number of dimensions and $N$ is the total
#'            number of cells in the dataset. $c$ can be computed easily using the following formula:
#'            \eqn{c = \lfloor \sqrt[d]{N}}.
#'
#' @example examples/example.cut.R
#'
#' @author Yann Abraham
#' @export
hilbert.order <- function(mat) {
  round(nrow(mat)^(1/ncol(mat)),0)
}

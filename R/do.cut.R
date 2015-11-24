#' Apply Cuts to the Reference Matrix
#'
#' Apply cuts generated using the \code{\link{make.cut}} function to the reference matrix
#'
#' @param mat the matrix to cut
#' @param cuts a list of cuts generated using \code{\link{make.cut}}
#' @param type the type of cuts to use (use \code{combined} by default)
#' @details
#' The matrix can be cut using either the fixed cuts (\code{type='fixed'}), or the combined cuts (\code{type='combined'})
#' where the limits have been adjusted to match local minima and maxima.
#' @examples
#' # generate a random 3D matrix
#' mat <- matrix(rnorm(300),ncol=3)
#' dimnames(mat)[[2]] <- LETTERS[1:3]
#' # generate 2 bins with a minimum bin size of 5
#' cuts <- make.cut(mat,n=3,count.lim=5)
#' par(mfrow=n2mfrow(ncol(mat)),
#' mar=c(1,1,3,1))
#' ksink <- lapply(dimnames(mat)[[2]],function(cur.ch) {
#'    plot(cuts[[cur.ch]]$dens,
#'         main=paste('Fixed Cuts for',cur.ch))
#'    abline(v=cuts[[cur.ch]]$fixed,lty=2,col=2)
#'  }
#' )
#' # Generate the cuts
#' cut.mat <- do.cut(mat,cuts,type='fixed')
#' head(cut.mat)
#' @author Yann Abraham
#' @export
do.cut <- function(mat,cuts,type='combined') {
  types <- c('fixed','combined')
  if(is.na(pmatch(type,types))) {
    stop(paste(type,'is not a recognized cut type - valid values are',paste(types,sep='',collapse=', ')))
  } else {
    type <- types[pmatch(type,types)]
  }
  if(!all(names(cuts) %in% dimnames(mat)[[2]])) {
    stop('Some cuts are not found in the reference matrix')
  }
  cur.ch.cut <- lapply(names(cuts),function(cur.ch) {
      cut(mat[,cur.ch],
          cuts[[cur.ch]][[type]],
          include.lowest=T,
          labels=F)
    }
  )
  cur.ch.cut <- do.call('cbind',cur.ch.cut)
  cur.ch.cut <- cur.ch.cut-1
  return(cur.ch.cut)
}

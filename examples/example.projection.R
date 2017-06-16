# generate a random matrix
ncols <- 5
mat <- matrix(rnorm(ncols*100),ncol=ncols)
dimnames(mat)[[2]] <- LETTERS[seq(ncols)]
# generate 4 bins with a minimum bin size of 5
horder <- 4
cuts <- make.cut(mat,n=horder+1,count.lim=5)
# Generate the cuts and compute the Hilbert index
cut.mat <- do.cut(mat,cuts,type='fixed')
hc <- do.hilbert(cut.mat,horder)
# project the matrix to 2 dimensions
proj <- hilbertProjection(hc)
plotHilbertProjection(hc,proj)

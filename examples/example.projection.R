# generate a random matrix
ncols <- 5
mat <- matrix(rnorm(ncols*5000),ncol=ncols)
dimnames(mat)[[2]] <- LETTERS[seq(ncols)]

# generate 4 bins with a minimum bin size of 5
horder <- 4
cuts <- make.cut(mat,n=horder+1,count.lim=5)

# Generate the cuts and compute the Hilbert index
cut.mat <- do.cut(mat,cuts,type='fixed')
hc <- do.hilbert(cut.mat,horder)
chc <- table(hc)
idx <- as.numeric(names(chc))

# project the matrix to 2 dimensions
proj <- hilbertProjection(hc)

# visualize the result
img <- matrix(0,ncol=max(proj[,2])+1,nrow = max(proj[,1])+1)
img[proj[idx,]+1] <- chc
image(img)

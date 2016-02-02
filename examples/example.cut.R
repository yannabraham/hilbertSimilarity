# generate a random 3D matrix with 2 peaks
mat <- rbind(matrix(rnorm(300),ncol=3),
             matrix(rnorm(300,5,1),ncol=3))
dimnames(mat)[[2]] <- LETTERS[1:3]
# estimate the Hilbert order
hilbert.order(mat)
# generate 2 bins with a minimum bin size of 5
cuts <- make.cut(mat,n=3,count.lim=5)
show.cut(cuts)
# Generate the cuts
cut.mat <- do.cut(mat,cuts,type='fixed')
head(cut.mat)

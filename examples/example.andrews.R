# generate a random matrix
ncols <- 5
mat <- matrix(rnorm(ncols*1000),ncol=ncols)
dimnames(mat)[[2]] <- LETTERS[seq(ncols)]

# generate categories
conditions <- sample(letters[1:3],nrow(mat),replace = TRUE)
# generate 4 bins with a minimum bin size of 5
horder <- 4
cuts <- make.cut(mat,n=horder+1,count.lim=5)
# Generate the cuts and compute the Hilbert index
cut.mat <- do.cut(mat,cuts,type='fixed')
hc <- do.hilbert(cut.mat,horder)
# compute hilbert index per condition
condition.mat <- table(conditions,hc)
condition.pc <- apply(condition.mat,1,function(x) x/sum(x))
condition.pc <- t(condition.pc)
# project the matrix to the Andrews curve
av <- andrewsProjection(condition.pc)
proj <- condition.pc %*% t(av$freq)

plot(range(av$i),
     range(proj),
     type='n',
     xlab='Andrews index',
     ylab='Projection')
for(i in seq(nrow(proj))) {
    lines(av$i,
          proj[i,],
          col=i)
}
legend('bottomleft',
       legend=letters[1:3],
       col=seq(1,3),
       pch=16,
       bty='n')

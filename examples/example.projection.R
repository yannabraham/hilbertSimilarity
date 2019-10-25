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
data.frame(idx=hc) %>%
    count(idx) %>%
    mutate(x=proj[idx,1],
           y=proj[idx,2],
           pc=n/sum(n)) %>%
    ggplot(aes(x=x,y=y))+
    geom_tile(aes(fill=pc))+
    scale_fill_gradient(low='grey80',high='blue')+
    theme_light()

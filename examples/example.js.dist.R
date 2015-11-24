# generate 3 samples over 5 dimensions
# sample 1 and 2 are similar, sample 3 has an extra population
my.samples <- lapply(LETTERS[1:3],function(x) {
    # each sample has a different number of events
    cur.mat <- matrix(rnorm(5*floor(runif(1)*10000)),ncol=5)
    dimnames(cur.mat)[[2]] <- LETTERS[(length(LETTERS)-4):length(LETTERS)]
    return(cur.mat)
  }
)
names(my.samples) <- LETTERS[1:3]
# add an extra population to sample C
my.samples[['C']] <- rbind(my.samples[['C']],
                           matrix(rnorm(5*floor(runif(1)*100),2),ncol=5))
# assemble a sample matrix
my.samples.mat <- do.call('rbind',my.samples)
my.samples.id <- lapply(names(my.samples),
                        function(cur.spl) rep(cur.spl,nrow(my.samples[[cur.spl]])))
my.samples.id <- unlist(my.samples.id)

# cut it
my.cuts <- make.cut(my.samples.mat,n=5,count.lim=5)
my.samples.cut <- do.cut(my.samples.mat,my.cuts,type='fixed')

# compute the hilbert index
system.time(my.samples.index <- do.hilbert(my.samples.mat,horder=4))

# assemble a contingency table
my.samples.table <- table(my.samples.index,my.samples.id)
dim(my.samples.table)

heatmap(log10(my.samples.table+0.00001),
        col=colorRampPalette(c('white',blues9))(24),
        Rowv=NA,Colv=NA,
        scale='none')

# compute the Jensen-Shannon distance
my.samples.dist <- js.dist(my.samples.table)
my.samples.clust <- hclust(my.samples.dist)

plot(my.samples.clust)

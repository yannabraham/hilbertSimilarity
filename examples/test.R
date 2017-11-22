library(hilbertSimilarity)
library(bodenmiller)
library(entropy)
library(grid)

data(refPhenoMat)
data(untreatedPhenoMat)
data(refFuncMat)
data(untreatedFuncMat)
data(refAnnots)
refAnnots$Treatment <- 'reference'
data(untreatedAnnots)
fullAnnots <- rbind(refAnnots[,names(untreatedAnnots)],
                    untreatedAnnots)
fullAnnots$Treatment <- factor(fullAnnots$Treatment)
fullAnnots$Treatment <- relevel(fullAnnots$Treatment,'reference')
refMat <- cbind(refPhenoMat,refFuncMat)
untreatedMat <- cbind(untreatedPhenoMat,
                      untreatedFuncMat)
fullMat <- rbind(refMat,untreatedMat)
fullMat <- apply(fullMat,2,function(x) {
  qx <- quantile(x,c(0.005,0.995))
  x[x<qx[1]] <- qx[1]
  x[x>qx[2]] <- qx[2]
  return(x)
})

nbins <- 2
cuts <- make.cut(fullMat,
                 n=nbins+1,
                 count.lim=40)

cutFullMat <- do.cut(fullMat,cuts,type='combined')

miFullMat <- matrix(0,nrow=ncol(fullMat),ncol = ncol(fullMat) )
for (i in seq(ncol(fullMat)-1)) {
  for (j in seq(i+1,ncol(fullMat))) {
    cur.tbl <- table(cutFullMat[,i],cutFullMat[,j])
    nent <- 1-mi.empirical(cur.tbl)/entropy.empirical(cur.tbl)
    miFullMat[i,j] <- miFullMat[j,i] <- nent
  }
}
dimnames(miFullMat) <- list(colnames(fullMat),colnames(fullMat))
hcFullMat <- hclust(as.dist(miFullMat))

col.order <- hcFullMat$labels[rev(hcFullMat$order)]
hc <- do.hilbert(cutFullMat[,col.order],nbins)

# hc <- factor(hc)
# hc <- as.numeric(hc)

# test <- lapply(seq(1,10),function(i) {
#   cutFullMat[which(hc==i)[1],rev(col.order)]
# })
# test <- do.call(rbind,test)
# test # ok

target <- 2
targetb <- max(hc)^(1/target)
targetb <- log2(targetb)
(targetb <- 2^ceiling(targetb)) # make sure target b is a valid power of 2

ref <- lapply(seq(target),function(i) seq(targetb)-1)
ref <- as.matrix(expand.grid(ref))

phc <- do.hilbert(ref,targetb)
range(phc) # works!

test <- lapply(seq(1,10),function(i) {
  ref[which(phc==i)[1],]
})
test <- do.call(rbind,test)
test # ok

ref <- ref[order(phc),]
head(ref) # ok

plot(ref[sort(unique(hc)),],
     pch='.',
     cex=2,
     col=colorRampPalette(blues9)(24)[cut(log10(table(hc)),breaks=24,labels=F)])
points(ref[sort(unique(hc[fullAnnots$Cells=='cd4+'])),],pch='.',
       cex=2,
       col=colorRampPalette(c("#F7FBFF",'green'))(24)[cut(log10(table(hc)),breaks=24,labels=F)])
points(ref[sort(unique(hc[fullAnnots$Cells=='cd8+'])),],pch='.',
       cex=2,
       col=colorRampPalette(c("#F7FBFF",'red'))(24)[cut(log10(table(hc)),breaks=24,labels=F)])

plot(ref[sort(unique(hc[fullAnnots$Treatment=='reference'])),],pch='.',
     cex=2,
     col=colorRampPalette(c("#F7FBFF",'green'))(24)[cut(log10(table(hc)),breaks=24,labels=F)])
points(ref[sort(unique(hc[fullAnnots$Treatment=='PMA/Ionomycin'])),],pch='.',
       cex=2,
       col=colorRampPalette(c("#F7FBFF",'red'))(24)[cut(log10(table(hc)),breaks=24,labels=F)])

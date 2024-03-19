##-----------spearman------------------------------------
library(Hmisc)

data <- read.csv()

a <- NULL

cor <- rcorr(as.matrix(t(data)),type = "spearman") 
r <- data.frame(cor$r)
p <- data.frame(cor$P)

k<- nrow(data)-1
for (i in 1:k) {
    out <- as.data.frame(array(dim = c(nrow(data)-i,4)))
    names(out)<- c("a","b","rho","P")
    
    n <- nrow(data)-i
    out$a <- rownames(r[i,])
    out$b <- rownames(r[i+1:n,])
    out$rho <- r[i+1:n,i]
    out$P <- p[i+1:n,i]
    a <- rbind(a,out)
}

fdr<- a[order(a[,4]),]

#p adjust
FDR <- fdr[,4]
FDR <- data.frame(p.adjust(FDR,method = "BH")) 
colnames(FDR) <- "FDR"
fdr <- cbind(fdr,FDR)

write.csv(fdr,"Spearman_species-FDR.csv")

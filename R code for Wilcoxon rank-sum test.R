#read file
group <- read.csv()
pri <- read.csv()
pri$ID<- rownames(pri)

#merge two files and wilcox test
wilcox.p <- NULL

n1 <-"Low" 
n2 <-"High"

for (i in 1:(ncol(pri)-1)){
  mer <- merge(pri[,c(i,ncol(pri))], group, by = "ID")
  g1 <- mer[mer$group == n1, 2]
  g2 <- mer[mer$group == n2, 2]
  g1_not0 <- 680-sum(g1==0)  
  g2_not0 <- 681-sum(g2==0)
  
  wilcox.p$metabo[i] <- colnames(mer)[2]
  wilcox.p$Nor_not0[i] <- g1_not0 
  wilcox.p$Mal_not0[i] <- g2_not0
  wilcox.p$Nor_max[i] <- max(g1)
  wilcox.p$Nor_mean[i] <- sum(g1)/680
  wilcox.p$Nor_median[i] <- median(g1)
  
  wilcox.p$Mal_max[i] <- max(g2)
  wilcox.p$Mal_mean[i] <- sum(g2)/681
  wilcox.p$Mal_median[i] <- median(g2)
  
 
  wil <- wilcox.test(x = g1,y = g2)
  wilcox.p$P[i] <- wil$p.value
  wil <- wilcox.test(x = g1,y = g2,alternative="greater")
  if(wil$p.value < 0.05)
    wilcox.p$engroup[i] <- n1
  else
    wilcox.p$engroup[i] <- "equal"
  wil <- wilcox.test(x = g1,y = g2,alternative="less")
  if(wil$p.value < 0.05)
    wilcox.p$engroup[i] <- n2
}

#data.frame
p <- data.frame(wilcox.p)

wilcox.fdr<- p[order(p[,10]),]

#p adjust
FDR <- wilcox.fdr[,10]
FDR <- data.frame(p.adjust(FDR,method = "BH")) 
colnames(FDR) <- "FDR"
wilcox.fdr <- cbind(wilcox.fdr,FDR)

#save
write.csv(wilcox.fdr, "wilcox¡ªFDR.csv",row.names = F)

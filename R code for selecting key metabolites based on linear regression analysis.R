##linear regression
cc <- read.csv()  # data matrix of metabolomics data

cc$sex<-factor(cc$sex, levels = c(0,1)) 
cc$Smoking<-factor(cc$Smoking,levels = c(0,1))
cc$Dringking<-factor(cc$Dringking,levels = c(0,1))
cc$Hypertension<-factor(cc$Hypertension, levels = c(0,1))
cc$Diabetes <-factor(cc$Diabetes , levels = c(0,1))
cc$overweight<-factor(cc$overweight, levels = c(0,1))

#Metabolites associated with Clostridium_sp_CAG_299 
n <- c(12:ncol(cc))
a <- NULL  
for (i in n) {
  lm.reg<-lm(Clostridium_sp_CAG_299~cc[,i]+age+sex+smoking+drinking+diet_diversity+physical_activity+overweight+diabetes+hypertension,data=cc)    
  s<-summary(lm.reg)
  r <- as.data.frame(s[["coefficients"]])[2,]
  a <- rbind(a,r)
}
rownames(a)<-colnames(cc)[11:ncol(cc)]

p <- data.frame(a)
cc.fdr<- p[order(p[,4]),]
FDR <- cc.fdr[,4]
FDR <- data.frame(p.adjust(FDR,method = "BH"))
colnames(FDR) <- "FDR"
COU <- cbind(cc.fdr,FDR)
write.csv(COU,file ="Clostridium_sp_CAG_299-FDR.CSV" ,row.names = T)


#Metabolites associated with Eubacterium_rectale
n <- c(12:ncol(cc))
a <- NULL 
for (i in n) {
  lm.reg<-lm(Eubacterium_rectale~cc[,i]+age+sex+smoking+drinking+diet_diversity+physical_activity+overweight+diabetes+hypertension,data=cc)    
  s<-summary(lm.reg)
  r <- as.data.frame(s[["coefficients"]])[2,]
  a <- rbind(a,r)
}
rownames(a)<-colnames(cc)[11:ncol(cc)]

p <- data.frame(a)
cc.fdr<- p[order(p[,4]),]
FDR <- cc.fdr[,4]
FDR <- data.frame(p.adjust(FDR,method = "BH"))
colnames(FDR) <- "FDR"
COU <- cbind(cc.fdr,FDR)
write.csv(COU,file ="Eubacterium_rectale-FDR.CSV" ,row.names = T)
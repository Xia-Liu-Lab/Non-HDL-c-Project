##linear regression
cc <- read.csv()

cc$sex<-factor(cc$sex, levels = c(0,1)) 
cc$Smoking<-factor(cc$Smoking,levels = c(0,1))
cc$Dringking<-factor(cc$Dringking,levels = c(0,1))
cc$Hypertension<-factor(cc$Hypertension, levels = c(0,1))
cc$Diabetes <-factor(cc$Diabetes , levels = c(0,1))
cc$overweight<-factor(cc$overweight, levels = c(0,1))


n <- c(11:ncol(cc))
a <- NULL  #建一个空数据框
for (i in n) {
  
  lm.reg<-lm(Non_HDLC~cc[,i]+age+sex+Cigarettes+Drinking+MET+DD,data=cc)
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
write.csv(COU,file ="model2-FDR.CSV" ,row.names = T)

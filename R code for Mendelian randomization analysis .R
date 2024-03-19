library("MendelianRandomization")
outcome<- read.table()
exposure <- read.table()
exposure_adjusted_snp <- exposure[which(exposure$TEST=="ADD"),]
outcome_adjusted_snp <- outcome[which(outcome$TEST=="ADD"),]
exposure_adjusted_snp$se <- exposure_adjusted_snp$BETA/exposure_adjusted_snp$STAT
outcome_adjusted_snp$se <- outcome_adjusted_snp$BETA/outcome_adjusted_snp$STAT
exposure_adjusted_snp <- exposure_adjusted_snp[which(exposure_adjusted_snp$P<1e-05),]
exposure_adjusted_snp <- exposure_adjusted_snp[,c(2,7,10)]
colnames(exposure_adjusted_snp) <- c("SNP","microbec","microbese")
harmonized <- merge(exposure_adjusted_snp,outcome_adjusted_snp,by="SNP",all.x = T)
harmonized <- harmonized[,c(1:3,9,12)]
colnames(harmonized)[4:5] <- c("RCc","RCse")

MRInputObject <- mr_input(bx = harmonized$microbec,bxse= harmonized$microbese,by = harmonized$RCc,byse = harmonized$RCse) #指定输入文件

IVWObject1 <- mr_ivw(MRInputObject,model= "default",robust = FALSE,penalized = FALSE,correl = FALSE,weights ="simple", psi = 0,distribution = "normal",alpha = 0.05)
IVWObject1 <- mr_allmethods(MRInputObject)
IVWObject1

mr_plot(mr_allmethods(MRInputObject))

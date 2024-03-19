library(glmnet)
data <- read.csv()

x <- as.matrix(data[,1:23])
y <- data[,24]

alpha1_fit <- glmnet(x,y,alpha=1,family="gaussian")
plot(alpha1_fit,xvar="lambda",label=TRUE)

pdf("1A_lasso.pdf", width = 30, height = 15)
plot(fit, xvar = "dev", label = TRUE)
dev.off()

#cross-validation
cvfit = cv.glmnet(x, y, nfold=10, family = "gaussian", alpha = 1)

pdf("2cvfit.pdf")
plot(cvfit)
dev.off()
cvfit$lambda.min

myCoefs <- coef(cvfit, s="lambda.min")
lasso_fea <- myCoefs@Dimnames[[1]][which(myCoefs != 0 )]
(lasso_fea <- lasso_fea[-1])

write.csv(lasso_fea,"lasso.csv")

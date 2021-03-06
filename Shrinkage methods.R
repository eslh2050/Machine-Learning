# Shrinkage methods

library(faraway)

data(meatspec)
?meatspec
# training set = first 172 rows of data
model1 <- lm(fat ~ ., meatspec[1:172,])
summary(model1)$r.squared

# Function that calculates RMSE
rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse(model1$fit,meatspec$fat[1:172])

rmse(predict(model1,meatspec[173:215,]),meatspec$fat[173:215])

model2 <- step(model1)
rmse(model2$fit,meatspec$fat[1:172])
rmse(predict(model2,meatspec[173:215,]),meatspec$fat[173:215])

library(MVA)
meatpca <- prcomp(meatspec[1:172,-101])
round(meatpca$sdev,3)
plot(cumsum(round(meatpca$sdev,3)[1:10])/sum(round(meatpca$sdev,3))*100)

matplot(1:100,meatpca$rot[,1:3],type="l",xlab="Frequency",ylab="")

model3 <- lm(fat ~ meatpca$x[,1:4]  , meatspec[1:172,])
rmse(model3$fit,meatspec$fat[1:172])
plot(model1$coef[-1],ylab="Coefficient")
svb <- meatpca$rot[,1:4] %*% model3$coef[-1]
plot(svb,ylab="Coefficient")
plot(meatpca$sdev[1:10],type="l",ylab="SD of PC",xlab="PC number")
mm <- apply(meatspec[1:172,-101],2,mean)
tx <- as.matrix(sweep(meatspec[173:215,-101],2,mm))
nx <- tx %*%  meatpca$rot[,1:4]
pv <- cbind(1,nx) %*% model3$coef
rmse(pv,meatspec$fat[173:215])
rmsmeat <- numeric(50)
for(i in 1:50){
  nx <- tx %*%  meatpca$rot[,1:i]
  model3 <- lm(fat ~ meatpca$x[,1:i]  , meatspec[1:172,])
  pv <- cbind(1,nx) %*% model3$coef
  rmsmeat[i] <- rmse(pv,meatspec$fat[173:215])
}
plot(rmsmeat,ylab="Test RMS",xlab="No. of Components")
which.min(rmsmeat)
min(rmsmeat)
library(pls)
pcrmod <- pcr(fat ~ ., data=meatspec[1:172,], validation="CV",ncomp=50)
validationplot(pcrmod)
plsg <- plsr(fat ~ ., data=meatspec[1:172,], ncomp=50, validation="CV")
coefplot(plsg,ncomp=4,xlab="Frequency")
validationplot(plsg)
ypred <- predict(plsg,ncomp=14)
rmse(ypred,meatspec$fat[1:172])
ytpred <- predict(plsg,meatspec[173:215,],ncomp=14)
rmse(ytpred,meatspec$fat[173:215])
library(MASS)
yc <- meatspec$fat[1:172]-mean(meatspec$fat[1:172])
trainx <- as.matrix(sweep(meatspec[1:172,-101],2,mm))
gridge <- lm.ridge(yc ~ trainx,lambda = seq(0,5e-8,1e-9))
matplot(gridge$lambda,t(gridge$coef),type="l",lty=1,xlab=expression(lambda),ylab=expression(hat(beta)))
select(gridge)
abline(v=1.8e-8)
which.min(gridge$GCV)
ypredg <- scale(trainx,center=FALSE,scale=gridge$scales) %*% gridge$coef[,19] + mean(meatspec$fat[1:172])
rmse(ypredg,meatspec$fat[1:172])
testx <-  as.matrix(sweep(meatspec[173:215,-101],2,mm))
ytpredg <- scale(testx,center=FALSE,scale=gridge$scales) %*% gridge$coef[,19] + mean(meatspec$fat[1:172])
rmse(ytpredg,meatspec$fat[173:215])
c(ytpredg[13],ytpred[13],meatspec$fat[172+13])
rmse(ytpredg[-13],meatspec$fat[173:215][-13])
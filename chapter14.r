# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

# Possible solutions to the exercises in chapter 14

##################
# Exercise 14.3  #
##################

# 
X <- c(0,0,0,0,0,1,1,1)
T <- c(1,1,0,0,1,1,0,1)
Y <- c(0,1,0,1,0,1,0,0)

m1 <- glm(Y~T+X+T:X, family=binomial)
# Set T=1 und T=0
newdata1 <- as.data.frame(cbind(Y,T,X))
newdata1$T <- c(rep(1,8))
newdata0 <- as.data.frame(cbind(Y,T,X))
newdata0$T <- c(rep(0,8))
predict(m1, type="response", newdata=newdata1)
predict(m1, type="response", newdata=newdata0)
# MRR
mean(predict(m1, type="response", newdata=newdata1))/ # E(Y^1)
mean(predict(m1, type="response", newdata=newdata0))  # E(Y^0)

##################
# Exercise 14.4  #
##################

cdata <- read.csv("cattaneo.csv")
attach(cdata)

# model summary for a)
summary(lm(bweight ~ mbsmoke+fbaby, data=cdata))

# c) percentile bootstrap confidence intervals
set.seed(01112021)
library(boot)
smoke <- as.integer(mbsmoke=="smoker")
fb <- as.integer(fbaby=="Yes")

ATE.gformula <- function(d,i){
  boot.data = d[i,]
  Y <- boot.data$bweight; T <- boot.data$smoke; X <- boot.data$fb
  model <- lm(Y~T+X, data=boot.data)
  newdata1 <- as.data.frame(cbind(Y,T,X))
  newdata1$T <- c(rep(1,length(T)))
  newdata0 <- as.data.frame(cbind(Y,T,X))
    mean(predict(model,newdata=newdata1))- 
    mean(predict(model,newdata=newdata0))  
}

ATE.boot <- boot(data=as.data.frame(cbind(fb,smoke,bweight)),statistic=ATE.gformula,R=1000)
boot.ci(ATE.boot,type="perc")



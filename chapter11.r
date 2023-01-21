# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

# Possible solutions to the exercises in chapter 11

#############################
# 11.2 Deep sleep exercise  #
#############################

it <- c(0.3,2.2,0.5,0.7,1.0,1.8,3.0,0.2,2.3)
sleep <- c(5.8,4.4,6.5,5.8,5.6,5.0,4.8,6.0,6.1)
summary(lm(sleep~it))

plot(it,sleep)
abline(a=6.16,b=-0.45)

pdf(file="exercise_11.2_c.pdf")
par(mar= c(5, 5, 2, 2))
plot(it,sleep,xlab="internet time (in h)",ylab="deep sleep (in h)",cex=1.75,pch=19, cex.axis=1.75,cex.lab=1.75)
abline(a=6.16,b=-0.45,lwd=3)
dev.off()

################################
# 11.3 weight/height exercise  #
################################

weight <- c(68,58,53,60,59,60,55,62,58,53,53,50,64,77,60,63,69)
height <- c(174,164,164,165,170,168,167,166,160,160,163,157,168,179,170,168,170)
w2 <- c(55,75)
h2 <- c(175,150)
w3 <- c(weight,w2)
h3 <- c(height,h2)

plot(height,weight)
abline(coef=coefficients(lm(weight~height)))
lines(h2,w2, type="p",col="red")
abline(coef=coefficients(lm(w3~h3)),col="red",lty=2)

# fancy graph as in the book
pdf(file="exercise_11.3_d.pdf",width=10)
par(mar= c(5, 5, 2, 2))
plot(height,weight, pch=19, cex.axis=1.75,cex.lab=1.75,cex=1.5,xlab="Height",ylab="Weight",xlim=c(149,185))
abline(coef=coefficients(lm(weight~height)),lwd=3)
lines(h2,w2, type="p",pch=15, cex.axis=1.75,cex.lab=1.75,cex=1.5,col="darkgrey")
abline(coef=coefficients(lm(w3~h3)),lwd=3,col="darkgrey",lty=2)
text(175,55,expression(paste("(",x[18],",",y[18],")")),pos=4,cex=1.5,col="darkgrey")
text(150,75,expression(paste("(",x[19],",",y[19],")")),pos=4,cex=1.5,col="darkgrey")
legend("left",col=c("black","darkgrey"),lty=c(1,2),legend=c("with 17 obs.","with 19 obs."),lwd=3,cex=1.25,box.col = NULL)
dev.off()

#######################
# 11.4 hotel example  #
#######################

X <- c(-6,-5,2,4,7,15,17,19,13,9,4,0,10,10,14,17,22,24,26,27,22,19,14,12,1,0,5,9,14,20,23,24,21,14,9,4)
Y <- c(91,89,76,52,42,36,37,39,26,27,68,92,13,21,42,64,79,81,86,92,36,23,13,41,23,82,40,45,39,43,50,95,64,78,9,12)
city <- as.factor(c(rep("Davos",12),rep("Polenca",12),rep("Basel",12)))

summary(lm(Y~city))
anova(lm(Y~city))
summary(lm(Y~X))
summary(lm(Y~X+city))
summary(lm(Y~X*city))
vcov(lm(Y~X*city))
summary(lm(Y[1:12]~X[1:12]))
summary(lm(Y[13:24]~X[13:24]))
summary(lm(Y[25:36]~X[25:36]))

########################
# 11.5 theatre example #
########################

theatre <- read.csv("theatre.csv")
attach(theatre)

m1 <- lm(Theatre ~Age+Sex+Income+Culture+Theatre_ly)
summary(m1)
plot(m1) # ALL model diagnostics, not only those explained in the book

# For your interest: the graphs from Figure 11.11
plot(m1,which=2)
plot(fitted(m1),sqrt(abs(rstandard(m1))))

pdf(file="exercise_11.5_b1.pdf")
par(mar= c(5, 5, 2, 2))
plot(m1, qqline=F,which=2, cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,main="",caption = NULL,id.n=0,xlab="Theoretical Quantiles")
lines(c(seq(-4,4,0.1)),c(seq(-4,4,0.1)),lwd=5,lty="dotted")
dev.off()

pdf(file="exercise_11.5_b2.pdf")
par(mar= c(5, 5.5, 2, 2))
plot(fitted(m1),sqrt(abs(rstandard(m1))), cex.axis=1.75,cex.lab=1.75,cex.main=1.75,xlim=c(0,300),ylim=c(0,2.5),xlab="Fitted Values",ylab=expression(paste(sqrt("|Standardized residuals|"))))
dev.off()

# histograms for c)

par(mfrow=c(1,2)) # two graphs next to each other
hist(Theatre)
hist(log(Theatre))
par(mfrow=c(1,1)) # only one graph per page

pdf(file="exercise_11.5_c1.pdf")
par(mar= c(5, 5, 2, 2))
hist(Theatre, cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,xlab="expenditure for theatre visits (in SFR)",main="",col="lightgrey")
dev.off()

pdf(file="exercise_11.5_c2.pdf")
par(mar= c(5, 5, 2, 2))
hist(log(Theatre), cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,xlab="log-expenditure for theatre visits (in SFR)",xlim=c(3.0,6.5),ylim=c(0,125),main="",col="lightgrey")
dev.off()

# For your interest: the graphs from Figure 11.12
m2 <- lm(I(log(Theatre)) ~Age+Sex+Income+Culture+Theatre_ly)
summary(m2)

pdf(file="exercise_11.5_e1.pdf")
par(mar= c(5, 5, 2, 2))
plot(m2, qqline=F,which=2, cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,main="",caption = NULL,id.n=0,xlab="Theoretical Quantiles")
lines(c(seq(-4,4,0.1)),c(seq(-4,4,0.1)),lwd=5,lty="dotted")
dev.off()

pdf(file="exercise_11.5_e2.pdf")
par(mar= c(5, 5.5, 2, 2))
plot(fitted(m2),sqrt(abs(rstandard(m1))), cex.axis=1.75,cex.lab=1.75,cex.main=1.75,xlim=c(3.5,6.0),ylim=c(0,2.5),xlab="Fitted Values",ylab=expression(paste(sqrt("|Standardized residuals|"))))
dev.off()

library(concurve)
# Note: recently package "concurve" was unfortunately recently removed from CRAN
# you can download the older version here: https://cran.r-project.org/src/contrib/Archive/concurve/
# in this case you also need to install the dependencies of the package
# Easier: use the explanation here: https://github.com/zadrafi/concurve and install with package 'remotes':
# remotes::install_github("zadrafi/concurve@master", dependencies = TRUE)
s_fun <- curve_gen(m2,'Theatre_ly')
pdf("exercise_11.5_f.pdf",width=9)
ggcurve(s_fun[[1]],type="s",  levels = c(0.95), nullvalue=0, title = "", subtitle = "")
dev.off()

detach(theatre)

###################
# 11.6 Pizza data #
###################

pizza <- read.csv("pizza_delivery.csv")
attach(pizza)

# a)
mp <- lm(time ~ temperature + branch + day + operator + driver + bill + pizzas + discount_customer)
summary(mp)

# b)
lcl <- coefficients(mp) -  qt(0.975,1248)*sqrt(diag(vcov(mp)))
ucl <- coefficients(mp) +  qt(0.975,1248)*sqrt(diag(vcov(mp)))
cbind(coefficients(mp),lcl,ucl)

# c)
sum(residuals(mp)^2)/(mp$df.residual)

# d)
SQE <- sum(residuals(mp)^2)
SQT <- sum((time-mean(time))^2)
1-(SQE/SQT)
1-((SQE/1248)/(SQT/1265))

# e)
library(MASS)
stepAIC(mp, direction="back")

# f)
mps <- lm(time ~ temperature + branch + day + driver + bill + pizzas)
summary(mps)

# g)

plot(mps, which=2)
plot(mps, which=3)


# g) by hand to make it nicer
pdf(file="exercise_11.6_g1.pdf")
par(mar= c(5, 5, 2, 2))
plot(mps, qqline=F,which=2, cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,main="",caption = NULL,id.n=0,xlab="Theoretical Quantiles")
lines(c(seq(-4,4,0.1)),c(seq(-4,4,0.1)),lwd=5,lty="dotted")
dev.off()

pdf(file="exercise_11.6_g2.pdf")
par(mar= c(5, 5.5, 2, 2))
plot(fitted(mps),sqrt(abs(rstandard(mps))), cex.axis=1.75,cex.lab=1.75,cex.main=1.75,xlim=c(20,45),ylim=c(0,2),xlab="Fitted Values",ylab=expression(paste(sqrt("|Standardized residuals|"))))
dev.off()

# i)
mps2 <- lm(time ~ temperature + I(temperature^2)  + branch + day + driver + bill + pizzas)
summary(mps2)

# j)
predict(mps,pizza[1266,])

#
detach(pizza)





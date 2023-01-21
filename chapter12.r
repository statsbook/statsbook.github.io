# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

# Possible solutions to the exercises in chapter 12

#################
# Exercise 12.1 #
#################

# a)
odds.ratio <- 80*60/(20*40)
odds.ratio

# b)
# Solution 1
# The glm function with family binomial can also handle successes and failures directly
#  as covariates
treatment <- c('B', 'A')
df <- data.frame( rbind(c(80,20), c(40,60)),  treatment)
colnames(df) <- c("y1", "y0", "treatment")

m.logit.1 <- glm( cbind(y1,y0)~treatment,  family=binomial, data=df)
summary(m.logit.1)

# Solution 2: expand the data by using weights
df <- data.frame( rbind( cbind(1,'B',80), cbind(0,'B',20), cbind(1,'A',40), cbind(0,'A', 60)))
colnames(df) <- c("y", "treatment", "weights")
df$weights <- as.numeric(df$weights)
df$y <- as.numeric(df$y)
m.logit.2 <- glm(y~treatment, family=binomial, weights=weights, data=df)
summary(m.logit.2)

#################
# Exercise 12.2 #
#################

pizza <- read.csv("pizza_delivery.csv")
attach(pizza)
pizza$long_delivery <- as.numeric(pizza$time > 30)

m1 <- glm(long_delivery ~ operator + branch + pizzas, data=pizza)
summary(m1)


# b) Calculate confidence interval for variable pizzas i) with confint and ii) manually
confint(m1)
str(summary(m1)) # $coefficients stores coefficient table 
lc <- summary(m1)$coefficients
lc[5,1] + qnorm(0.975)*lc[5,2] # lower limit
lc[5,1] - qnorm(0.975)*lc[5,2] # upper limit

# c) Interpretation of model with interactions
m2 <- glm(long_delivery ~ operator + branch*pizzas + driver, data=pizza)
summary(m2)

detach(pizza)

#################
# Exercise 12.3 #
#################

# a)
theatre <- read.csv("theatre.csv")
theatre <- theatre[,-1]
theatre$Theatre.bin <- ifelse(theatre$Theatre > 150,1,0)
attach(theatre)
m.theatre.1 <- glm(Theatre.bin~Age+Sex+Income+Culture+Theatre_ly, family=binomial)
summary(m.theatre.1)

# b)
predictions <- data.frame(cbind(predict(m.theatre.1, type="link"),  predict(m.theatre.1, type="response") ))
colnames(predictions) <- c("eta", "p")
predictions <- predictions[order(predictions$eta),]

pdf(file="exercise_12.3_b.pdf")
par(mar= c(5, 5, 2, 2))
plot(predictions, type="l", cex.axis=1.75,cex.lab=1.75,cex=1.5,xlab=expression(eta),ylab="Probability")
dev.off()

# c)
# Only include Age, Sex, Income and Culture
# All models with two variables: 2 out of 4: 6 models
# save AIC values
AIC.all2 <- numeric(6)

m.theatre.all2 <-glm(Theatre.bin~Age+Sex, family=binomial)
AIC.all2[1] <- m.theatre.all2$aic
m.theatre.all2 <-glm(Theatre.bin~Age+Income, family=binomial)
AIC.all2[2] <- m.theatre.all2$aic
m.theatre.all2 <-glm(Theatre.bin~Age+Culture, family=binomial)
AIC.all2[3] <- m.theatre.all2$aic
m.theatre.all2 <-glm(Theatre.bin~Sex+Income, family=binomial)
AIC.all2[4] <- m.theatre.all2$aic
m.theatre.all2 <-glm(Theatre.bin~Sex+Culture, family=binomial)
AIC.all2[5] <- m.theatre.all2$aic
m.theatre.all2 <-glm(Theatre.bin~Income+Culture, family=binomial)
AIC.all2[6] <- m.theatre.all2$aic

AIC.all2

# All models with three variables: 3 out of 4: 4 models

AIC.all3 <- numeric(4)

m.theatre.all3 <-glm(Theatre.bin~Age+Sex+Income, family=binomial)
AIC.all3[1] <- m.theatre.all3$aic
m.theatre.all3 <-glm(Theatre.bin~Age+Sex+Culture, family=binomial)
AIC.all3[2] <- m.theatre.all3$aic
m.theatre.all3 <-glm(Theatre.bin~Age+Culture+Income, family=binomial)
AIC.all3[3] <- m.theatre.all3$aic
m.theatre.all3 <-glm(Theatre.bin~Sex+Income+Culture, family=binomial)
AIC.all3[4] <- m.theatre.all3$aic

AIC.all3


# d)
library(generalhoslem)

m.t2 <- glm(Theatre.bin~Age+Sex+Income+Culture, family=binomial)
summary(m.t2)

# 10 groups
gof.10 <- logitgof(Theatre.bin, predict(m.t2, type="response"), g=10)
print(gof.10)

# 20 groups
gof.20 <- logitgof(Theatre.bin, predict(m.t2, type="response"), g=20 )
print(gof.20)

# 5 groups
gof.5 <- logitgof(Theatre.bin, predict(m.t2, type="response"), g=5 )
print(gof.5)

# Additionally, to obtain the table of observed and expected frequencies for g=5,
# we could do the following:
print(cbind(gof.5$observed, gof.5$expected), digits=3)


# e) Confusion matrix for different thresholds
# total:
table(Theatre.bin)

#  collect predictions and true y's & order them
pred.result <- data.frame(Theatre.bin,  predict(m.t2, type="response") )
colnames(pred.result) <- c("y", "p")
pred.result <- pred.result[order(pred.result$p),]

# c=0.5
c <- 0.5
# all cases with p<=c are predicted as 0, the others as 1
pred.0 <- pred.result[(pred.result$p<=c), ]
pred.1 <- pred.result[(pred.result$p>c), ]

# number of observations with prediction 0 or 1
nrow(pred.0)
nrow(pred.1)

# number of 0's and 1's in the respective prediction group
table(pred.0$y)
table(pred.1$y)

c <- 0.35
# all cases with p<=c are predicted as 0, the others as 1
pred.0 <- pred.result[(pred.result$p<=c), ]
pred.1 <- pred.result[(pred.result$p>c), ]

# number of observations with prediction 0 or 1
nrow(pred.0)
nrow(pred.1)

# number of 0's and 1's in the respective prediction group
table(pred.0$y)
table(pred.1$y)





# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

# Possible solutions to the exercises in chapter 9

################
# Exercise 9.1 #
################

MLP <- function(lambda){
27*log(lambda) - log(factorial(4)*factorial(3)*factorial(8)*factorial(6)*factorial(6)) - 5*lambda
}

curve(MLP, from=0, to=10) # simple solution, nicer graph prduced below

pdf(file="exercise_9.1_b.pdf")
par(mar= c(5, 5, 2, 2))
curve(MLP, from=0, to=10, xlab=expression(lambda), ylab="log-likelihood", cex.axis=1.75,cex.lab=1.75,cex=1.5,lwd=2)
lines(c(5.4,5.4),c(-1000,MLP(5.4)),lwd=2,type="l",lty=2)
text(4.5,-90,expression(paste(bar(x),"=5.4")),cex=2)
dev.off()

################
# Exercise 9.5 #
################

eland <- c(450,730,700,600,620,660,850,520,490,670,700,820,910,770,760,620,550,520,590,490,620,660,940,790)
t.test(eland)$conf.int

################
# Exercise 9.8 #
################

binom.test(11,104)$conf.int

##################
# Exercise 9.11  #
##################

pizza <- read.csv("pizza_delivery.csv")
attach(pizza)

timecat     <- cut(time, breaks=c(-1,30,150))

library(epitools)  #cmake sure the library is installed, e.g using install.packages("epitools")
oddsratio(table(timecat,operator), method="wald")

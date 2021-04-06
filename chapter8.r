# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

# Possible solutions to the exercises in chapter 8

#################
# Exercise 8.6  #
#################

set.seed(24121980)

raffle <- function(n){
probab <- 1-( (choose(500,2)*choose(3500,n-2))/(choose(4000,n)))-((choose(500,1)*choose(3500,n-1))/(choose(4000,n)))-((choose(500,0)*choose(3500,n))/(choose(4000,n)))
return(probab)
}
raffle(50:100)
raffle(63:64)

nb <- seq(1:75)

pdf("exercise_8.6_b.pdf",width=9)
par(mar= c(5, 5, 2, 2))
plot(nb,raffle(nb),type="l",xlab="Number of tickets",ylab="Probability for at least 3 prices",cex.lab=1.75,cex.axis=1.75,cex=1.75,lwd=3)
dev.off()

##################
# Exercise 8.11  #
##################

set.seed(24121980)

R <- 1000
means <- c(rep(NA,R))
for(i in 1:R){means[i] <- mean(rnorm(1000))}
mean(means)
var(means)

plot(density(means)) # simple plot, labelling imrpoved below

pdf("exercise_8.11_a.pdf",width=9)
par(mar= c(5, 5, 2, 2))
plot(density(means),xlab="",ylab="",cex.lab=1.75,cex.axis=1.75,cex=1.75,lwd=3,main="")
dev.off()

means2 <- c(rep(NA,R))
for(i in 1:R){means2[i] <- mean(rexp(1000))}
mean(means2)
var(means2)

pdf("exercise_8.11_b.pdf")
par(mar= c(5, 5, 2, 2))
plot(density(means2),xlab="",ylab="",cex.lab=1.75,cex.axis=1.75,cex=1.75,lwd=3,main="")
dev.off()

means3 <- c(rep(NA,10000))
for(i in 1:10000){means3[i] <- mean(rexp(1000))}

pdf("exercise_8.11_c.pdf")
par(mar= c(5, 5, 2, 2))
plot(density(means3),xlab="",ylab="",cex.lab=1.75,cex.axis=1.75,cex=1.75,lwd=3,main="")
dev.off()

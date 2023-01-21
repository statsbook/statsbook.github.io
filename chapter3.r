# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

################
# Exercise 3.1 #
################

distance <- c(12.5,29.9,14.8,18.7,7.6,16.2,16.5,27.4,12.1,17.5)
altitude <- c(342,1245,502,555,398,670,796,912,238,466)

# a) 
mean(distance)
mean(altitude)

median(distance)
median(altitude)

# b)
quantile(distance,probs=0.75)
quantile(distance,probs=0.25)
quantile(altitude,probs=0.75)
quantile(altitude,probs=0.25)

# c)
quantile(distance,probs=0.75)-quantile(distance,probs=0.25)
quantile(altitude,probs=0.75)-quantile(altitude,probs=0.25)

amd <- function(mv){1/length(mv)*sum(abs(mv-median(mv)))}
amd(distance)
amd(altitude)

var(altitude)
var(distance)

# e)

pdf(file="exercise_3.1_e1.pdf")
par(mar= c(5, 5, 2, 2))
boxplot(altitude,xlab="",ylab="Altitude (in m)", cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
dev.off()

pdf(file="exercise_3.1_e2.pdf")
par(mar= c(5, 5, 2, 2))
boxplot(distance,xlab="",ylab="Distance (in km)", cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
dev.off()

# f)
weighted.mean(c(10,17.5,25),c(4/10,4/10,2/10))

#################
# Exercise 3.6  #
#################

mymode <- function(vec){
mt <- table(vec)
names(mt)[mt == max(mt)]
}

mymode(c(1,2,3,3,4))
mymode(c("1","1","2"))

################
# Exercise 3.9 #
################

# d)
u2 <- c(0,0.25,0.5,0.75,1)
v2 <- c(0,0.044,0.168,0.428,1)

library(ineq)
investment <- c(800,10300,4700,2200)
plot(Lc(investment))
ineq(investment)

# d) "by hand", more complicated solution than "plot(Lc(investment))". 
# We plot u against v, and make the graph nice using the options. 
pdf(file="exercise_3.9_d2.pdf",width=9)
par(mar= c(5, 5, 2, 2))
plot(u2,v2,type="l",xlab=expression(u[i]),ylab=expression(v[i]), cex.axis=1.75,cex.lab=1.75,cex=1.75,cex.main=1.5,lwd=3, xaxt = "n",bty="n", yaxt="n",las=1)
axis(1, at=round(u2,digits=2), labels=c("0","0.25","0.5","0.75","1"), cex.axis=1.75,cex.lab=1.75,cex=1.75,cex.main=1.5,lwd=3)
axis(2, at=round(v2,digits=2), labels=c("0","0.044","0.168","0.428","1"), cex.axis=1.75,cex.lab=1.75,cex=1.75,cex.main=1.5,lwd=3,las=1)
dev.off()


#################
# Exercise 3.10 #
#################

#a)
pizza <- read.csv("pizza_delivery.csv")
attach(pizza)
summary(pizza[,c("time","temperature","bill","pizzas")])

# b)
quantile(time,probs=0.99)
quantile(temperature,probs=0.99)

# c)
amdev <- function(mv){1/length(mv)*sum(abs(mv-mean(mv)))}
amdev(temperature)

# d)
sc.time <- scale(time)
mean(sc.time)
var(sc.time) 

# e)
# Simple solution
boxplot(temperature)
boxplot(time)

# nicer graph, using the options
pdf(file="exercise_3.10_e1.pdf")
par(mar= c(5, 5, 2, 2))
boxplot(temperature,range=0,xlab="",ylab="Temperature", ylim=c(30,100), cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75)
dev.off() 

pdf(file="exercise_3.10_e2.pdf")
par(mar= c(5, 5, 2, 2))
boxplot(time,range=0,xlab="",ylab="Delivery Time (in minutes)", ylim=c(10,60), cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75)
dev.off() 

# f)
tc <- cut(time,breaks=seq(10,60,10))
weighted.mean(c(15,25,35,45,55),table(tc)/sum(table(tc)))

# g)
# fast and simple
qqplot(time[driver=="Luigi"],time[driver=="Domenico"])
qqplot(time[driver=="Mario"],time[driver=="Salvatore"])

# long and nice
pdf(file="exercise_3.10_g1.pdf")
par(mar= c(5, 5, 2, 2))
qqplot(time[driver=="Luigi"],time[driver=="Domenico"],xlim=c(10,60),ylim=c(10,60),xlab="Delivery time from Luigi",ylab="Delivery time from Domenico", cex.axis=1.75,cex.lab=1.75,cex.main=1.75, cex=1.75)
abline(a=0,b=1,lwd=3)
dev.off()

pdf(file="exercise_3.10_g2.pdf")
par(mar= c(5, 5, 2, 2))
qqplot(time[driver=="Mario"],time[driver=="Salvatore"],xlim=c(10,60),ylim=c(10,60),xlab="Delivery time from Mario",ylab="Delivery time from Salvatore", cex.axis=1.75, cex.lab=1.75,cex.main=1.75, cex=1.75)
abline(a=0,b=1,lwd=3)
dev.off()

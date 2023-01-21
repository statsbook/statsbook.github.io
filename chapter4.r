# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

################
# Exercise 4.5 #
################

# c)
X <- c(-6,-5,2,4,7,15,17,19,13,9,4,0,10,10,14,17,22,24,26,27,22,19,14,12,1,0,5,9,14,20,23,24,21,14,9,4)
Y <- c(91,89,76,52,42,36,37,39,26,27,68,92,13,21,42,64,79,81,86,92,36,23,13,41,23,82,40,45,39,43,50,95,64,78,9,12)
Z <- c(rep('Davos',12),rep('Polenca',12),rep('Basel',12))

cor(X,Y)
cor(X[Z=='Davos'],Y[Z=='Davos'])
cor(X[Z=='Basel'],Y[Z=='Basel'])
cor(X[Z=='Polenca'],Y[Z=='Polenca'])


#################
# Exercise 4.8  #
#################

# a)
decathlon <- read.csv("decathlon.csv", row.names=1)
attach(decathlon)
cor(X.Discus,X.High.jump)
# c)
cor(decathlon)
# d)
cor(na.omit(decathlon))

# and....
library(corrplot) # nice summary of results with package corrplot (not mentioned in solutions)
corrplot(cor(na.omit(decathlon)),method="number",col="black",cl.pos="n",tl.col="black")

detach(decathlon)

################
# Exercise 4.9 #
################

pizza <- read.csv("pizza_delivery.csv")
pizza$tempcat <- cut(pizza$temperature, breaks=c(0,65,100))
pizza$timecat <- cut(pizza$time, breaks=c(0,30,100))
attach(pizza)

# a)
addmargins(table(tempcat,timecat))

# b) 
(101*261)/(213*691)

# c) 
library(vcd)          # download if necessary (install.packages("vcd"))
library(ryouready)    # download if necessary (install.packages("ryouready"))
library(lattice)      
assocstats(xtabs(~tempcat+timecat))
ord.gamma(table(tempcat,timecat))
ord.tau(table(tempcat,timecat))

# simple
barchart(table(tempcat,timecat),horizontal=F,stack=T)
# more options 
barchart(table(tempcat,timecat),horizontal=F,stack=T,auto.key=list(space="top", columns=2,points=FALSE,cex=1.5,rectangles=TRUE,title="timecat"),col=gray.colors(2),ylab=list(label="absolute frequencies",  cex=1.75), xlab=list(cex=1.75),par.names.text=list(cex=2),par.settings=list(superpose.polygon=list(col=gray.colors(2))))

# d) 
# simple
plot(time,temperature)

# nicer, also saved as pdf
pdf(file="exercise_4.9_d.pdf") 
par(mar= c(5, 5, 2, 2))
plot(time, temperature, pch=19, cex.axis=1.75,cex.lab=1.75,cex=1.5,xlab="Time",ylab="Temperature")
dev.off()

cor(time,temperature)
cor(time,temperature,method="spearman")

# e)
# simple
boxplot(temperature~driver)
boxplot(temperature~operator)

# nicer, also saved as pdf 
pdf(file="exercise_4.9_e1.pdf") 
par(mar= c(5, 5, 2, 2))
boxplot(temperature~driver,xlab="Driver",ylab="Temperature", cex.axis=1.25,lwd=3,cex.lab=1.75,cex.main=1.75)
dev.off()

pdf(file="exercise_4.9_e2.pdf") 
par(mar= c(5, 5, 2, 2))
boxplot(temperature~operator,xlab="Operator",ylab="Temperature", cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75)
dev.off()

cor(temperature,pizzas)
cor(temperature,bill)


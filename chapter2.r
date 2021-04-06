# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

#################
# Exercise 2.1  #
#################

# a)
results2014 <- c(0.6215,0.2223,0.0635,0.0240,0.0067,0.0620)
results2009 <- c(0.6590,0.1666,0,0.0455,0.0742,0.0547)
barplot(results2014)           # default version, does not look nice, type ?barplot for options to improve graph

pdf(file="exercise_2.1_a.pdf")   # to save graph as a pdf in your working directory - delete or comment out if not needed
par(mar= c(5, 5, 2, 2))          # to increase plot margins on the side of the figure to ensure that large labels stay in figure 
barplot(results2014,names.arg=c('ANC',"DA","EFF","IFP","COPE","Others"),col=gray.colors(6),
                    ylim=c(0,0.7),xlab="Parties",ylab="Votes (%)",
                    ,cex.axis=1.75,lwd=3,cex.lab=1.75,cex.names=1.25)       # to learn more about these options type ?par
dev.off()



# b)
difference <- results2014-results2009
barplot(difference)           # does not look nice, improve with options
                              #(e.g. range of y-axis)

pdf(file="exercise_2.1_b.pdf")
par(mar= c(5, 5, 2, 2))
barplot(difference,names.arg=c("ANC","DA","EFF","IFP","COPE","Others"),col=gray.colors(6),
                    ylim=c(-0.1,0.1),xlab="Parties",ylab="Difference",
                    ,cex.axis=1.75,lwd=3,cex.lab=1.75,cex.names=1.25)
dev.off()


#################
# Exercise 2.2  #
#################

# c) + d)  
# [to improve the look of the basic graphs, we use options such as ylab and xlab for labelling and cex for increasing font sizes]
goals <- c(6,24,91,8,4,25,3,83,89,34,25,24,18,6,23,10,28,4,63,6,60,5,40,2,22,26,23,26,
44,49,34,2,33,9,16,55,23,13,23,4,8,26,70,4,6,60,23,95,28,49,6,57,33,56,7)

# simple option
hist(goals)
plot(density(goals))

# nicer option (saved as pdf)
pdf(file="exercise_2.2_c.pdf")
par(mar= c(5, 5, 2, 2))
hist(goals, breaks=c(0,15,30,45,60,75,90,96),col="lightgrey", cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,ylab="Density",xlab="Time (in min)",xlim=c(0,100),ylim=c(0,0.025),main="")
dev.off()

pdf(file="exercise_2.2_d.pdf")
par(mar= c(5, 5, 2, 2))
plot(density(goals),lwd=2.5,main="",xlab="Time (in min)", cex.axis=1.75,cex.lab=1.75,cex.main=1.75)
dev.off()  


# f) 
plot.ecdf(goals)

# nicer
pdf(file="exercise_2.2_f1.pdf")
par(mar= c(5, 5, 2, 2))
plot.ecdf(goals, cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,xlab="Time (in min)",ylab="F(x)",main="")
dev.off()

goals_cat <- cut(goals, breaks=c(0,15,30,45,60,75,90,96),labels=c(15,30,45,60,75,90,96)) # see exercise solutions for more details

pdf(file="exercise_2.2_f2.pdf")
par(mar= c(5, 5, 2, 2))
plot.ecdf(as.numeric(as.character(goals_cat)), cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,xlab="Time (in min)",ylab="F(x)",main="")
dev.off()

################
# Exercise 2.5 #
################

# not asked in the exercise, but useful to learn more about R; plot ECDF and add lines with the "lines" command
score <- c(rep(1,1),rep(2,3),rep(3,8),rep(4,8),rep(5,27),rep(6,30),rep(7,11),rep(8,6),rep(9,4),rep(10,2))

pdf(file="exercise_2.5.pdf", width=8)
par(mar= c(5, 5, 2, 2))
plot.ecdf(score, cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,xlab="Score",ylab="F(x)",main="",yaxp=c(0,1,10),xaxp=c(0,10,10))
lines(c(0,5,10),c(0,0.47,1),type="l",col="darkgrey",lwd=3)
lines(c(3,3),c(0,0.282),type="l",col="darkgrey",lwd=3,lty=2)
lines(c(9,9),c(0,0.894),type="l",col="darkgrey",lwd=3,lty=2)
text(4,0.27,"28.2%",col="darkgrey",cex=1.5)
text(10,0.9,"89.4%",col="darkgrey",cex=1.5)
dev.off()

################
# Exercise 2.6 #
################

library(ggplot2)    # Browse http://docs.ggplot2.org/ for a comprehensive summary
pizza <- read.csv("pizza_delivery.csv")
attach(pizza)

# a)
pdf(file="exercise_2.6_a.pdf",width=11)
par(mar= c(5, 5, 2, 2))
hist(temperature, cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,xlab="Temperature",main="",xlim=c(40,90),ylim=c(0,400),col="lightgrey",ylab="Number of deliveries")
lines(c(65,65),c(0,400),type="l",lty=2,lwd=3)
dev.off()

# b)  [the "theme" option is to improve labelling and font size, not necessarily needed]
pdf(file="exercise_2.6_b.pdf",width=11)
p1 <- ggplot(data=pizza,aes(x=temperature))
p2 <- p1 + geom_histogram(fill="darkgrey",alpha=0.5,binwidth=2.5,)  + scale_y_continuous("Number of deliveries") + scale_x_continuous("Temperature")
plot(p2  + theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20), axis.text.x = element_text(size=14), axis.text.y = element_text(size=14)))
dev.off()

# c)
pdf(file="exercise_2.6_c.pdf",width=11)
par(mar= c(5, 5, 2, 2))
barplot(table(driver),ylim=c(0,400),ylab="Number of deliveries", xlab="Driver",col=gray.colors(7),main="Title", cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,cex.names=1.5)
dev.off()


# d)  [the "theme" option is to improve labelling and font size, not necessarily needed]
qplot(driver,data=pizza,aes="bar")

pdf(file="exercise_2.6_d.pdf",width=11)
p3 <- qplot(driver,data=pizza,aes=("bar"),fill=day)
p4 <- p3 + scale_fill_grey() +theme_bw() + scale_y_continuous("Number of deliveries") + scale_x_discrete("Driver") 
plot(p4+ theme(axis.title.x = element_text(size=20),axis.title.y = element_text(size=20), legend.title =  element_text(size=18, hjust = 0), legend.text =  element_text(size=18), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16)))
dev.off()






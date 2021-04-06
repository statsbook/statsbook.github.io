# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

#################
# Exercise 7.3  #
#################

# a)
cdf <- function(x){
(3*x^2-2*x^3)*(x<=1 & x>=0) + 1*(x>1) + 0*(x<0)
}

pdf(file="exercise_7.3_a.pdf", width=11)  # save figure as pdf, comment out if not needed
par(mar= c(5, 5, 2, 2))                   # optional, to create space for increased axis fonts
curve(cdf,from=-0.5,to=1.5,cex.axis=1.75,lwd=3,cex.lab=1.75,cex.main=1.75,xlab="x",ylab="F(x)",main="")
dev.off()                                 # save figure as pdf, comment out if not needed

# d)
cdf(2/3)-cdf(1/3)


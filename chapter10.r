# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

# Possible solutions to the exercises in chapter 10

#################
# Exercise 10.3 #
#################

na <- c(18.19,16.98,19.97,16.98, 18.19, 15.99, 13.79, 15.9,15.9,15.9, 15.9,15.9,19.97,17.72)
a <- c(10.5,12.0, 9.54, 10.55, 11.99, 9.3, 10.59, 10.5, 10.01, 11.89,11.03, 9.52, 15.49, 11.02)

# a)

mean(na)
mean(a)

var(na)
var(a)

sd(na)
sd(a)

sd(na)/mean(na)
sd(a)/mean(a)

# b) + c)

t.test(na,mu=16.95)

# d)
t.test(a,mu=16.95,alternative="less")

# e)
t.test(na,a, alternative="greater")

# f)
var.test(na,a)

# g)
wilcox.test(na,a)

#################
# Exercise 10.5 #
#################

# b)
qbinom(p=0.975,prob=0.1,size=230)
qbinom(p=0.025,prob=0.1,size=230)

# d)
fisher.test(matrix(c(30,200,7,112),ncol=2))

#################
# Exercise 10.6 #
#################

p1 <- c(91, 101, 112,  99, 108,  88,  99, 105, 111, 104)
p2 <- c(261,  47,  40,  29,  64,  6,  87,  47, 98, 351)

t.test(p1,p2,alternative="greater")
wilcox.test(p1,p2,alternative="greater")

#################
# Exercise 10.8 #
#################

# a) & b) full data
titanic <- t(matrix(c(202,135,125,160,180,541,211,674), ncol=4, nrow=2))
chisq.test(titanic)
prop.test(titanic)

# c) summarized data
fisher.test(matrix(c(327,295,391,1215),ncol=2,nrow=2))

##################
# Exercise 10.10 #
##################

theatre <- read.csv("theatre.csv")
attach(theatre)

# a)
t.test(Culture[Sex==1],Culture[Sex==0])

# b)
wilcox.test(Culture[Sex==1],Culture[Sex==0])
t.test(Culture[Sex==1],Culture[Sex==0],var.equal=TRUE)

# c)
t.test(Theatre[Sex==1],Theatre[Sex==0], alternative="greater")

# d)
t.test(Theatre,Theatre_ly,paired=TRUE)

detach(theatre)

##################
# Exercise 10.11 #
##################

pizza <- read.csv("pizza_delivery.csv")
attach(pizza)

# a)
t.test(temperature,mu=65,alternative="greater")
t.test(time,mu=30,alternative="less")

# b)
table(free_wine)
binom.test(c(229,1037),p=0.15,alternative="less")

# c)
hot <- cut(temperature,breaks=c(-Inf,65,Inf))
fisher.test(table(hot,operator))
chisq.test(table(hot,operator))
prop.test(table(hot,operator))

# d)
chisq.test(table(branch))

# e)
prop.test(table(branch, operator))

# f)
chisq.test(table(driver, branch))

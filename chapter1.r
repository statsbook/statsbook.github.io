# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

# Possible solutions to the exercises in chapter 1

#################
# Exercise 1.5  #
#################

# a)
pizza <- read.csv("pizza_delivery.csv")

# b) 
View(pizza)
pizza

# c) 
pizza2 <- pizza[1:5,1:5]
pizza2
write.csv(pizza2,file='pizza2.csv')
write.table(pizza2,file='pizza2.dat') 
save(pizza2,file='pizza2.Rdata')

# d) 
pizza$NewTemperature <- 32+1.8*pizza$temperature

# e)
attach(pizza)
NewTemperature

# f) 
str(pizza)
dim(pizza) 
colnames(pizza)
names(pizza)
nrow(pizza)
ncol(pizza)
head(pizza)
tail(pizza)




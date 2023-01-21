# Specify your working directory on the computer. The folder should (ideally) contain the data sets used in the exercises.
setwd('C:/Users/123456/Documents/Book/Data')

# Possible solutions to the exercises in chapter 13

#################
# Exercise 13.2 #
#################

amounts <- c(60.80, 41.59, 61.01, 59.72, 27.02, 55.43, 26.74, 37.21, 47.29, 40.90, 48.01, 34.04, 65.24, 58.71, 31.85, 31.64, 64.00, 55.10, 58.91, 45.70, 35.90, 58.86, 43.83, 49.62, 31.69, 32.35, 35.20, 50.34, 63.33, 45.67, 36.69, 44.52, 63.13, 50.34, 54.24, 58.55,27.64, 49.44, 42.79, 21.67, 37.80, 56.37, 42.14, 33.02, 58.30, 50.22, 44.31, 44.09, 56.31, 48.18, 55.76, 46.26, 64.41, 55.95, 70.15, 44.95, 64.95, 49.22, 50.29, 39.16)
amounts

# a)
choose(60, 10)

# b)
set.seed(4321)
amt_wor <- sample(amounts, size = 10, replace = FALSE)
amt_wor

# c)
meanamtwor <- mean(amt_wor)
meanamtwor

N<-60
n<-length(amt_wor)
var_amt_wor <- ((N- n)/(N*n))*var(amt_wor)
var_amt_wor

mean(amt_wor) - qt(0.975, n-1)*sqrt(((N-n)/(length(amt_wor)*N))*var(amt_wor))
mean(amt_wor) + qt(0.975, n-1)*sqrt(((N-n)/(length(amt_wor)*N))*var(amt_wor))

# d)  
tot_wor <- N*meanamtwor
tot_wor

tot_var_wor <- N*N*var_amt_wor
tot_var_wor

#################
# Exercise 13.3 #
#################

amounts <- c(60.80, 41.59, 61.01, 59.72, 27.02, 55.43, 26.74, 37.21, 47.29, 40.90, 48.01, 34.04, 65.24, 58.71, 31.85, 31.64, 64.00, 55.10, 58.91, 45.70, 35.90, 58.86, 43.83, 49.62, 31.69, 32.35, 35.20, 50.34, 63.33, 45.67, 36.69, 44.52, 63.13, 50.34, 54.24, 58.55, 27.64, 49.44, 42.79, 21.67, 37.80, 56.37, 42.14, 33.02, 58.30, 50.22, 44.31, 44.09, 56.31, 48.18, 55.76, 46.26, 64.41, 55.95, 70.15, 44.95, 64.95, 49.22, 50.29, 39.16)
amounts

# a)
sample_wr <- 60^10
sample_wr

# b)
set.seed(1234)
amt_wr <- sample(amounts, size = 15, replace = TRUE)
amt_wr

meanamtwr <- mean(amt_wr)
meanamtwr

N <- 60
n <- length(amt_wr)
var_amt_wr <- ((N- n)/(N*n))*var(amt_wr)
var_amt_wr

lcl_amt_wr <- mean(amt_wr) - qt(0.975, n-1)*sqrt(var(amt_wr)/n)
ucl_amt_wr <- mean(amt_wr) + qt(0.975, n-1)*sqrt(var(amt_wr)/n)
lcl_amt_wr
ucl_amt_wr

# d)
tot_wr <- N*meanamtwr
tot_wr 
tot_var_wr <- N*N*var_amt_wr
tot_var_wr


#################
# Exercise 13.4 #
#################

marks <- c(29.1, 69.8, 68.7, 69.9, 88.9, 71.2, 20.8, 38.6, 73.3, 61.1, 75.5, 63.6, 42.6, 93.9,43.4, 87.0, 42.9, 41.3, 34.9, 38.6, 45.3, 44.2, 32.7, 23.2, 37.5, 84.8, 62.1, 93.2, 86.5, 23.7, 56.5, 41.2, 44.4, 60.6, 34.5, 80.8, 36.1, 40.7, 99.4, 84.6, 64.3, 71.7,44.9, 69.7, 46.4, 60.2, 74.2, 58.8, 39.5, 81.2)
marks

# a)
set.seed(1234)
markswor <- sample(marks[marks>30], size=10, replace=FALSE)
markswor

meanmarkswor <- mean(markswor)
meanmarkswor

# b)
N <- 50;
n <- length(markswor)
var_marks_wor <- ((N- n)/(N*n))*var(markswor)
var_marks_wor

lcl_marks_wor <- mean(markswor) - qt(0.975, n-1) * sqrt((N-n)/(n*N)*var(markswor))
ucl_marks_wor <- mean(markswor) + qt(0.975, n-1)*sqrt((N-n)/(n*N)*var(markswor))
lcl_marks_wor
ucl_marks_wor

# c)
tot_wor <- N*meanmarkswor
tot_var_wor <- N*N*var_marks_wor
tot_wor
tot_var_wor


#################
# Exercise 13.5 #
#################

marks <- c(29.1, 69.8, 68.7, 69.9, 88.9, 71.2, 20.8, 38.6, 73.3, 61.1, 75.5, 63.6, 42.6, 93.9,43.4, 87.0, 42.9, 41.3, 34.9, 38.6, 45.3, 44.2, 32.7, 23.2, 37.5, 84.8, 62.1, 93.2, 86.5, 23.7, 56.5, 41.2, 44.4, 60.6, 34.5, 80.8, 36.1, 40.7, 99.4, 84.6, 64.3, 71.7,44.9, 69.7, 46.4, 60.2, 74.2, 58.8, 39.5, 81.2)
marks

# a)
set.seed(1234)
markswr <- sample(marks[marks<60], size = 15, replace = TRUE)
markswr

# b)
meanmarkswr <- mean(markswr)
meanmarkswr

N <- 50
n <- length(markswr)
se_marks_wr <- sqrt(((N-n)/(N*n))*var(markswr))
se_marks_wr

lcl_marks_wr <- mean(markswr) - qt(0.975, n-1) * sqrt(((N-n)/(n*N))*var(markswr))
ucl_marks_wr <- mean(markswr) + qt(0.975, n-1)*sqrt(((N-n)/(n*N))*var(markswr))
lcl_marks_wr
ucl_marks_wr

# c)
tot_wr <- N*meanmarkswr
tot_var_wr <- N*N*se_marks_wr*se_marks_wr
tot_wr
tot_var_wr

#################
# Exercise 13.6 #
#################

taste <-  c( 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0)
taste

# a)
set.seed(1234)
taste_wor <- sample(taste, size=15, replace=FALSE)
taste_wor
sum(taste_wor)

# b)
meantastewor <- mean(taste_wor)
meantastewor

N <- 50
n <- length(taste_wor)
setastewor <- sqrt(((N - n)/(N*(n - 1))) * mean(taste_wor) * (1 - mean(taste_wor)))
setastewor

#################
# Exercise 13.7 #
#################

taste <-  c( 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0)
taste

# a)
set.seed(4321)
taste_wr <- sample(taste, size=10, replace=TRUE)
taste_wr
sum(taste_wr==0)

#b)
meantastewr <- 1 - mean(taste_wr)
meantastewr

N <- 50
n <- length(taste_wr)
setastewr <- sqrt((mean(taste_wr) * (1-mean(taste_wr)))/(n-1))
setastewr


#################
# Exercise 13.8 #
#################

child <- c(1:10)
internet <- c(0.3, 2.2, 0.5, 0.7, 1.0, 1.8, 3.0, 0.2, 2.3, 2.2)
sleep <- c(5.8, 4.4, 6.5, 5.8, 5.6, 5.0, 4.8, 6.0, 6.1, 5.9)
internet_sleep_data <- data.frame(child, internet, sleep)

# a)
corr_internet_sleep_data <- function(d, i){
  internet_sleep_data = d[i,]
  return(cor(internet_sleep_data$internet, internet_sleep_data$sleep))
}

set.seed(1234)
library(boot)
corrboot <- boot(data = internet_sleep_data, statistic = corr_internet_sleep_data, R = 5000)
corrboot

mean(corrboot$t) - corrboot$t0

# b)
boot.ci(boot.out = corrboot, type = c('norm', 'perc'))


#################
# Exercise 13.9 #
#################

child <- c(1:10)
sleep <- c(5.8, 4.4, 6.5, 5.8, 5.6, 5.0, 4.8, 6.0, 6.1, 5.9)
sleep_data <- data.frame(child, sleep)

# a)
sleep_CV <- function(d, i){
  d2 <- d[i,]
  return(sqrt(var(d2$sleep))/mean(d2$sleep))
}

set.seed(1234)
cvboot <- boot(data = sleep_data, statistic = sleep_CV, R=5000)
cvboot

# b)
boot.ci(boot.out = cvboot, type = c('norm', 'perc'))

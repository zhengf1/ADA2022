# clear the workspace
rm(list=ls())

# set the working directory it needed
  # obtain the path of current R file
  current_path = rstudioapi::getActiveDocumentContext()$path 
  # set the current path as working directory
  setwd(dirname(current_path)) 

# -----------------------------------------------------------------
# 1. Shooting Stars
# -----------------------------------------------------------------
B = 1000  # number of simulations
n = 300   # sample size
p = 100   # dimension of x
  
sig = c() # empty; used to store
  
for (b in 1:B){
  # simulate the data X
  x = matrix(rnorm(n*p), n, p)
  # simulate the data y based on true beta = 0
  y = rnorm(n)
  
  # suppose, we estimate the model by regress y on x 
  fit = glm(y~x)
  # get the p-value of all the coefficients
  pval = summary(fit)$coefficients[-1,'Pr(>|t|)']
  # calculate the number of p-value that is smaller than 0.05
  # in other words, count the number of significant estimates
  sig = c(sig, sum(pval<0.05))
}

# plot the number of significant estimates for B simulations
hist(sig, freq=FALSE)
abline(v = mean(sig), col = "red", lwd=2)

# In reality, we know y has nothing to do with x, so we would expect
# all the estimates are 0 (insignificant). 
# In simulation, we see that 5% of the estimates are significant, 
# which seems to contract the truth. 
# Does this mean the regression is not OK? No, indeed, we choose to 
# use 5% as our alpha =  
# P(the probability of rejecting the null hypothesis when it is true)

# Even if no useful variable ( ??=0 ) exists, 
# we will have significant results if MANY 
# variables are tested. A small p-value will 
# show with high probability. One must be careful 
# of any interpretation. Empirical people should 
# not be so paranoid about stars. Respect data, 
# respect non-significance.

# -----------------------------------------------------------------
# 2. Simulation as a Tool: sample chi-square
# -----------------------------------------------------------------
B = 10000     # reduce B if your computational power restricts
x = rnorm(B)
xx = rnorm(B)
y = x^2 + xx^2
hist(y, freq = FALSE)
d = density(y)
plot(d, main="kernel density")


# -----------------------------------------------------------------
# Order statistics from the Uniform distribution
# -----------------------------------------------------------------

# motivation, what is order statistics?
minX = c()
medX = c()
maxX = c()
for (i in 1:10000){
ran_x = rnorm(9) # random 9 normal samples
order_x = sort(ran_x) # sort them

minX = c(minX, min(order_x)) # keep the smallest sample
medX = c(medX, median(order_x)) # keep the median
maxX = c(maxX, max(order_x)) # keep the max sample
}
plot(density(minX), main="Order stat distribution"
     , col='red', xlim=c(-4,4), ylim=c(0,1), lwd=2)
lines(density(medX), col='blue', lwd=2)
lines(density(maxX), col='purple', lwd=2)

legend("topleft", legend=c("k=1 (min)", "k=5 (median)","k=9 (max)")
       ,col= c('red','blue','purple'), cex=1.1
       ,lty=1, title="Order statistics")

# tute question starts
B=10000
p_order = c()
p_7th = c()
for(b in 1:B){
  p = runif(10)
  p_order = rbind(p_order, sort(p))
  p_7th = c(p_7th,sort(p)[7])
}

dim(p_order) # check the dimension

#pick a order between 1 and 10 inclusive
ii = 7

pii = p_order[, ii]
hist(pii, freq=FALSE, main='Histogram of a Chosen Order Statistic', xlab="")
abline(v=ii/11, col='blue', lwd=2)
print(paste("The theoretical mean is", ii/11))
print(paste("The sample mean is", mean(pii)))

hist(p_7th)

# Draw the means of these means
p_mean = colMeans(p_order)
plot(1:10, p_mean, pch=19, col='red', cex=2, xlab='order', ylab='mean') #cex is the dot size
abline(a=0, b=1/11, col='blue')


















 
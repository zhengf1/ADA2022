# clear the workspace
rm(list=ls())
# -----------------------------------------------------------------
# Sampling distribution
# -----------------------------------------------------------------
# 1 and 2
set.seed(123)
X = rnorm(100, mean=1, sd=3)
hist(X, freq=FALSE)
xx = seq(from=-6, to=8, length.out=100)
ypdf = dnorm(xx, mean=1, sd=3)
lines(xx, ypdf, type='l', col='blue', lw=2)

# 3
# sample mean
mean(X)
# standard deviation of the sample mean (in theory)
3/sqrt(100)
# standard error of the sample mean
sd(X)/sqrt(100)

# 4 MC
nSim = 10000
n = 100   
Xbar = c() # collect sample mean from each simulation
# Sig2bar = c() # collect sample variance from each simulation
# pb = txtProgressBar(min = 0, max = nSim, initial = 0, style = 3) 
for (i in 1:nSim){
  # setTxtProgressBar(pb, i)
  X = rnorm(n, mean=1, sd = 3)
  Xbar = rbind(Xbar, mean(X))
 # Sig2bar = rbind(Sig2bar, var(X))
}
# close(pb)

# distribution of the sample mean (Xbar) from simulated data
hist(Xbar, freq=FALSE, ylim=c(0,1.5))         # the histogram of sample mean
xx = seq(from=0, to=2, length.out=100)        # discretize the grid 
ypdf = dnorm(xx, mean=1, sd=3/sqrt(n))        # calculate the theoretical density
lines(xx, ypdf, type='l', col='blue', lw=2)   # plot the density line for reference

mean(Xbar)
var(Xbar)

# distribution of the sample variance (Sig2bar) from simulated data
hist(Sig2bar, freq=FALSE)         # the histogram of sample mean

mean(Sig2bar)
var(Sig2bar)

# -----------------------------------------------------------------
# Bootstrap
# -----------------------------------------------------------------
# regenerate data to avoid overwriting
set.seed(123)
X = rnorm(10000, mean=1, sd=3)
#nonparametric bootstrap
B = 10000 # number of bootstrap samples you want
          # if may take times if B is large
n = 100   # sample size of each bootstrap
Xbar = c()
# pb = txtProgressBar(min = 0, max = B, initial = 0, style = 3) 
for (b in 1:B){
  # setTxtProgressBar(pb, b)
  Xbar1 = mean(sample(X, n, replace=TRUE))
  Xbar = rbind(Xbar, Xbar1)
}
# close(pb)
 
# It is better to make smaller cell to make clear of steps.
# combine them after the code is tested and you feel comfortable.
hist(Xbar, freq=FALSE, ylim=c(0,1.5)) 
xx = seq(from=0, to=2, length.out=100)
ypdf = dnorm(xx, mean=1, sd=3/sqrt(n))
lines(xx, ypdf, type='l', col='blue', lw=2)


# -----------------------------------------------------------------
# 2nd extension: variance
# -----------------------------------------------------------------
# regenerate data to avoid overwriting
set.seed(123)
X = rnorm(100, mean=1, sd=3)
#nonparametric bootstrap
B = 10000
n = 100
B.Sig2bar = c()
pb = txtProgressBar(min = 0, max = B, initial = 0, style = 3) 
for (b in 1:B){
  setTxtProgressBar(pb, b)
  B.Sig2bar = c(B.Sig2bar, var(sample(X, n, replace=TRUE)))
}
close(pb)

hist(B.Sig2bar, freq=FALSE) 
mean(B.Sig2bar) # problematic when sample size is small

# It is better to make smaller cell to make clear of steps.
# combine them after the code is tested and you feel comfortable.
s = var(X)
# bias correction
B = 10000
n = 100
eb <- c()
pb = txtProgressBar(min = 0, max = B, initial = 0, style = 3) 
for (b in 1:B){
  sb <- var(X[sample.int(100, replace=TRUE)]) 
  eb <- c(eb, sb-s)
}

mean(eb)
tvals <- quantile(eb, c(0.05, 0.95))
print(tvals)
debiased_s = s-tvals[2:1]; # do not print
names(debiased_s)= c('5%', '95%') # what if remove this line?
print(debiased_s)
# 9 is in the CI!
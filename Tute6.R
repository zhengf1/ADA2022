# clear the workspace
rm(list=ls())

# -----------------------------------------------------------------
# set the working directory it needed
# -----------------------------------------------------------------
# obtain the path of current R file
current_path = rstudioapi::getActiveDocumentContext()$path 
# set the current path as working directory
setwd(dirname(current_path)) 

# You don't need to set the working directory
# if you load the data from online

# -----------------------------------------------------------------
# TE Simulation
# -----------------------------------------------------------------

# 1. Simulate a data set and estimate the TE.

# specify the mean of treatment outcome
mu0 = 0         # mean outcome of control group
mu1 = 1         # mean outcome of treated group
sigma0 = 1      # sd of outcome in control group
sigma1 = 2      # sd of outcome in treated group
n0 = 100        # number of control group
n1 = 50         # number of treated group

set.seed(135)

y0 = rnorm(n0, mean=mu0, sd = sigma0)
y1 = rnorm(n1, mean=mu1, sd = sigma1)

TE = mean(y1) - mean(y0)
se = sqrt(var(y1)/n1 + var(y0)/n0)

print(c(TE, se))

# 2. Carry out a Monte Carlo study to compare the simulated standard deviation to the theory.
B = 1000
set.seed(135)

# We ignore se because TE are simulated and its distribution is automatically obtained.
TE = c()

for (b in 1:B){
  y0 = rnorm(n0, mean=mu0, sd = sigma0)
  y1 = rnorm(n1, mean=mu1, sd = sigma1)
  TE = rbind(TE, mean(y1) - mean(y0))
}

# Get a histogram and overlay a normal distribution with the true sd
hist(TE, freq=FALSE) # the distribution of TE from each iteration

# plot the true treatment effect based on theory (the true TE)
mu = mu1 - mu0                     # true TE
sd = sqrt(sigma1^2/n1+sigma0^2/n0) # true standard deviation

# Overlay normal distribution (x-axis set to +/- 3 sd from mean)
xx = seq(from = mu - 3*sd,         # discretize x to plot the true density
         to = mu + 3*sd, 
         length.out = 200)
lines(xx, dnorm(xx, mean=mu, sd = sd), col=2, lwd=2)

# -----------------------------------------------------------------
# Heterogeneity and ATE
# -----------------------------------------------------------------

# specify the mean of treatment outcome
mu_m_0 = 0      # male without treatment (control)    
mu_m_1 = 1      # male with treatment    
mu_f_0 = 0      # female without treatment (control)    
mu_f_1 = 2      # female with treatment    

# 0 effect on both male and female without treatment
# larger effect on female with treatment

n0 = 100        # number of untreated people (half male, half female)
n1 = 50         # number of treated people (half male, half female)

# MC
B = 1000        # number of iterations
set.seed(135)   # set seed to make result replicable

# We ignore se because TE are simulated and its distribution is automatically obtained.
TE = c()        # used to store TE from each iteration

for (b in 1:B){
  # assume both treated and untreated have half male and half female
  y0 = append(rnorm(n0/2, mean=mu_f_0, sd = 1),  # untreated female (control)
              rnorm(n0/2, mean=mu_m_0, sd = 1))  # untreated male (control)
  y1 = append(rnorm(n1/2, mean=mu_m_1, sd = 1),  # treated male
              rnorm(n1/2, mean=mu_f_1, sd = 1))  # treated female
  TE = rbind(TE, mean(y1) - mean(y0))            # compute the average TE  
}
 
hist(TE, freq=FALSE)
mean(TE)
# -----------------------------------------------------------------
# Regression for ATE: Genarate the Data
# -----------------------------------------------------------------

# specify the mean of treatment outcome
mu_m_0 = 0      # male without treatment (control) 50   
mu_m_1 = 1      # male with treatment 25   
mu_f_0 = 0      # female without treatment (control) 50   
mu_f_1 = 2      # female with treatment 25     

# 0 effect on both male and female without treatment
# larger effect on female with treatment

n0 = 100        # number of untreated people (half male, half female)
n1 = 50         # number of treated people (half male, half female)

# there must be efficient ways, but readability is also important
# male & control
n = n0 / 2                                    # number of males in control
g = as.character(rep("male", n))              # define g = male
d = rep(0, n)                                 # control group, d = 0
y = rnorm(n, mean = mu_m_0, sd = 1)           # generate the outcome y

# female & control
n = n0 / 2                                    # number of females in control
g = append(g, as.character(rep("female", n))) # define g = female
d = append(d, rep(0, n))                      # control group, d = 0
y = append(y, rnorm(n, mean = mu_f_0, sd = 1))

# male & treatment
n = n1 / 2
g = append(g, as.character(rep("male", n)))
d = append(d, rep(1, n))
y = append(y, rnorm(n, mean = mu_m_1, sd = 1))

# female & treatment
n = n1 / 2
g = append(g, as.character(rep("female", n)))
d = append(d, rep(1, n))
y = append(y, rnorm(n, mean = mu_f_1, sd = 1))

# make outcome y, treated or not d, gender g as a data frame
df = data.frame(y=y, d=d, g=g)    

# Tabulate the data
table(g, d)

#install.packages("pivottabler")
library(pivottabler)
# Make a pivot table (this is where the library comes in)
# Values printed to 2 decimal places.
qpvt(df, "g", "d", "mean(y)")

# preview the data frame
df

# -----------------------------------------------------------------
# Regression Model
# -----------------------------------------------------------------

# regress y = alpha + gamma * d + u 
fit = glm(y~d, data=df) # only one variable d
summary(fit)

# regress y = alpha_g + gamma * d + u
fit2 = glm(y~-1+g+d, data=df) # now g takes either male or female; two more variables
summary(fit2)

# regress y = alpha_g + gamma_g * d + u
df$dfac = as.factor(df$d)
fit3 = glm(y~-1+g  + g:dfac, data=df) # g: 2 var; g:dfac: 2 var
summary(fit3)


fit$coefficients[2]
fit2$coefficients[3]
(fit2$coefficients[3] + fit2$coefficients[4])/2

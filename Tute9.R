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

# install.packages("textir")
library(textir)

# ---------------------------------------------------------------------
# Data: Gas Octane Data: PLS
# ---------------------------------------------------------------------
gas = read.csv("https://www.dropbox.com/s/255h2nr3u82fj6j/gasoline.csv?dl=1")

# define dependent variable, y
octane <- gas[,1]
# the rests are explanatory variables, x
nir <- as.matrix(gas[,-1])

# ---------------------------------------------------------------------
# Fit PLS: use command "pls" from library "textir"
gaspls <- pls(x=nir, y=octane,  K=3) 

lm(octane~nir)
# replicate the plots shown in the lecture
par(mfrow=c(1,3), mai=c(.7,.7,.1,.1))
plot(gaspls, bty="n", cex.lab=1.4)

# you may recover the default plot format
par(mfrow=c(1,1)) 

# ---------------------------------------------------------------------
# Partial Least Squares (PLS)
# without using the package, write algorithm
# ---------------------------------------------------------------------

# Algorithm 20 Marginal Regression (Page 226)
# Algorithm 21 Partial Lest Squares (Page 228)

# Implement Algorithm 21 and try to
# draw a plot similar to the lecture (above).

# ---------------------------------------------------------------------
# step 1: MR (Algorithm 20)
K = dim(nir)[2]             # the number of variables
n = dim(nir)[1]             # likewise, the number of obs

y1 = octane                 # dependent variable
x = nir                     # covariates 

# estimate the "slope" coefficient
phi = cor(x, y1) / sd(x)  # regress y1 on each of x1, x2...x401
# Is it the same as "lm(y1 ~ x)"?
# In addition, phi is actually not coefficient 
# beta_hat = var(x,y1)/var(x)

v1 = x %*% phi              # fitted values

fit1 = glm(y1 ~ v1)         # Fit the “forward” univariate linear regression

summary(fit1)
yfit1 = fit1$fitted.values  # fitted values predicted by fitted y

plot(y1, yfit1, pch=20)
corr(y1, yfit1)

# ---------------------------------------------------------------------
# step 2: PLS (Algorithm 21)
# an extension of marginal regression

y2 = y1 - yfit1             # residual        

phi2 = cor(x, y2) / sd(x)   # regress residual y2 on x (for each var) 
v2 = x %*% phi2             # fitted value for residual y2 

beta2 = drop(cor(v2, y2)/sd(v2))  # regress residual y2 on v2
# drop just convert a 1 by 1matrix into a array

# Now calculate the new fitted values as per the algorithm
yfit2 = yfit1 + beta2 * v2  

plot(y1, yfit2, pch=20)
corr(y1, yfit2)

# ---------------------------------------------------------------------
# step 3: PLS one more time
# just another iteration, same algorithm (code) performed 
y3    = y1 - yfit2               # Residual series

phi3  = cor(x, y3) / sd(x)       # Loadings
v3    = x %*% phi3               # Factors

beta3 = drop(cor(v3, y3)/sd(v3)) # Beta coefficient

yfit3 = yfit2 + beta3 * v3       # Fitted values


plot(y1, yfit3, pch=20)
corr(y1, yfit3)

par(mfrow=c(1,3))
plot(y1, yfit1)
legend("topleft", paste("corr = ", round(corr(y1, yfit1), 2)), bty="n")
plot(y1, yfit2)
legend("topleft", paste("corr = ", round(corr(y1, yfit2), 2)), bty="n")
plot(y1, yfit3)
legend("topleft", paste("corr = ", round(corr(y1, yfit3), 2)), bty="n")

par(mfrow=c(1,1))

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Do it Right!!!
# actually PLS package contains much more steps than algorithm 20 and 21
# eg. The factors should be orthogonal.
# The data requires first step normalization.
# ---------------------------------------------------------------------
# Yong's approach.
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# step 1: MR
K = dim(nir)[2]            # number of variables

y1 = octane                # y
x = nir                    # covariates

v1 = 0                     # starting value  

# loop to accumulate fitted values for each covariates
for (ii in 1:K){
  fit = glm(y1~x[,ii])     # linear regression
  v1 = v1 + fit$fitted     # accumulate fitted values
}

# Average ALL fitted values as a factor! 
v1 = v1/K                  # avarage over number of variables
fit1 = glm(y1 ~ v1)        # regress y1 on the factor

summary(fit1)
yfit1 = fit1$fitted.values # the first fit

plot(y1, yfit1, pch=20)
corr(y1, yfit1)

# ---------------------------------------------------------------------
# step 2: PLS
# Nothing special, just repeat the process
# algorithm is shown below (in tute material)

# 1. Get the y residuals from the previous regression. 
# This will be your new y, say y2.
y2 = fit1$residuals             # y values from the previous regression.

# 2. Use the same way as step 1 to get the second "factor".
v2 = 0 
for (ii in 1:K){
  fit = glm(y2~x[,ii])          # OLS to get beta
  v2 = v2 + fit$fitted.values   # accumulate fitted values
}
v2 = v2/K                       # average fitted values

# 3. Then, regress this "factor" on the first factor and 
# obtain the residuals. This is the "factor" residual and the real second factor.
v2 = glm(v2~v1)$residuals       # "factor" residual and the real second factor.

# 4. Regress the $y$ residual on the second factor to get the second fit. 
fit2 = glm(y2 ~ v2)
summary(fit2)

# 5. The sum of the first and second fitted values is the new fitted value.
yfit2 = yfit1 + fit2$fitted.values # the second fit

plot(y1, yfit2, pch=20)
corr(y1, yfit2)

# ---------------------------------------------------------------------
# step 3: PLS one more time
# Nothing special, just repeat the process again

y3 = y1 - yfit2 # Residuals

# Retrieve factor loadings like last time
v3 = 0
for (ii in 1:K){
  fit = glm(y3~x[,ii])         # All possible univariate regressions
  v3  = v3 + fit$fitted.values # Sum of factor values
}

v3   = v3/K # Average

# Same as step 4 from earlier, except that we regress on v1 AND v2
v3   = glm(v3~v1+v2)$residuals

fit3 = glm(y3 ~ v3)
summary(fit3)

# Finally obtain the fitted values.
yfit3 = yfit2 + fit3$fitted.values   # the third (last) fit


plot(y1, yfit3, pch=20)
corr(y1, yfit3)


par(mfrow=c(1,3))
plot(y1, yfit1)
legend("topleft", paste("corr = ", round(corr(y1, yfit1), 2)), bty="n")
plot(y1, yfit2)
legend("topleft", paste("corr = ", round(corr(y1, yfit2), 2)), bty="n")
plot(y1, yfit3)
legend("topleft", paste("corr = ", round(corr(y1, yfit3), 2)), bty="n")



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
# install and load required library
# -----------------------------------------------------------------

install.packages("gamlr")
install.packages("MASS")
install.packages("glmnet")
install.packages("distrom")

library(gamlr)
library(MASS)
library(glmnet) 
library(distrom)

# -----------------------------------------------------------------
# FGL data
# -----------------------------------------------------------------

data(fgl)  # load the data 
help(fgl)  # look at the data description
str(fgl)   # look at the data structure
table(fgl$type)

# in lecture, we have 
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1] #remove the intercept
# The textbook model includes all levels and interactions of the elements with RI.

# Here we want all squared and the interactions 
xfgl <- sparse.model.matrix(type~.^2, data=fgl)[,-1] #remove the intercept
gtype <- fgl$type

dim(xfgl) # why 45 columns?

# glmnet function is very similar to gamlr
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial") #default alpha=1, LASSO
plot(glassfit)

# let's look at how coefficients vary with Lambda
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4))       # set the pic layout and margins
plot(glassfit$glmnet.fit, xvar="lambda")    # plot each coefficients with lambda

B  <- coef(glassfit, select="1se") #this is a list
B 

B_min  <- coef(glassfit, select="min") #this is a list
B_min
B_min_m = do.call(cbind, B_min)
colnames(B_min_m) = names(B_min)
print(B_min_m)

# -----------------------------------------------------------------
# Distributed Multinomial Regression
# -----------------------------------------------------------------

# this section requires packages "distrom" 
detectCores()
cl = makeCluster(6)
cl

# Distributed Multinomial Regression
glassdmr <- dmr(cl, xfgl, gtype, verb=TRUE) # cores, X, y
stopCluster(cl)
names(glassdmr)
glassdmr["WinF"]
is.list(glassdmr)
glassdmr[["WinF"]]

par(mfrow=c(2,3))
for(k in names(glassdmr)) plot(glassdmr[[k]], main=k)  
# For lists, one generally uses [[ ]] to select any single element, 
# whereas [ returns a list of the selected elements.

Bdmr <- coef(glassdmr)
Bdmr

# E.g., compare Head
cbind(Bdmr[,"Head"], B_min_m[,"Head"])

# These results differ because DMR uses different  λ  for each category.
# The DMR uses Poisson regression for approximation.
# The DMR uses parallelisation.

# -----------------------------------------------------------------
# In-sample prediction
# -----------------------------------------------------------------

# multinomila logit
fit_lasso = glmnet(xfgl, gtype, family="multinomial")

# distributed 
cl = makeCluster(6)
fit_dmr <- dmr(cl, xfgl, gtype, verb=TRUE) # some redundancy to make this part self-contained
stopCluster(cl)

# To whom the credit belongs to
# https://stackoverflow.com/questions/40920051/r-getting-aic-bic-likelihood-from-glmnet
# https://en.wikipedia.org/wiki/Akaike_information_criterion
sy_aicc = function(fit){
  tLL <- fit$nulldev - deviance(fit)
  k <- fit$df
  n <- fit$nobs
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  return(AICc)
}

aicc_lasso = sy_aicc(fit_lasso)          # calculate AICc FOR THE LASSO regression
id_min = which.min(aicc_lasso)           # find the index which has min AICc


pred_lasso = drop(predict(fit_lasso, newx=xfgl, type="response", s=fit_lasso$lambda[id_min]))
# this part is tricky. You need to understand the structure of predict(glmnet)
# It is a 3-D matrix. You need a proper lambda value to get the prediction that you want.

Bdmr = coef(fit_dmr)
pred_dmr <- predict(Bdmr,xfgl,type="response")


par(mfrow=c(1,2)) # be careful of setting this, change it back to (1,1) after use
for(kk in levels(gtype)){
  type = as.factor(gtype==kk)
  levels(type)=c("others", kk)
  boxplot(pred_lasso[, kk]~type, main=kk, xlab="", ylab="LASSO", col=2)
  boxplot(pred_dmr[, kk]~type, main=kk, xlab="", ylab="DMR", col=3)
}


# -----------------------------------------------------------------
# OOS
# -----------------------------------------------------------------

dim(fgl)
table(gtype)
# let's randomly select 10 from WinF for the test. You may try WinNF.
# other categories may not have enough data.
winf = which(gtype=="WinNF")
test = winf[1:10]

fit_lasso = glmnet(xfgl[-test,], gtype[-test], family="multinomial")
cl = makeCluster(6)
fit_dmr <- dmr(cl, xfgl[-test,], gtype[-test], verb=TRUE) # some redundancy to make this part self-contained
stopCluster(cl)

aicc_lasso = sy_aicc(fit_lasso)
which.min(aicc_lasso)

pred_lasso = drop(predict(fit_lasso, newx=xfgl[test,], type="response", s=fit_lasso$lambda[88]))

Bdmr = coef(fit_dmr)
pred_dmr <- predict(Bdmr,xfgl[test,],type="response")

par(mfrow=c(1,2))
kk="WinNF"

boxplot(pred_lasso[, kk], main=kk, xlab="", ylab="LASSO OOS", col=2, ylim=c(0, 1))
boxplot(pred_dmr[, kk], main=kk, xlab="", ylab="DMR OOS", col=3, ylim=c(0, 1))

# a global view may not be too bad. WinF and WInNF could be similar.
par(mfrow=c(1,2))
kk="WinNF"

boxplot(pred_lasso, main=kk, xlab="", ylab="LASSO OOS", col=2, ylim=c(0, 1))
boxplot(pred_dmr, main=kk, xlab="", ylab="DMR OOS", col=3, ylim=c(0, 1))


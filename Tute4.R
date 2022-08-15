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
#Use Taddy's library for LASSO. (Gamma Lasso Regression)
install.packages("gamlr")
install.packages("slm")

library(gamlr)
library(slm)

# -----------------------------------------------------------------
# LASSO
# -----------------------------------------------------------------
# if you want to load data directly
load(url("https://www.dropbox.com/s/jcubctl0m1wyu7w/Beijing_house_price_reg.rda?dl=1"))

# and the formula
x = "-1 + DOM + followers + square + bedRoom + livingRoom + kitchen + bathRoom + floor + buildingType + 
    constructionTime + buildingStructure + elevatorPerHouse + fiveYearsProperty + subway + district +
    communityAverage + tradeYear + tradeMonth"
y = "price"
fml = as.formula(paste(y, "~", x))

# repeat the OLS
fit_ols = glm(formula=fml, data=house) 

houseX = sparse.model.matrix(fml, data=house)[,-1] #design matrix x
housey = house$price

# gamlr: Gamma-Lasso regression
fit_lasso <- gamlr(houseX, housey, family="gaussian", lambda.min.ratio=1e-2, nlambda=200)
fit_lasso$lambda

plot(fit_lasso) # the ubiquitous path plot

fit_lasso$lambda

B_lasso = coef(fit_lasso)
B_ols = coef(fit_ols)
print(cbind(B_lasso, B_ols))
  
log(fit_lasso$lambda[which.min(AICc(fit_lasso))]) #lower is better

# -----------------------------------------------------------------
# 1 Plot the time trend
# -----------------------------------------------------------------
ind = grep("tradeYear[d]*", rownames(B_lasso)) # get the id (location) for tradeyear coefficients
byear_lasso = B_lasso[ind]
byear_ols = B_ols[ind]

plot(1:length(byear_lasso), byear_lasso, xaxt="n", xlab="year", ylab="year effect", type="l", 
     lwd=2, col=2, ylim=c(-15000, 45000)) # plot year vs year effects from LASSO
lines(1:length(byear_lasso), byear_ols, col=3, lwd=2)
abline(h=0) # a reference line with 0 effect
legend("topleft", legend = c("LASSO", "OLS"), bty="n", col=2:3, lty=1)
# remember adding axis in Week 1 note?
xgrid = 1:length(byear_lasso)
xlabel = 2011:2018
axis(1, at=xgrid, labels=xlabel)


# -----------------------------------------------------------------
# 2 Plot the monthly cycle
# -----------------------------------------------------------------
# try month effect
ind = grep("tradeMonth[d]*", rownames(B_lasso))
bmonth_lasso = B_lasso[ind]
bmonth_ols = B_ols[ind]

NT = length(bmonth_lasso)
plot(1:NT, bmonth_lasso, xaxt="n", xlab="year", ylab="month effect", type="l", lwd=2, col=2, 
     ylim=c(-1000, 10000))
lines(1:NT, bmonth_ols, col=3, lwd=2)
abline(h=0)
legend("topleft", legend = c("LASSO", "OLS"), bty="n", col=2:3, lty=1)
# remember adding axis in Week 1 note?
xgrid = 1:NT
xlabel = 2:12
axis(1, at=xgrid, labels=xlabel)
# which month is the best time to sell?

# -----------------------------------------------------------------
# 3 Is the followers a good indicator for house price?
# -----------------------------------------------------------------
B_lasso["followers", ]
B_ols["followers" ]
# lasso penalise the "followers"

# -----------------------------------------------------------------
# 4 Which district is more expensive?
# -----------------------------------------------------------------
# which district is more valuable?
ind = grep("district[w]*", rownames(B_lasso))
bd_lasso = B_lasso[ind ]
bd_ols = B_ols[ind]
cbind(bd_lasso, bd_ols)
# Haidian is the winner. FYI, top universities such as Peking or Tsinghua are there.


# -----------------------------------------------------------------
# 5 What is the value of elevator intensity? Or is there any value in an elevator at all?
# -----------------------------------------------------------------
B_lasso["elevatorPerHouse",]
B_ols["elevatorPerHouse"]


# -----------------------------------------------------------------
# 6 Does people want to live higher to see the view?
# -----------------------------------------------------------------
# higher or lower?
B_lasso["floor",]
B_ols["floor"]
# but is there a nonlinear effect on floor?
x = "-1 + DOM + followers + square + bedRoom + livingRoom + kitchen + bathRoom + floor + I(floor^2) + buildingType + 
    constructionTime + buildingStructure + elevatorPerHouse + fiveYearsProperty + subway + district +
    communityAverage + tradeYear + tradeMonth"
y = "price"
fml3 = as.formula(paste(y, "~", x))
fit3 = glm(formula=fml3, data=house) 

houseX_1 = sparse.model.matrix(fml3, data=house)[,-1] #design matrix x
housey_1 = house$price

fit_lasso_1 <- gamlr(houseX_1, housey_1, family="gaussian", lambda.min.ratio=1e-3, nlambda=200)
plot(fit_lasso_1)

B_lasso_1 = coef(fit_lasso_1)

B3 = summary(fit3)$coef
a1 = B3["floor", 1]
a2 = B3["I(floor^2)", 1]
xgrid = 1:70
y = a1*xgrid + a2 * xgrid^2
plot(xgrid, y, type="l", ylim = c(-5000, 0)) # not concave enough, higher floor is not so idea.

b1 = B_lasso_1["floor", 1]
b2 = B_lasso_1["I(floor^2)", 1]
z = b1 * xgrid + b2 * xgrid^2
lines(xgrid, z, col=2)

legend("topright", legend = c("OLS", "LASSO"), bty="n", col=1:2, lty=1)

# they are actually similar if your check the range of floor
summary(house$floor)
  
# -----------------------------------------------------------------
# LASSO Cross-validation
# -----------------------------------------------------------------
fit_cv = cv.gamlr(houseX, housey, family="gaussian", nfold = 10, lambda.min.ratio = 1e-3)
plot(fit_cv)
# We have MANY data. So CV likes more parameters!
# you do not see the standard deviation of the deviance on the figure, because they are SMALL. THis is also because of the big data.

fit_cv$lambda.min #this is what you want
fit_cv$seg.min

B_cv = coef(fit_cv, select="min") # if you use the default, it is "1se"


# year trend
ind = grep("tradeYear[d]*", rownames(B_lasso))
byear_lasso = B_lasso[ind]
byear_ols = B_ols[ind]
byear_cv = B_cv[ind]

plot(1:length(byear_lasso), byear_lasso, xaxt="n", xlab="year", ylab="year effect", type="l", 
     lwd=2, col=2, ylim=c(-15000, 45000))
lines(1:length(byear_lasso), byear_ols, col=3, lwd=1)
lines(1:length(byear_lasso), byear_cv, col=4, lwd=1)
abline(h=0)
legend("topleft", legend = c("LASSO", "OLS", "CV"), bty="n", col=2:4, lty=1)
# remember adding axis in Week 1 note?
xgrid = 1:length(byear_lasso)
xlabel = 2011:2018
axis(1, at=xgrid, labels=xlabel)



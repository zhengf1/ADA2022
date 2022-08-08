# clear the workspace
rm(list=ls())

# set the working directory it needed
# obtain the path of current R file
current_path = rstudioapi::getActiveDocumentContext()$path 
# set the current path as working directory
setwd(dirname(current_path)) 

# You don't need to set the working directory
# if you load the data from online

# -----------------------------------------------------------------
# 1. Linear Regression
# -----------------------------------------------------------------
oj = read.csv("https://www.dropbox.com/s/8oll01xv92mkij8/oj.csv?dl=1", stringsAsFactors = TRUE)
# you can download the data to local drive as well.

str(oj) # R-studio does not really need this, 'look at Environment'
levels(oj$brand)

# Relevel the brands
# change Tropicana as the reference/default group
# i.e. reorder the levels that Tropicana is the first brand listed
# (in the original data, Dominicks is the first one)
relevel(oj$brand, "tropicana")
levels(oj$brand)
# above does not work, because it's not specified to the data set!

# this code works, why?
oj$brand = relevel(oj$brand, "tropicana")
levels(oj$brand)

# Simple linear regression
fit = glm(log(sales) ~ log(price) + brand, data=oj)
summary(fit)

# scatter plot and the fitted regression line
beta <- coef(fit)  # extract all the beta estimates
brandcol <- c("gold", "green", "red")

plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], cex=.1, pch=20, bty="n")
abline(a=beta[1], b=beta[2], col=brandcol[1], lwd=2) # a is intercept, b is slope
abline(a=beta[1]+beta[3], b=beta[2], col=brandcol[2], lwd=2)
abline(a=beta[1]+beta[4], b=beta[2], col=brandcol[3], lwd=2)
legend("bottomleft", bty="n", lwd=2, col=brandcol, legend=levels(oj$brand))


# re-estimate without intercept
fit1 = glm(log(sales) ~ -1 + log(price) + brand, data=oj)
summary(fit1)


# any difference? 
# basically no; same interpretation

# -----------------------------------------------------------------
# 2. Logistic Regression: studies the labor force participation problem
# -----------------------------------------------------------------
# The data set is in .raw format. Need "readr" to read
install.packages("readr") # you only need to install once
# Load the library
library("readr") 

data = read_table("https://www.dropbox.com/s/vhzum0w1t614k9c/MROZ.raw?dl=1", col_names = FALSE)
tail(data) 

# always check the data before we proceed; adjust accordingly
data$X7 = as.numeric(data$X7)
data$X21 = as.numeric(data$X21)
data["X23"] = NULL

# X1; X2 up to X22 just look not good
# change it according to description file
names(data) = c("inlf",   "hours",     "kidslt6",   "kidsge6",   "age",       "educ",      "wage",      "repwage",  
                "hushrs",  "husage",   "huseduc",   "huswage",   "faminc",    "mtr",       "motheduc",  
                "fatheduc",  "unem",      "city",      "exper",     "nwifeinc",  "lwage",     "expersq")

# you may save it as csv; so that you can load it easily in the future
write.csv(data, "MROZ.csv", row.names = FALSE)
# save it to online cloud or local drive.

# 2. What is the distribution of wages (a histogram)? Does it make sense?
hist(data$wage)

# 3. run the regression according to the question
fit = glm(inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6 , data=data, family = "binomial")
summary(fit)

# 4. R2? 
(R2 = 1 - fit$deviance / fit$null.deviance) # bracket will print the result

# 5. 10-fold out-of-sample cross-validation to calculate the average OOS R2
deviance <- function(y, pred, family=c("gaussian","binomial")){
  family <- match.arg(family)
  if(family=="gaussian"){
    return( sum( (y-pred)^2 ) ) # calculate the deviance if y is Gaussion distributed
  }else{
    if(is.factor(y)) y <- as.numeric(y)>1 # make sure y is numerical
    return( -2*sum( y*log(pred) + (1-y)*log(1-pred) ) ) 
  }
}
# these two functions are available in the textbook and Matt Taddy's website
R2 <- function(y, pred, family=c("gaussian","binomial")){
  fam <- match.arg(family)
  if(fam=="binomial"){
    if(is.factor(y)){ y <- as.numeric(y)>1 }  # make sure y is numerical
  }
  dev <- deviance(y, pred, family=fam) # how true data y is explained by the predicted y; (SS.residual) 
  dev0 <- deviance(y, mean(y), family=fam) # how well data y is explained by the intercept; (SS.T)
  return(1-dev/dev0) # think about R2 = 1- SSE/SST
}


n <- nrow(data) # the number of observations
K <- 10 # the number of `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)] #ceiling(): round up to integer.
# create an empty dataframe of results
OOS <- data.frame(R2=rep(NA,K))  # try rep() to understand it
sy_formula = as.formula("inlf~nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6")

for(k in 1:K){ 
  print(paste0("working on the ", k, "th fold"))
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the two regressions, exclude the k-th fold samples
  rfull <- glm(sy_formula, data=data, subset=train, family="binomial")
  
  ## get predictions: type=response so we have probabilities
  predfull <- predict(rfull, newdata=data[-train,], type="response")
  
  ## calculate and log R2
  OOS$R2[k] <- R2(y=data$inlf[-train], pred=predfull, family="binomial")
  
  ## print progress
  cat(k, " is done\n")
}

boxplot(OOS$R2, xlab="10-folds OOS", col = "orange")
mean(OOS$R2)


# -----------------------------------------------------------------
# 3. A Simulation Study on Heteroskedasticity
# -----------------------------------------------------------------
n = 100 # sample size
x = rchisq(n, df = 1) # generate n chi-square df=1 samples
y = 1 + 0.5 * x + rnorm(100, mean = 0, sd = sqrt(0.01 + x^2)) # calculate true y
plot(x, y, pch=19, col=4)

B = 1000 # do 1000 replications
tstat = c() # used to save t-value in the loop

for(b in 1:B){
  x = rchisq(n, df = 1) # generate n chi-square df=1 samples
  y = 1 + 0.5 * x + rnorm(100, mean = 0, sd = sqrt(0.01 + x^2))
  # keep the t-value for the slope coefficient
  tstat = c(tstat, summary(glm(y~x))$coefficients["x", "t value"])
}

 
xgrid = seq(from=min(tstat), to = max(tstat), length.out = 300) # discretize t
ypdf = dt(xgrid, df=n-2) # calculate the y-axis value for each x-axis
hist(tstat, freq=FALSE, ylim = c(0,0.5), col=5) 
lines(xgrid, ypdf, col=2, lwd = 2) #
legend(8, 0.45, legend="True distribution", bty="n", fill=5, border=NA)
legend(7, 0.42, legend="OLS implied distribution", lty=1,col=2, bty="n", lwd=2)

# If we test significance using the OLS formula, 
# we tend to over reject the null hypothesis. 
# This is a size distortion.

 

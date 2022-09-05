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

library(stargazer)

# -----------------------------------------------------------------
# DiD: Effect of a Garbage Incinerator on Housing Price.
# -----------------------------------------------------------------

# 1. Read the data and give proper names to each variable.
# -----------------------------------------------------------------

data = read.table("https://www.dropbox.com/s/6nga0ds63zhujwq/KIELMC.raw?dl=1", header = F)

var.names = c("year", "age", "agesq", "nbh", "cbd", "intst", "lintst", "price", 
              "rooms", "area", "land", "baths", "dist", "ldist", "wind", "lprice",  
              "y81", "larea", "lland", "y81ldist", "lintstsq", "nearinc", "y81nrinc", "rprice",
              "lrprice")
colnames(data) = var.names
head(data)

# 2. Use 1981 data to estimate a linear model of rprice on nearinc
# -----------------------------------------------------------------
fit1981 = glm(rprice~nearinc, data=data, subset = data$y81==1)
summary(fit1981)

# 3. Use 1978 data to estimate the same model.
# -----------------------------------------------------------------
fit1978= glm(rprice~nearinc, data=data, subset = data$y81==0)
summary(fit1978)

# 4. What is the treatment effect based on the previous two regression results?
# -----------------------------------------------------------------
print(coef(fit1981) - coef(fit1978))

# 5. Set up a DiD regression as in the lecture and find the treatment effect.
# -----------------------------------------------------------------
fit.did = glm(formula = rprice ~ nearinc * y81, data = data)
summary(fit.did)

# the coeff on the interaction term is identical to the differenced 
# slope coefficients from the previous two simple regressions.

stargazer(fit1981, fit1978, fit.did,type = "text", nobs = FALSE, mean.sd = TRUE, median = TRUE,
          iqr = TRUE, digits=2,
          title = "Summary statistics for Garbage Incinerator")

# -----------------------------------------------------------------
# RDD
# -----------------------------------------------------------------

# DGP
# -----------------------------------------------------------------
# simulation   
# left model y = a0 + a1 * r + a2 * r^2
# right model y = b0 + b1 * r + b2 * r^2 + b3 * r^3

a0 = -1
a1 = 1
a2 = 0.5
b0 = 1
b1 = 0.5
b2 = 1
b3 = -1

#local ATE is b0 - a0 = 2

N = 1000
set.seed(123)
r = rnorm(N) * 0.8
y = rep(0, N)
ind0 = r<=0
ind1 = r>0
y[ind0] = a0 + a1 * r[ind0] + a2 * r[ind0]^2 + rnorm(sum(ind0))
y[ind1] = b0 + b1 * r[ind1] + b2 * r[ind1]^2 + b3 * r[ind1]^3 + rnorm(sum(ind1))
plot(r, y, col=2)

df = data.frame(y=y, r=r, d=(r>0))
# write.csv(df, "rdd.csv", row.names = F)

# Estimate the TE
# -----------------------------------------------------------------

# DiD: brute force
fit.lm = glm(y~r*d, data=df)
summary(fit.lm) # does not seem very well, but why?

# DiD: find local values
w = seq(0.1, 3, length.out = 50) # try a different range of neighbour 
ate = rep(0, 50)

for (ii in 1:50){ # only consider a subset of data around the treatment
  fit.lm = glm(y~r*d, data=df, subset = (abs(df$r)<w[ii]))
  ate[ii] = coef(fit.lm)["dTRUE"]
}

plot(w, ate, type="l", col=2, lwd=2) # how the ATE changes wrt length of nrighbour

# use LOESS
fita <- loess(y ~ r, data=df[df$d,]) # LOESS after the treatment
fitb <- loess(y ~ r, data=df[!(df$d),]) # LOESS before the treatment
rr <- seq(0.01, 2-0.01,length=100) # what to predict
preda <- predict(fita,rr)  
predb <- predict(fitb,-rr)

# plot the estimated LOESS line
plot(rev(-rr), rev(predb), type="l", lwd=2, col=2, xlab="r", ylab="y", xlim=c(-2, 2), ylim=c(-2, 2))
lines(rr, preda, lwd=2, col=3)

preda[1] - predb[length(predb)] # use the difference b/t the fitted value at 2-0.01, and 2+0.01
# this should be good enough!!

### not in the solution
# a more straight and naive way (but not theoretically supported)
ddd = df[abs(df$r)<0.1,] # consider the subset around 0.1 away from the treatment
ddd1 = ddd[ddd$r>0,] # the mean after treatment
ddd2 = ddd[ddd$r<0,] # the mean before treatment
mean(ddd1$y)-mean(ddd2$y)
# because you never know the true TE
# choose on 0.1 (the neighbour size) can be very arbitrary 




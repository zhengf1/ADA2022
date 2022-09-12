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

# install the required packages if you haven't done so.
install.packages("gamlr")
install.packages("tm")
install.packages('AER')
install.packages("wordcloud")

# load packages
library(gamlr)        # LASSO
library(tm)           # package for Text Mining
library(AER)          # for covHC (may not need)
library(wordcloud)    # package for word cloud plot

# -----------------------------------------------------------------
# Hold my beer: elasticity measure
# -----------------------------------------------------------------
# Today's tutorial will just be estimating the elasticity based on
# the sales data. Very straightforward, right?
# -----------------------------------------------------------------


#load("dominicks-beer.rda")
load(file=url("https://www.dropbox.com/s/8qry2ep99xwsm1z/dominicks-beer.rda?dl=1"))

# 2 files:
# upc. UPC means Unique Product Code. 
# wber. Weekly Beer sale data.

dim(upc)
head(upc)
# For the UPCs, we have available their size in beer volume
# (OZ, for fluid ounces) and a short text description.

dim(wber)
head(wber)
# PRICE is the average prices across 63 different stores.
# MOVE is the sale

# check data types
print(sapply(wber, class))
# or 
str(wber)

# how many upcs in ?
length( upctab <- table(wber$UPC) )
# It hints all UPC are shown in the sales record.

# calculate and work with the log price per 12 ounces.
wber$lp <- log(12*wber$PRICE/upc[wber$UPC,"OZ"]) #the row names are alternative index
# the product is normalized first and then logged.

# small sample
set.seed(888) # set seed to make the sub-sample reproducible
ss <- sample.int(nrow(wber),5000)

# how you can get a small elasticity:
# fitting a regression without controls yields a suspiciously
# small elasticity
coef( margfit <- lm(log(MOVE) ~ lp, data=wber[ss,]) )
# This says that sales drop by only 0.7% per every 1%
# increase. 
# Inelastic: increased prices are straight profit.
# suggest goods that are massively underpriced, 
# which is unrealistic for all beer

# Maybe endogeneity issues. 
# So, we better have some controls. 
# Say, dummy indicators for beer type, week, and store.
# This allows for both sales and price to vary as functions of
# beer type, transaction week, and store.
wber$s <- factor(wber$STORE)                     
wber$u <- factor(wber$UPC)                      
wber$w <- factor(wber$WEEK)                       
xs <- sparse.model.matrix( ~ s-1, data=wber)    # sparse matrix
xu <- sparse.model.matrix( ~ u-1, data=wber)
xw <- sparse.model.matrix( ~ w-1, data=wber)
# But what is the problem here?
# having completely separate models for every type of beer is poor practice:
# Bud Light 6-pack of cans is modeled as completely 
# independent from a Bud Light 12-pack of bottles
# then we will have little data for modeling each individual beer type

# -----------------------------------------------------------------
# Text Mining: keep it simple
# -----------------------------------------------------------------
# use packages "tm" 
# -----------------------------------------------------------------
descr <- Corpus(VectorSource(as.character(upc$DESCRIP)))
inspect(descr) # list of character

# make it as a sparse matrix
descr <- DocumentTermMatrix(descr)
dim(descr)
inspect(descr)
inspect(descr[100:110,])

# what does such a matrix look like?
class(descr)
ls(descr)
descr$i   # Integer vectors of row and column indices, respectively.
descr$j   # Integer vectors of row and column indices, respectively.
descr$v   # Vector of values.

# -----------------------------------------------------------------
# Word Clouds: just for fun (nothing to do with regression)
# -----------------------------------------------------------------
descr_matrix <- as.matrix(descr) 
words <- sort(colSums(descr_matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, colors=brewer.pal(8, "Dark2"),
          min.freq = 1, max.words=200, random.order=FALSE, scale=c(10,1) )


# -----------------------------------------------------------------
# convert from simple triplet matrix to Sparse Matrix format
# which can be used as controls in our regression
# -----------------------------------------------------------------
descr <- sparseMatrix(i=descr$i,j=descr$j,x=as.numeric(descr$v>0), dims=dim(descr),dimnames=list(rownames(upc),colnames(descr)))

print(descr[1:5,1:6])
print(descr[287,descr[287,]!=0])

# These terms encode a natural hierarchy. For example, many
# beers will be sold in 6-packs, but few will be from Oregon
# brewing, and even fewer will be red honey ales. This
# information, combined with week, store, and product
# indicators, forms our set of controls.

controls <- cbind(xs, xu, xw, descr[wber$UPC,]) 
dim(controls)

# -----------------------------------------------------------------
# Naive LASSO and ATE
# -----------------------------------------------------------------

naivefit <- gamlr(x=cbind(lp=wber$lp,controls)[ss,], y=log(wber$MOVE)[ss], free=1, standardize=FALSE)
# What does free=1 do here?
# - indices of the columns of x which will be unpenalized.
# Why don't we standardize here?
# - to interpret the elasticity 

print( coef(naivefit)[1:2,] )
# Is it more sensible?
# This is more realistic than the uncontrolled −0.7 elasticity
# found earlier. But can be further improved

# no SE availble unless splitting the sample or bootstrapping

# -----------------------------------------------------------------
# Orthogonal ML and ATE
# -----------------------------------------------------------------
# source("orthoML.R")
source("https://www.dropbox.com/s/4kp75xyfj3waxxp/orthoML.R?dl=1")
dreg <- function(x,d){ 
  gamlr(x, d, standardize=FALSE, lmr=1e-5) }

yreg <- function(x,y){ 
  gamlr(x, y, standardize=FALSE, lmr=1e-5) }

# Make sure to understand the algorithm for your own benefits as leisure
# algorithm 15 in the textbook
# Here we are just to implement them with a well-written function

# x: controls; d: treatment; y: price data
resids <- orthoPLTE( x=controls[ss,], d=wber$lp[ss], y=log(wber$MOVE)[ss], dreg=dreg, yreg=yreg, nfold=5)

# comment on the estimate and its SE
# - an average elasticity of between −3.1 and −3.7, 
# - which is in the range of values we would expect for beer
# - at least from the literature

# For comparison, we have the full sample of 1.6 million
# observations. This is enough data that we can reliably
# estimate all of the necessary week/store/UPC control effects
# using unbiased MLE estimation, and hence we can use this
# as a gold-standard elasticity estimate in this example.
fullfit <- gamlr(x=cbind(lp=wber$lp,controls), y=log(wber$MOVE), lambda.start=0)
# start from 0, then it must always be 0. LASSO->OLS
print( coef(fullfit)["lp",] )
# The full sample value is inside of the 90% interval provided
# by orthogonal ML on the small subsample.

# -----------------------------------------------------------------
# HTE
# -----------------------------------------------------------------
# Orthogonal ML
# -----------------------------------------------------------------

# Heterogeneous treatment effects are great for beer, but complicated to build. 
# It is the right thing to proceed.
# UPC description text as a source of heterogeneity
 
# create our interaction matrix with descriptions (words)
xhte <- cbind(BASELINE=1,descr[wber$UPC,]) 
# includes the bag-of-words matrix plus an intercept for gamma0

dmlhte <- gamlr(x=xhte[ss,]*resids$dtil, y=resids$ytil, free=1, standardize=FALSE)
# why free = 1?
print(coef(dmlhte)[1:2]) # intercept and baseline

# create a index, which can be used to find the 
# corresponding elasticity based on upc
eachbeer <- xhte[match(rownames(upc),wber$UPC),] #return the first match
rownames(eachbeer) <- rownames(upc)
dim(eachbeer)

print(range( gamdml <- drop(eachbeer%*%coef(dmlhte)[-1,]) ))
par(mfrow=c(1,1))
hist(gamdml, main="", xlab="elasticity", col="lightblue", freq=FALSE)
# What does the histogram mean?
# - Beer-specific elasticities obtained by lasso
# - regression on the orthogonal ML residuals.

B <- coef(dmlhte)[-(1:2),] # all coef except the intercept and the "main" effect.
B <- B[B!=0]
print(head(sort(round(B,2))))
print(head(sort(round(B,2), decreasing=TRUE)))
# We find that consumers (or potential consumers) of beers
# labeled “Draft,” as well as of “Guinness” or “Miller,” tend to
# be more price sensitive than the baseline (at least in the
# usual range of prices for these beers). This indicates that
# these are products that many consumers buy only when they
# are on sale. 

upc[names(sort(gamdml)[1:3]),] 
# The most elastic products are 24-packs of Miller Lite.
# what is gamdml?
# most elastic ones

# -----------------------------------------------------------------
# Naive LASSO
# -----------------------------------------------------------------

# we will try to recover the HTEs in a
# single regression (including the controls) without making use
# of the residualization and sample splitting of Orthogonal ML.
d <- xhte * wber$lp 
colnames(d) <- paste("lp",colnames(d),sep=":")

naivehte <- gamlr(x=cbind(d,controls)[ss,], y=log(wber$MOVE)[ss], free=1, standardize=FALSE)

gamnaive <- drop(eachbeer%*%coef(naivehte)[2:(ncol(d)+1),])
hist(gamnaive, main="", xlab="elasticity", col="lightyellow", freq=FALSE)
# The naive lasso estimates (i.e., without the benefit of
# orthogonalization) are highly peaked around values between
# −2 and −1.5

# -----------------------------------------------------------------
# MLE
# -----------------------------------------------------------------

mlehte <- gamlr(x=cbind(d,controls)[ss,], y=log(wber$MOVE)[ss], lambda.start=0)
gammle <- drop(eachbeer%*%coef(mlehte)[2:(ncol(d)+1),])
hist(gammle, main="", xlab="elasticity", breaks=200, 
     col="pink", xlim=c(-60,25), freq=FALSE)
sort(gammle)[1:4]
# The MLE elasticities are all over the place,
# ranging from massively negative to massively positive values
# as high as 20
# completely unrealistic.

# -----------------------------------------------------------------
# Note: before are all in sub-sample
# -----------------------------------------------------------------
# MLE full sample
# -----------------------------------------------------------------

# If you use the entire data, even MLE could work (under ignorability):
# Warning: this might take a long time (It took Colab 15 minutes.)

fullhte <- gamlr(x=cbind(d,controls), y=log(wber$MOVE), lambda.start=0)
gamfull <- drop(eachbeer%*%coef(fullhte)[2:(ncol(d)+1),])
hist(gamfull, main="", xlab="elasticity", col="darkgrey", freq=FALSE)
# does it take a long time?
# But this is not a great sign. Why?
upc[which(gamfull>0),]

# Compare the elasticity estimates:
ylim <- c(-8,1)
par(mai=c(.7,.7,.1,.1), mfrow=c(1,3))
plot(gamfull, gammle, pch=21, bg="pink", xlab="fulldata MLE", ylab="subsample MLE", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gammle))$r.squared))
plot(gamfull, gamnaive, pch=21, bg="lightyellow", xlab="fulldata MLE", ylab="subsample Naive ML", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gamnaive))$r.squared))
plot(gamfull, gamdml, pch=21, bg="lightblue", xlab="fulldata MLE", ylab="subsample Orthogonal ML", bty="n", ylim=ylim)
text(x=-6,y=1, sprintf("R2 = %.02f",summary(lm(gamfull~gamdml))$r.squared))

# -----------------------------------------------------------------
# Summary
# -----------------------------------------------------------------

# The orthogonal ML estimates are (by far) closest
# they appear roughly unbiased and, in an simple linear regression
# The naive lasso results are severely biased. 
# The MLE estimates are mostly noise.
















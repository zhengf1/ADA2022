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

install.packages('textir')
install.packages("wordcloud")
install.packages("maptpx")

library(textir)      # give us the data congress109
library(wordcloud)   # to produce word cloud
library(maptpx)      # estimation of topic models 

# ---------------------------------------------------------------------
# Word Clouds
# ---------------------------------------------------------------------

# The data is from "textir" package:
# from a study on how newspapers target content to 
# the politics of their readership

data(congress109)

# The text is already tokenized into bigrams (two-word phrases)
# eg. stem cell becomes ‘stem.cel’ 
#     and natural gas becomes ‘natural.ga’.

# term counts
dim(congress109Counts)

# Who says the most frequent term?
which(congress109Counts == max(congress109Counts), arr.ind = TRUE)
congress109Counts[394:396, 995:1000]

# info about each member of Congress
dim(congress109Ideology)
congress109Ideology[1:4,]
# The main variable of interest here will be repshare
# which is the proportion of the two-party vote
# larger repshare means more support to Bush
                                  
# covariance of words and repshare
dim(as.matrix(congress109Counts))
length(congress109Ideology$repshare)
word.cov = cov(as.matrix(congress109Counts),  # dim: 529*1000
               congress109Ideology$repshare)  # dim: 529
# note that, counts: 529*1000; repshare is a vector of 529 (n=529)
# here is calculating the covariance b/w each 1 of 1000 variable
# and rephare
# What's the dim of word.cov?
dim(word.cov)
# neg -> demo

set.seed(1234) # for reproducibility 
wordcloud(words = colnames(congress109Counts),  # colnames are all the variable names
          freq = colSums(congress109Counts),    # frequency of each words
          colors=c("blue","red")[(word.cov<0)+1], # red for negative; blue for positive
          min.freq = 10, max.words=100, random.order=TRUE )

# ---------------------------------------------------------------------
# Mid-semester Feedback and Topic Model
# ---------------------------------------------------------------------

# Topic models have been adopted instead of PCA for text
# because they tend to lead to more easily interpretable
# factorization. Interpretation remains subjective and messy.

# Once you have the text in a numeric format, the tools
# you’ve learned from elsewhere in this book give you a
# powerful framework for text analysis.

# ---------------------------------------------------------------------
# Q1(lecture):
Q = read.csv("https://www.dropbox.com/s/hxfwkj1r1whxrkr/Q1.csv?dl=1")
dim(Q)
# 14 obs, and 288 variables (text)

#######################################################
# 1. Draw a word cloud to peek into the data.
set.seed(1234) # for reproducibility 
wordcloud(words = colnames(Q), freq = colSums(Q), 
          min.freq = 1, max.words=100, random.order=TRUE )

#######################################################
# 2. Estimate a topic model (LDA) for  K=2,3,4,5 . 
# The sample size is small, so computational time is short.

# need to convert the data from a Matrix to a slam simple_triplet_matrix
# so that they can be in the same channel with a certain common language.
x <- as.simple_triplet_matrix(Q)

dim(x)

# the command uses a Bayes factor to choose the best-fitting size
tpc <- topics(x, K=2:5, verb=1)

plot(tpc)

# but how do we interpretate? It remains complicated as PCA. 
# work from bottom-up and top-down to build a narrative.

#######################################################
# 3. You cannot use the top-down method for analysis; why?

# For a top-down interpretation, you look at the fitted 
# vik and use domain knowledge about observation i to 
# build a narrative.

# but here we can not identify who is whom (anonymity).

#######################################################
# 4. Try the bottom-up method by investigating the topics. 
# Explain these topics in your words

# summary prints the top n words for each topic
summary(tpc, 10)
# how a topic's proportion  θjk  is different from the marginal distribution
# A large difference means a better way to identify the topic. 
# This will promote rare words that with high in-topic probability.

#######################################################
# 5. Draw bor plots of the most used words for each topic. 

# theta:  p×K  topic probabilities (like rotation/PC).
theta = tpc$theta[,1]
theta = theta[order(theta, decreasing = T)[1:15]]
barplot(theta, las=2)

theta = tpc$theta[,2]
theta = theta[order(theta, decreasing = T)[1:15]]
barplot(theta, las=2)

#######################################################
# 6. Compute each person's number of words and check its correlation to their topic weights.

# omega:  n×K  document topic weights (like factor).
tpc$omega
rowSums(Q)
X1 = cbind(tpc$omega, rowSums(Q))
colnames(X1)= c("v1", "v2", "n_words")
X1
cor(X1)

# ---------------------------------------------------------------------
# Q2(assessment):
Q = read.csv("https://www.dropbox.com/s/ryc264kkezqfbhb/Q2.csv?dl=1")
dim(Q)
# 14 obs, and 248 variables (text)

#######################################################
# 1. Draw a word cloud to peek into the data.
set.seed(1234) # for reproducibility 
wordcloud(words = colnames(Q), freq = colSums(Q), min.freq = 1, max.words=100, random.order=TRUE )

#######################################################
# 2. Estimate a topic model (LDA) for  K=2,3,4,5 . 
# The sample size is small, so computational time is short.

# need to convert the data from a Matrix to a slam simple_triplet_matrix
# so that they can be in the same channel with a certain common language.
x <- as.simple_triplet_matrix(Q)

dim(x)

# the command uses a Bayes factor to choose the best-fitting size
tpc <- topics(x, K=2:5, verb=1)

plot(tpc)

# but how do we interpretate? It remains complicated as PCA. 
# work from bottom-up and top-down to build a narrative.

#######################################################
# 3. You cannot use the top-down method for analysis; why?

# Top-down analysis is disabled because we can not identify who is whom (anonymity).

#######################################################
# 4. Try the bottom-up method by investigating the topics. 
# Explain these topics in your words

# summary prints the top n words for each topic
summary(tpc, 20)
# how a topic's proportion  θjk  is different from the marginal distribution
# A large difference means a better way to identify the topic. 
# This will promote rare words that with high in-topic probability.

#######################################################
# 5. Draw boxplots of the most used words for each topic. 

# theta:  p×K  topic probabilities (like rotation/PC).
theta = tpc$theta[,1]
theta = theta[order(theta, decreasing = T)[1:15]]
barplot(theta, las=2)

theta = tpc$theta[,2]
theta = theta[order(theta, decreasing = T)[1:20]]
barplot(theta, las=2)

#######################################################
# 6. Compute each person's number of words and check its correlation to their topic weights.

# omega:  n×K  document topic weights (like factor).
dim(Q)
Qsum = rowSums(Q)
ind0 = which(Qsum==0)
ind0

tpc_omega = as.data.frame(tpc$omega[1:9, ])
tpc_omega[10, ] = rep(NA, 2)                # Do you know why?
tpc_omega[11:14,] = tpc$omega[ind0:13, ]
tpc_omega

X2 = cbind(tpc_omega, rowSums(Q))
colnames(X2)= c("v1", "v2", "n_words")
X2

cor(na.omit(X2) )

# ---------------------------------------------------------------------
# Q3 (Tutorial):
Q = read.csv("https://www.dropbox.com/s/ykfuugn2zf0f9n8/Q3.csv?dl=1")
dim(Q)
# 14 obs, and 120 variables (text)

#######################################################
# 1. Draw a word cloud to peek into the data.
set.seed(1234) # for reproducibility 
wordcloud(words = colnames(Q), freq = colSums(Q), min.freq = 1, max.words=100, random.order=TRUE )

#######################################################
# 2. Estimate a topic model (LDA) for  K=2,3,4,5 . 
# The sample size is small, so computational time is short.

# need to convert the data from a Matrix to a slam simple_triplet_matrix
# so that they can be in the same channel with a certain common language.
x <- as.simple_triplet_matrix(Q)

dim(x)

# the command uses a Bayes factor to choose the best-fitting size
tpc <- topics(x, K=2:5, verb=1)

plot(tpc)

# but how do we interpretate? It remains complicated as PCA. 
# work from bottom-up and top-down to build a narrative.

#######################################################
# 3. You cannot use the top-down method for analysis; why?

# Top-down analysis is disabled because we can not identify who is whom (anonymity).

#######################################################
# 4. Try the bottom-up method by investigating the topics. 
# Explain these topics in your words

# summary prints the top n words for each topic
summary(tpc)
# how a topic's proportion  θjk  is different from the marginal distribution
# A large difference means a better way to identify the topic. 
# This will promote rare words that with high in-topic probability.

#######################################################
# 5. Draw boxplots of the most used words for each topic. 

# theta:  p×K  topic probabilities (like rotation/PC).
theta = tpc$theta[,1]
theta = theta[order(theta, decreasing = T)[1:15]]
barplot(theta, las=2)

theta = tpc$theta[,2]
theta = theta[order(theta, decreasing = T)[1:15]]
barplot(theta, las=2)

#######################################################
# 6. Compute each person's number of words and check its correlation to their topic weights.

# omega:  n×K  document topic weights (like factor).
tpc$omega

rowSums(Q)
X3 = cbind(tpc$omega, rowSums(Q))
colnames(X3)= c("v1", "v2", "n_words")
X3
# Correlation between topics and number of words
cor(X3)

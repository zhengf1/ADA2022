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

# install.packages("tree")
# install.packages("randomForest")
# install.packages("ranger") #faster

library(tree)
library(randomForest)  # it seems to be embedded in R now
library(ranger)        # claimed to be a faster version of RF
library(stringr)       # to use "split" function

# ---------------------------------------------------------------------
# Titanic
# ---------------------------------------------------------------------

df.train = read.csv("https://www.dropbox.com/s/i0kduxato6oxwyg/train.csv?dl=1")
# df.test = read.csv("https://www.dropbox.com/s/czbbrsrawir2vuy/test.csv?dl=1") 
# we do not predict. The competition is finished, so we cannot verify with the true data.

dim(df.train)

N = dim(df.train)[1]         # Number of observations
K = dim(df.train)[2]         # Number of variables

# check is there any NA entries
colSums(is.na(df.train))
# only "Age" has many missing data

# we do have prior knowledge that sex is key to survive
# make a balance check
table(df.train$Sex)          # number of F and M in the data
ind = is.na(df.train$Age)    # index of "Age == NA"
table(df.train$Sex[ind])     # number of F and M from "Age == NA"
# looks fine

# dependent variables
table(df.train$Survived)     # number of (not) survived
table(df.train$Pclass)       # number of people in each class
# does class affect survival possibility?
table(df.train$Survived, df.train$Pclass)

fit = glm(Survived ~ Age * Sex, data=df.train)
summary(fit)

# ---------------------------------------------------------------------
# Women (sex) and Children (age) first:
# ---------------------------------------------------------------------
# The dependent variable is Survived. 
# Use *Age* and *Sex* to ***grow a tree***.
# Keep in mind that *Age* has missing values
# ---------------------------------------------------------------------

df = df.train[complete.cases(df.train), ]    # data without NA rows
df$Sex = as.factor(df$Sex)
df$Survived = as.factor(df$Survived)
df.ss = df # cannot use df as a name in next cv.tree.

# grow a tree, simply use **tree**
titanic = tree(Survived ~ Age + Sex, data=df.ss, mincut = 1)

levels(df$Sex)

# hist(df$Age)
mean(df$Age<=13)

# Plot the tree
plot(titanic, col=2)
text(titanic, label="yprob")

# Cross-Validation to prune the tree
titanic.cv = cv.tree(titanic, K = 10)

plot(titanic.cv$size, titanic.cv$dev, xlab="size", ylab="oos deviance", pch=21, bg="lightblue", bty="n")
titanic.cv$size[which.min(titanic.cv$dev)]

# So we can save the pruned tree according to this recommendation
# note that *randomness* exists
titanic.prune = prune.tree(titanic, best=2)
plot(titanic.prune, col=2)
text(titanic.prune, label="yprob")

# ---------------------------------------------------------------------
# Titles:
# ---------------------------------------------------------------------
# The variable Name has titles such as Miss or Mr. Extract these titles.
# Which variable is more important? Title or Sex?
# ---------------------------------------------------------------------
head(df.train$Name)
# to get Mr (or Miss and etc), we need to keep things after ","

split1 = as.data.frame(str_split_fixed(df.train$Name, ",", n=2))
head(split1)
# then get the title before the ".space" in the second column

split2 = str_split_fixed(split1[,2], "\\.", 2)
head(split2)
# the 1st column is title
titles = split2[,1]
print(table(titles))

# Don is a title for men in Spanish
# Jonkheer is literally translated as 'young lord' 
#   or 'young lady'. In the Middle Ages, such a person was a young 
#   and unmarried child of a high-ranking knight or nobleman
# Rev is an officially appointed religious leader

length(titles)

# Let's grow another tree with these titles.
df          = df.train                 # New dataset to add titles to
df$Titles   = as.factor(titles)        # Adding titles to the data
df          = df[complete.cases(df), ] # Preserving non-missing obs only
df$Survived = as.factor(df$Survived)   # Turning Survived into a factor

# grow the tree again
titanic = tree(Survived ~ Age + Titles, data=df, mincut = 5)

levels(df$Title)
titanic

# plot the tree
plot(titanic, col=2)
text(titanic, label="yprob")

## if just use titles
df = df.train
df$Titles = as.factor(titles)
df$Survived = as.factor(df$Survived)
# there is no need to truncate the data
# because NA only in age, and we don't need age

# grow the tree
titanic = tree(Survived~ Titles, data=df, mincut = 5)
levels(df$Title)
titanic

# plot the tree
plot(titanic, col=2)
text(titanic, label="yprob")

## use titles and sex
df = df.train
df$Titles = as.factor(titles)
df$Survived = as.factor(df$Survived)

# grow the tree
titanic = tree(Survived ~ Sex + Titles, data=df, mincut = 5)
levels(df$Title)
titanic

# plot the tree
plot(titanic, col=2)
text(titanic, label="yprob")

# all the trees are identical, which perhaps suggests
# title includes most of the information about age, sex
# so not a very superising thing!

# ---------------------------------------------------------------------
# All together:
# ---------------------------------------------------------------------
# All data but Age, so we can use all observations
# ---------------------------------------------------------------------

## all but Age
df = df.train
df$Titles = as.factor(titles)
df$PassengerId = NULL
df$Survived = as.factor(df$Survived)
df$Name = NULL
df$Sex = as.factor(df$Sex)
df$Age = NULL
df$Ticket = NULL
df$Cabin = NULL
df$Fare = NULL
df$Embarked = NULL

df1 = df
# grow the tree
titanic = tree(Survived~ ., data=df1, mincut = 5)
titanic

# plot the tree
plot(titanic, col=2)
text(titanic, label="yprob")

levels(df$Titles)

# cross-validation to prune the tree 
titanic.cv = cv.tree(titanic, K = 10)

plot(titanic.cv$size, titanic.cv$dev, xlab="size", ylab="oos deviance", pch=21, bg="lightblue", bty="n")
titanic.cv$size[which.min(titanic.cv$dev)]
# no need to trim (but subject to randomness).

titanic.prune = prune.tree(titanic, best=5)
plot(titanic.prune, col=2)
text(titanic.prune, label="yprob")













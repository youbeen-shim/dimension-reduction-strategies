# Homework 2              
# Youbeen Shim (YS3EP)
###########################

library(tidyverse)

# Read in stocks data
stocks<- read_csv("stocks.csv")
colnames(stocks)[1] <- 'Date' #changing var name
head(stocks)

##############
# Question 1

# Perform Principle Component Analysis:
# Data is already centered and standardized.
pca = prcomp(stocks[,-1]) # to not include date (first column)
class(pca) # prcomp class
summary(pca)
# PC1 is explaining ~36% of the variation
ls(pca) # center, rotation, scale, sdev, x

# The first column of rotation (PC1) gives direction of maximal variance of data
# (first principle component).

# Create visualization that reports the weights of this direction, and interpret
# it. Create a tibble with 2 columns, the first being the name of the stocks,
# the second being the weights from the first principle component

pca1 = tibble(Stock=colnames(stocks)[2:23], PCA1=pca$rotation[,1])
pca1 %>%
  ggplot(aes(PCA1,Stock)) +
  geom_point(color="blue") + xlab("Weight(s) from 1st Principle Component") + ylab("Stock") +
  labs(title="Loading Plot of 1st Principle Component") + 
  theme_minimal()
pca1
#--- Extra Stuff:
# Representation of how much variance each component showing.. usually
# use this to pick which components to keep.
screeplot(pca,type="line",main="Scree Plot")

# Biplot:
library(ggplot2)
biplot(pca,main="Biplot of Stocks",cex=c(0.5,0.8))

# Well the first principle component will captures as much of the variance
# that it can while the following principle component or the second principle
# component would capture the rest of it of it that it can, and so on with
# the succeeding components.
# Proximity of plotted eigenvectors shows how correlated they are (if they are
# closer together, they are probably highly correlated)

##############
# Question 2 #
##############
# Using pairs, explore the bivariate relationship between stocks in
# dataset. Include one set (around 4 or so) of interesting stocks in the
# writeup and discuss. Note: set pch=16 in the pairs function, otherwise
# it doesn't look good. 
pairs(stocks[,c("MO","YHOO")], pch=16) # Uncorrelated
pairs(stocks[,c("F","GE","EFX","BAC")], pch=16) # Correlated
pairs(stocks[,c("GE","GS","HAL","CXW")], pch=16) # Correlated
# Does there seem to be a relationship between the weights of the principle
# component and 'interesting' pairwise plots?


##############
# Question 3 #
##############
# Create a tibble with one column being date and the second column being
# these projections onto the first principle components.
ls(pca)
pca$x

q3 = tibble(Date=stocks$Date, PCA1Projections=pca$x[,1])

# Plot the projection against date.
q3 %>%
  ggplot(aes(Date,PCA1Projections)) +
  geom_point(color="blue", size=.3) + ylab("Projections of Data onto PC1") +
  labs(title="Date vs Projections of Data onto PC1") + 
  theme_minimal()

# What patterns do you see? Does this line up with what you know about the
# history of the stock market?
#--- Stock market crash

# Without looking at the data and hand labelling, how might you go about
# dividing your data points into two distinct groups?


# Are principle components a good way to classify the data?


##############
# Question 4 #
##############
install.packages("mixtools")
library(mixtools)

# Run "mvnormalmixEM" on your data to infer a mixture model of 2 multivariate
# gaussians. You control # of gaussians by parameter "k", which by default
# is set to 2, so nothing to do here. 
mix = mvnormalmixEM(stocks[,-1])

# What are the variables in the object returned by the "mvnormalmixEM" function?
ls(mix)
#[1] "all.loglik" "ft"         "lambda"     "loglik"     "mu"         "posterior"  "restarts"  
#[8] "sigma"      "x"

# The variable 'posterior' indicates the posterior probability of each individual data 
# point (rows) coming from each gaussian (columns). Because there are only 2 gaussians,
# however, you need only one column to fully specify the posterior probabilities (the 
# rows should sum to 1). 
mix$posterior

# We can sort data points by, for instance, considering a data point to belong to the gaussian
# with the highest posterior probability. Do this, in the process creating a tibble with the
# following variables (naming them appropriately): date, projection on the first
# principle component, and gaussian membership. Plot the projection onto the first 
# principle component again (as in question 3) but this time color by gaussian membership.

q4 <- tibble(Date=stocks$Date, PCA1Projections=pca$x[,1],
             Gauss1=mix$posterior[,1], Gauss2=mix$posterior[,2])

q4 %>%
  mutate(Membership=ifelse(Gauss1 > Gauss2, "Gaussian 1", "Gaussian 2")) %>%
  ggplot(aes(Date,PCA1Projections)) +
  geom_point(aes(color=Membership)) + ylab("Projections of Data onto PC1") +
  labs(title="Date vs Projections of Data onto PC1")

# Discuss. Do you think that the multivariate gaussian mixture model is a good model of the data?
# What's considered good, bad?
# gaussian 1 are the points that cater more to pca 2 and gaussian 2 are the points that cater more to pca 1
# so the points taht adhere to the expected linear trend fall under gaussian 2

##############
# Question 5 #
##############
# Return to pairs plot. Explore, then include in your writeup a visualization of interesting
# pairs of variables, this time conditioning on each multivariate gaussian (in other words,
# two sets of pairs plots, one for data points in one of the multivariate gaussians, and the
# second for data points in the other). 

pairs(stocks[,c("AAPL","PG")], pch=16) # Uncorrelated
pairs(stocks[,c("GE","F")], pch=16) # Highly correlated


pairs(stocks[,c("GE","GS","HAL","CXW")], pch=16) # Correlated

q4 <- tibble(Date=stocks$Date, PCA1Projections=pca$x[,1], Gauss1=mix$posterior[,1], Gauss2=mix$posterior[,2])

q4 %>%
  mutate(Membership=ifelse(Gauss1 > Gauss2, "Gaussian 1", "Gaussian 2")) %>%
  ggplot(aes(Date,PCA1Projections)) +
  geom_point(aes(color=Membership)) + ylab("Projections of Data onto PC1") +
  labs(title="Date vs Projections of Data onto PC1")

# Discuss.

#############
# Challenge #
#############

hm %>%
  mutate(Membership=ifelse(Gauss1 > Gauss2, "Gaussian 1", "Gaussian 2")) %>%
  ggplot(aes(Date,PCA1Projections)) +
  geom_point(aes(color=Membership)) + ylab("Projections of Data onto PC1") +
  labs(title="Date vs Projections of Data onto PC1")
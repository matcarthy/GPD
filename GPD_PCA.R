#pricipal components analysis


# Splitting the dataset into the Training set and Test set (Iterate for each DV)
grep("tot.aversion.bins", colnames(gpd12nomiss))

library(caTools)

set.seed(123)
split12 <- sample.split(gpd12nomiss$tot.aver.cl, SplitRatio = 0.8)
training12 <- subset(gpd12nomiss, split12 == TRUE)
test12 <- subset(gpd12nomiss, split12 == FALSE)

#scale
training12[-102:103] <- scale(training12[-102:103])
test12[-102:103] <- scale(test12[-102:103])

#pca
library(caret)
library(e1071)

pca12 <- preProcess(x = training12[-102:103], method = 'pca', pcaComp = 2)
training12 <- predict(pca12, training12)
test12 <- predict(pca12, test12)

set.seed(123)
split14 <- sample.split(gpd14nomiss$tot.aver.cl, SplitRatio = 0.8)
training14 <- subset(gpd14nomiss, split14 == TRUE)
test14 <- subset(gpd14nomiss, split14 == FALSE)

#scale
training14[-103:106] <- scale(training14[-103:106])
test14[-103:106] <- scale(test14[-103:106])

#pca

pca14 <- preProcess(x = training14[-103:106], method = 'pca', pcaComp = 2)
training14 <- predict(pca14, training14)
test14 <- predict(pca14, test14)


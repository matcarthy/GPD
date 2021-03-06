#pricipal components analysis
##Select non correlated features

gpd12.pca1 <- gpd12nomiss %>%
  select(1:79)

gpd12.pca2 <- gpd12nomiss %>%
  select(104:112)

gpd12.pca1$tot.aversion.binary <- gpd12.pca2$tot.aversion.binary

gpd14.pca1 <- gpd14nomiss %>%
  select(1:59)

gpd14.pca2 <- gpd14nomiss %>%
  select(76:108)

gpd14.pca1$tot.trust.binary <- gpd14.pca2$tot.trust.binary
gpd14.pca1$tot.aversion.binary <- gpd14.pca2$tot.aversion.binary

## Splitting the dataset into the Training set and Test set (Iterate for each DV)
#grep("tot.aversion.bins", colnames(gpd14nomiss))

library(caTools)

set.seed(123)
split12 <- sample.split(gpd12.pca2$tot.aversion.binary, SplitRatio = 0.8)
training12 <- subset(gpd12.pca2, split12 == TRUE)
test12 <- subset(gpd12.pca2, split12 == FALSE)

##scale
training12[-10] <- scale(training12[-10])
test12[-10] <- scale(test12[-10])

##pca
library(caret)
library(e1071)

pca12 <- preProcess(x = training12[-10], method = 'pca', pcaComp = 2)
training12 <- predict(pca12, training12)
test12 <- predict(pca12, test12)

set.seed(123)
split14.1 <- sample.split(gpd14.pca1$tot.trust.binary , SplitRatio = 0.8)
training14.1 <- subset(gpd14.pca1, split14.1 == TRUE)
test14.1 <- subset(gpd14.pca1, split14.1 == FALSE)

set.seed(123)
split14.2 <- sample.split(gpd14.pca2$tot.trust.binary , SplitRatio = 0.8)
training14.2 <- subset(gpd14.pca2, split14.2 == TRUE)
test14.2 <- subset(gpd14.pca2, split14.2 == FALSE)

##scale
#training14[-60] <- scale(training14[-60])
#test14[-60] <- scale(test14[-60])

##pca

pca14.1 <- preProcess(x = training14.1[-60], method = 'pca', pcaComp = 2)
training14.1 <- predict(pca14.1, training14.1)
test14.1 <- predict(pca14.1, test14.1)

pca14.2 <- preProcess(x = training14.2[-33], method = 'pca', pcaComp = 2)
training14.2 <- predict(pca14.2, training14.2)
test14.2 <- predict(pca14.2, test14.2)


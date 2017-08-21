#2014 wave feature engineering
library(psych)
##Knowlege priv. policy
gpd14nomiss$know.pp.idx <- gpd14nomiss$S14_4 + gpd14nomiss$S14_5 + gpd14nomiss$S14_8 + gpd14nomiss$S14_9

gpd14nomiss$know.pp.scale <- gpd14nomiss$know.pp.idx/4

alpha(data.frame(gpd14nomiss$S14_4, gpd14nomiss$S14_5, gpd14nomiss$S14_8, gpd14nomiss$S14_9))


##knowledge tech
gpd14nomiss$know.tech.idx <- gpd14nomiss$S3 + gpd14nomiss$S4

gpd14nomiss$know.tech.scale <- gpd14nomiss$know.tech.idx/2

cor(gpd14nomiss$S3, gpd14nomiss$S4)

##Total Know
gpd14nomiss$tot.know.idx <- gpd14nomiss$know.pp.idx + gpd14nomiss$know.tech.idx

##Orientation to appropriateness/aversion
###Employee monitoring Split, bad alpha
gpd14nomiss$emp.mon.idx <- gpd14nomiss$S13_5 + gpd14nomiss$S13_8 + gpd14nomiss$S13_10

gpd14nomiss$emp.mon.scale <- gpd14nomiss$emp.mon.idx/3

alpha(data.frame(gpd14nomiss$S13_5, gpd14nomiss$S13_8, gpd14nomiss$S13_10))

###Corporate data tracking
gpd14nomiss$corp.track.idx <- gpd14nomiss$S13_0 + gpd14nomiss$S13_1 + gpd14nomiss$S13_2 + 
  gpd14nomiss$S13_3 + gpd14nomiss$S13_6 + gpd14nomiss$S13_9 + gpd14nomiss$S13_11

gpd14nomiss$corp.track.scale <- gpd14nomiss$corp.track.idx/7

alpha(data.frame(gpd14nomiss$S13_0, gpd14nomiss$S13_1, gpd14nomiss$S13_2, 
                 gpd14nomiss$S13_3, gpd14nomiss$S13_6, gpd14nomiss$S13_9, gpd14nomiss$S13_11))

###Police/state Surveillance for legit reasons
gpd14nomiss$police.idx <- gpd14nomiss$S13_4 + gpd14nomiss$S13_7 + gpd14nomiss$S13_14 

gpd14nomiss$police.scale <- gpd14nomiss$police.idx/4

alpha(data.frame(gpd14nomiss$S13_4, gpd14nomiss$S13_7, gpd14nomiss$S13_14))

###Total Aversion
gpd14nomiss$tot.aversion.idx <- gpd14nomiss$police.idx + gpd14nomiss$corp.track.idx + gpd14nomiss$S13_5 + gpd14nomiss$S13_8 + gpd14nomiss$S13_10

alpha(data.frame(gpd14nomiss$S13_4, gpd14nomiss$S13_7, gpd14nomiss$S13_14, gpd14nomiss$S13_0,
                 gpd14nomiss$S13_1, gpd14nomiss$S13_2,gpd14nomiss$S13_3, gpd14nomiss$S13_6,
                 gpd14nomiss$S13_9, gpd14nomiss$S13_11, gpd14nomiss$S13_5, gpd14nomiss$S13_8, gpd14nomiss$S13_10))

##Taking action remove from lists, refused to answer Split, bad alpha
gpd14nomiss$action.idx <- gpd14nomiss$S14_0 + gpd14nomiss$S14_1 + gpd14nomiss$S14_2 + gpd14nomiss$S14_3

gpd14nomiss$action.scale <- gpd14nomiss$action.idx/4

alpha(data.frame(gpd14nomiss$S14_0, gpd14nomiss$S14_1, gpd14nomiss$S14_2, gpd14nomiss$S14_3))

##Trust
###Corp Split low cor. 
gpd14nomiss$trust.corp.idx <- gpd14nomiss$S8 + gpd14nomiss$S20

gpd14nomiss$trust.corp.scale <- gpd14nomiss$trust.corp.idx/2

cor(gpd14nomiss$S8, gpd14nomiss$S20)

###State

gpd14nomiss$trust.state.idx <- gpd14nomiss$S18 + gpd14nomiss$S19

gpd14nomiss$trust.state.scale <- gpd14nomiss$trust.state.idx/2

cor(gpd14nomiss$S18, gpd14nomiss$S19)

###Total Trust
gpd14nomiss$tot.trust.idx <- gpd14nomiss$trust.state.idx + gpd14nomiss$trust.corp.idx + gpd14nomiss$S10

alpha(data.frame(gpd14nomiss$S8, gpd14nomiss$S20, gpd14nomiss$S18, gpd14nomiss$S19, gpd14nomiss$S10))

###creating dendrogram to find clusters for tot.aversion

x <- (data.frame(gpd14nomiss$police.idx + gpd14nomiss$corp.track.idx + gpd14nomiss$emp.mon.idx))

dendrogram <- hclust(dist(x, method = 'euclidean'), method = "ward.D2")

plot(dendrogram, main = paste('dendrogram'), xlab = 'aversion', ylab = 'distance')

#####fit HC to data
hc <- hclust(dist(x, method = 'euclidean'), method = "ward.D2")

gpd14nomiss$tot.aver.cl <- cutree(hc, k = 3)

#####visualizing
library(cluster)
clusplot(x,
         gpd14nomiss$tot.aver.cl,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE)

plot(gpd14nomiss$tot.aver.cl, gpd14nomiss$tot.aversion.idx)

gpd14nomiss$tot.aver.cl <- as.factor(gpd14nomiss$tot.aver.cl)

####Eyeball clusters w/bins 
hist(gpd14nomiss$tot.aversion.idx)
gpd14nomiss$tot.aversion.bins <- NA


gpd14nomiss$tot.aversion.bins <- cut(gpd14nomiss$tot.aversion.idx, c(0, 4, 10, 13), labels=c(1:3))

table(gpd14nomiss$tot.aversion.bins)

library(caret)
confusionMatrix(gpd14nomiss$tot.aver.cl, gpd14nomiss$tot.aversion.bins)


###creating dendrogram to find clusters for tot.trust

y <- (data.frame(gpd14nomiss$trust.state.idx + gpd14nomiss$trust.corp.idx + gpd14nomiss$S10))

dendrogram <- hclust(dist(y, method = 'euclidean'), method = "ward.D")

plot(dendrogram, main = paste('dendrogram'), xlab = 'trust', ylab = 'distance')

#####fit HC to data
hc <- hclust(dist(y, method = 'euclidean'), method = "ward.D")

gpd14nomiss$tot.trust.cl <- cutree(hc, k = 3)

#####visualizing
library(cluster)
clusplot(y,
         gpd14nomiss$tot.trust.cl,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE)

plot(gpd14nomiss$tot.trust.cl, gpd14nomiss$tot.trust.idx)

gpd14nomiss$tot.trust.cl <- as.factor(gpd14nomiss$tot.trust.cl)

####Eyeball clusters w/bins 
hist(gpd14nomiss$tot.trust.idx)
gpd14nomiss$tot.trust.bins <- NA

gpd14nomiss$tot.trust.bins <- cut(gpd14nomiss$tot.trust.idx, c(0, 1, 6, 15), labels=c(1:3))

table(gpd14nomiss$tot.trust.bins)

confusionMatrix(gpd14nomiss$tot.trust.cl, gpd14nomiss$tot.trust.bins)

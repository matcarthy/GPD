# 2012 wave Feature Engineering 
library(psych)
##Knowlege tech
gpd12nomiss$know.idx <- gpd12nomiss$Q1_0 + gpd12nomiss$Q1_1 + gpd12nomiss$Q1_2 +
  gpd12nomiss$Q1_3 + gpd12nomiss$Q1_4 + gpd12nomiss$Q1_5

gpd12nomiss$know.scale <- gpd12nomiss$know.idx/6

alpha(data.frame(gpd12nomiss$Q1_0, gpd12nomiss$Q1_1, gpd12nomiss$Q1_2, 
                 gpd12nomiss$Q1_3, gpd12nomiss$Q1_4, gpd12nomiss$Q1_5))

##knowledge law
gpd12nomiss$know.law.idx <- gpd12nomiss$Q8_GD + gpd12nomiss$Q8_PC

gpd12nomiss$know.law.scale <- gpd12nomiss$know.law.idx/2

cor(gpd12nomiss$Q8_GD, gpd12nomiss$Q8_PC)

##knowledge privacy policy data/use
gpd12nomiss$know.pp.idx <- gpd12nomiss$Q19_4 + gpd12nomiss$Q19_5 + gpd12nomiss$Q19_8 + 
  gpd12nomiss$Q19_9

gpd12nomiss$know.pp.scale <- gpd12nomiss$know.pp.idx/4


alpha(data.frame(gpd12nomiss$Q19_4, gpd12nomiss$Q19_5, gpd12nomiss$Q19_8, gpd12nomiss$Q19_9))

##Effectiveness (meh .67 maybe split)
gpd12nomiss$effective.idx <- gpd12nomiss$Q10_0 + gpd12nomiss$Q10_1

gpd12nomiss$effective.scale <- gpd12nomiss$effective.idx/2

cor(gpd12nomiss$Q10_0, gpd12nomiss$Q10_1)

##Orientation to appropriateness/aversion
###Employee monitoring
gpd12nomiss$emp.mon.idx <- gpd12nomiss$Q11_0 + gpd12nomiss$Q11_1 + gpd12nomiss$Q11_2

gpd12nomiss$emp.mon.scale <- gpd12nomiss$emp.mon.idx/3

alpha(data.frame(gpd12nomiss$Q11_0, gpd12nomiss$Q11_1, gpd12nomiss$Q11_2))

###Employer sharing data
gpd12nomiss$emp.share.idx <- gpd12nomiss$Q12_0 + gpd12nomiss$Q12_1

gpd12nomiss$emp.share.scale <- gpd12nomiss$emp.share.idx/2

cor(gpd12nomiss$Q12_0, gpd12nomiss$Q12_1)

###Corporate data sharing
gpd12nomiss$corp.share.idx <- gpd12nomiss$Q16_0 + gpd12nomiss$Q16_1 + gpd12nomiss$Q16_2 +
  gpd12nomiss$Q16_3

gpd12nomiss$corp.share.scale <- gpd12nomiss$corp.share.idx/4

alpha(data.frame(gpd12nomiss$Q16_0, gpd12nomiss$Q16_1, gpd12nomiss$Q16_2, gpd12nomiss$Q16_3))

###Police Surveillance
gpd12nomiss$police.idx <- gpd12nomiss$Q17_0 + gpd12nomiss$Q17_1 + gpd12nomiss$Q17_2 + gpd12nomiss$Q17_3

gpd12nomiss$police.scale <- gpd12nomiss$police.idx/4

alpha(data.frame(gpd12nomiss$Q17_0, gpd12nomiss$Q17_1, gpd12nomiss$Q17_2, gpd12nomiss$Q17_3))

##Taking action
### Asked to remove from lists (no .58 def split)
gpd12nomiss$removed <- gpd12nomiss$Q19_2 + gpd12nomiss$Q19_3

cor(gpd12nomiss$Q19_2, gpd12nomiss$Q19_3)

## Willingness to be exposed (only on limited data frame w/ high missingness)
#gpd12nomiss$will.exp.idx <- gpd12nomiss$Q14_0 + gpd12nomiss$Q14_1 + gpd12nomiss$Q14_2 

#gpd12nomiss$will.exp.scale <- gpd12nomiss$will.exp.idx/3

#alpha(data.frame(gpd12nomiss$Q14_0, gpd12nomiss$Q14_1, gpd12nomiss$Q14_2))

## ActualExposure
gpd12nomiss$actual.exp.idx <- gpd12nomiss$Q2_0 + gpd12nomiss$Q2_1 + gpd12nomiss$Q2_2 + gpd12nomiss$Q2_3 +
  gpd12nomiss$Q2_4  

gpd12nomiss$actual.exp.scale <- gpd12nomiss$actual.exp.idx/5

alpha(data.frame(gpd12nomiss$Q2_0, gpd12nomiss$Q2_1, gpd12nomiss$Q2_2, gpd12nomiss$Q2_3,
                 gpd12nomiss$Q2_4))

## taken measure to protect info online
gpd12nomiss$protect.idx <- gpd12nomiss$Q20_0 + gpd12nomiss$Q20_1 + gpd12nomiss$Q20_2 + gpd12nomiss$Q20_3

gpd12nomiss$protect.scale <- gpd12nomiss$protect.idx/4

alpha(data.frame(gpd12nomiss$Q20_0, gpd12nomiss$Q20_1, gpd12nomiss$Q20_2, gpd12nomiss$Q20_3))


##Total Taking action 

gpd12nomiss$tot.action.idx <- gpd12nomiss$Q19_2 + gpd12nomiss$Q19_3 + gpd12nomiss$Q20_0 + 
  gpd12nomiss$Q20_1 + gpd12nomiss$Q20_2 + gpd12nomiss$Q20_3

gpd12nomiss$tot.action.scale <- gpd12nomiss$tot.action.idx/6

alpha(data.frame(gpd12nomiss$Q19_2, gpd12nomiss$Q19_3, gpd12nomiss$Q20_0, gpd12nomiss$Q20_1,
                 gpd12nomiss$Q20_2, gpd12nomiss$Q20_3))

### Total aversion var 
gpd12nomiss$tot.aversion.idx <- gpd12nomiss$emp.mon.idx + gpd12nomiss$police.idx + gpd12nomiss$corp.share.idx +
  gpd12nomiss$emp.share.idx 

gpd12nomiss$tot.aversion.scale <- gpd12nomiss$tot.aversion.idx/4

alpha(data.frame(gpd12nomiss$emp.mon.idx, gpd12nomiss$police.idx, gpd12nomiss$corp.share.idx, 
                 gpd12nomiss$emp.share.idx))

###creating dendrogram to find clusters for tot.aversion

x <- (data.frame(gpd12nomiss$emp.mon.idx, gpd12nomiss$police.idx, gpd12nomiss$corp.share.idx,
                 gpd12nomiss$emp.share.idx))

dendrogram <- hclust(dist(x, method = 'euclidean'), method = "ward.D2")

plot(dendrogram, main = paste('dendrogram'), xlab = 'aversion', ylab = 'distance')

#####fit HC to data
hc <- hclust(dist(x, method = 'euclidean'), method = "ward.D2")

gpd12nomiss$tot.av.cl <- cutree(hc, k = 3)

#####visualizing
library(cluster)
clusplot(x,
         gpd12nomiss$tot.av.cl,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE)

gpd12nomiss$tot.av.cl <- as.factor(gpd12nomiss$tot.av.cl)

####Eyeball clusters w/bins 
hist(gpd12nomiss$tot.aversion.idx)
gpd12nomiss$tot.aversion.bins <- NA


gpd12nomiss$tot.aversion.bins <- cut(gpd12nomiss$tot.aversion.idx, c(0, 30, 49, 75), labels=c(1:3))


table(gpd12nomiss$tot.aversion.bins)

library(caret)
confusionMatrix(gpd12nomiss$tot.av.cl, gpd12nomiss$tot.aversion.bins)



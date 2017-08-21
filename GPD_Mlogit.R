#Analyses
##Mlogit 2012
library(nnet)

mlog12.1 <- multinom(gpd12nomiss$tot.av.cl ~ as.factor(gpd12nomiss$Q19_2) + as.factor(gpd12nomiss$Q19_3) + 
                       scale(gpd12nomiss$effective.idx) + scale(gpd12nomiss$know.law.idx) + scale(gpd12nomiss$know.idx) + gpd12nomiss$agesex.f +
                       gpd12nomiss$Education_Level.f + scale(gpd12nomiss$protect.idx) + scale(gpd12nomiss$actual.exp.idx) +
                       scale(gpd12nomiss$know.pp.idx), data = gpd12nomiss, weights = gpd12nomiss$wgt)

summary(mlog12.1)

z12.1 <- summary(mlog12.1)$coefficients/summary(mlog12.1)$standard.errors
p12.1 <- (1 - pnorm(abs(z12.1), 0, 1)) * 2
p12.1


mlog12.2 <- multinom(gpd12nomiss$tot.av.cl ~ as.factor(gpd12nomiss$Q19_2) + 
                       scale(gpd12nomiss$effective.idx) + scale(gpd12nomiss$know.law.idx) + gpd12nomiss$agesex.f, data = gpd12nomiss, weights = gpd12nomiss$wgt)

summary(mlog12.2)

z12.2 <- summary(mlog12.2)$coefficients/summary(mlog12.2)$standard.errors
p12.2 <- (1 - pnorm(abs(z12.2), 0, 1)) * 2
p12.2

mlog12.3 <- multinom(gpd12nomiss$tot.aversion.bins ~ as.factor(gpd12nomiss$Q19_2) + as.factor(gpd12nomiss$Q19_3) + 
                       scale(gpd12nomiss$effective.idx) + scale(gpd12nomiss$know.law.idx) + scale(gpd12nomiss$know.idx) + gpd12nomiss$agesex.f +
                       gpd12nomiss$Education_Level.f + scale(gpd12nomiss$protect.idx) + scale(gpd12nomiss$actual.exp.idx) +
                       scale(gpd12nomiss$know.pp.idx), data = gpd12nomiss, weights = gpd12nomiss$wgt)

summary(mlog12.3)

z12.3 <- summary(mlog12.3)$coefficients/summary(mlog12.3)$standard.errors
p12.3 <- (1 - pnorm(abs(z12.3), 0, 1)) * 2
p12.3


mlog12.4 <- multinom(gpd12nomiss$tot.aversion.bins ~ as.factor(gpd12nomiss$Q19_2) +  
                       scale(gpd12nomiss$effective.idx) + scale(gpd12nomiss$know.law.idx) + scale(gpd12nomiss$know.idx) + gpd12nomiss$agesex.f +
                       gpd12nomiss$Education_Level.f + scale(gpd12nomiss$actual.exp.idx), data = gpd12nomiss, weights = gpd12nomiss$wgt)

summary(mlog12.4)

z12.4 <- summary(mlog12.4)$coefficients/summary(mlog12.4)$standard.errors
p12.4 <- (1 - pnorm(abs(z12.4), 0, 1)) * 2
p12.4


##mlogit 2014

mlog14.1 <- multinom(gpd14nomiss$tot.aver.cl ~ gpd14nomiss$tot.trust.idx + gpd14nomiss$know.pp.idx + gpd14nomiss$know.tech.idx +
                       as.factor(gpd14nomiss$S14_0) + as.factor(gpd14nomiss$S14_1) + as.factor(gpd14nomiss$S14_2) +
                       as.factor(gpd14nomiss$S14_3) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + as.factor(gpd14nomiss$S17) +
                       as.factor(gpd14nomiss$S6) + gpd14nomiss$race.f + gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$income.f +
                       gpd14nomiss$ed.f + gpd14nomiss$age.f + gpd14nomiss$gender.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)

summary(mlog14.1)

z14.1 <- summary(mlog14.1)$coefficients/summary(mlog14.1)$standard.errors
p14.1 <- (1 - pnorm(abs(z14.1), 0, 1)) * 2
p14.1

mlog14.2 <- multinom(gpd14nomiss$tot.aver.cl ~ gpd14nomiss$tot.trust.idx +
                       as.factor(gpd14nomiss$S14_2) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + 
                       as.factor(gpd14nomiss$S6) + gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$income.f +
                       gpd14nomiss$ed.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)

z14.2 <- summary(mlog14.2)$coefficients/summary(mlog14.2)$standard.errors
p14.2 <- (1 - pnorm(abs(z14.2), 0, 1)) * 2
p14.2


mlog14.3 <- multinom(gpd14nomiss$tot.aversion.bins ~ gpd14nomiss$tot.trust.idx + gpd14nomiss$know.pp.idx + gpd14nomiss$know.tech.idx +
                       as.factor(gpd14nomiss$S14_0) + as.factor(gpd14nomiss$S14_1) + as.factor(gpd14nomiss$S14_2) +
                       as.factor(gpd14nomiss$S14_3) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + as.factor(gpd14nomiss$S17) +
                       as.factor(gpd14nomiss$S6) + gpd14nomiss$race.f + gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$income.f +
                       gpd14nomiss$ed.f + gpd14nomiss$age.f + gpd14nomiss$gender.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)

z14.3 <- summary(mlog14.3)$coefficients/summary(mlog14.3)$standard.errors
p14.3 <- (1 - pnorm(abs(z14.3), 0, 1)) * 2
p14.3

mlog14.4 <- multinom(gpd14nomiss$tot.aversion.bins ~ gpd14nomiss$tot.trust.idx + gpd14nomiss$know.tech.idx + as.factor(gpd14nomiss$S14_1) + 
                       as.factor(gpd14nomiss$S14_2) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S6) + gpd14nomiss$married * gpd14nomiss$kids.f + 
                       gpd14nomiss$income.f + gpd14nomiss$ed.f + gpd14nomiss$age.f + gpd14nomiss$gender.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)

z14.4 <- summary(mlog14.4)$coefficients/summary(mlog14.4)$standard.errors
p14.4 <- (1 - pnorm(abs(z14.4), 0, 1)) * 2
p14.4


mlog14.5 <- multinom(gpd14nomiss$tot.trust.cl ~ gpd14nomiss$know.pp.idx + gpd14nomiss$know.tech.idx + gpd14nomiss$tot.aversion.idx +
                       as.factor(gpd14nomiss$S14_0) + as.factor(gpd14nomiss$S14_1) + as.factor(gpd14nomiss$S14_2) + 
                       as.factor(gpd14nomiss$S14_3) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + as.factor(gpd14nomiss$S17) +
                       as.factor(gpd14nomiss$S6) + gpd14nomiss$race.f + gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$income.f +
                       gpd14nomiss$ed.f + gpd14nomiss$age.f + gpd14nomiss$gender.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)


z14.5 <- summary(mlog14.5)$coefficients/summary(mlog14.5)$standard.errors
p14.5 <- (1 - pnorm(abs(z14.5), 0, 1)) * 2
p14.5

mlog14.6 <- multinom(gpd14nomiss$tot.trust.cl ~ gpd14nomiss$know.pp.idx + gpd14nomiss$know.tech.idx + gpd14nomiss$tot.aversion.idx +
                       as.factor(gpd14nomiss$S14_0) + as.factor(gpd14nomiss$S14_1) + as.factor(gpd14nomiss$S14_2) + 
                       as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + as.factor(gpd14nomiss$S17) +
                       as.factor(gpd14nomiss$S6) + gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$income.f +
                       gpd14nomiss$ed.f + gpd14nomiss$age.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)

z14.6 <- summary(mlog14.6)$coefficients/summary(mlog14.6)$standard.errors
p14.6 <- (1 - pnorm(abs(z14.6), 0, 1)) * 2
p14.6

mlog14.7 <- multinom(gpd14nomiss$tot.trust.bins ~ gpd14nomiss$know.pp.idx + gpd14nomiss$know.tech.idx + gpd14nomiss$tot.aversion.idx +
                       as.factor(gpd14nomiss$S14_0) + as.factor(gpd14nomiss$S14_1) + as.factor(gpd14nomiss$S14_2) + 
                       as.factor(gpd14nomiss$S14_3) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + as.factor(gpd14nomiss$S17) +
                       as.factor(gpd14nomiss$S6) + gpd14nomiss$race.f + gpd14nomiss$married * gpd14nomiss$kids.f + gpd14nomiss$income.f +
                       gpd14nomiss$ed.f + gpd14nomiss$age.f + gpd14nomiss$gender.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)


z14.7 <- summary(mlog14.7)$coefficients/summary(mlog14.7)$standard.errors
p14.7 <- (1 - pnorm(abs(z14.7), 0, 1)) * 2
p14.7

mlog14.8 <- multinom(gpd14nomiss$tot.trust.bins ~ gpd14nomiss$know.tech.idx + gpd14nomiss$tot.aversion.idx +
                       as.factor(gpd14nomiss$S14_0) + as.factor(gpd14nomiss$S14_2) + as.factor(gpd14nomiss$S1) + as.factor(gpd14nomiss$S5) + 
                       as.factor(gpd14nomiss$S17) + as.factor(gpd14nomiss$S6) + gpd14nomiss$race.f + gpd14nomiss$married * gpd14nomiss$kids.f + 
                       gpd14nomiss$income.f + gpd14nomiss$ed.f + gpd14nomiss$age.f, data = gpd14nomiss, weights = gpd14nomiss$Weight)


z14.8 <- summary(mlog14.8)$coefficients/summary(mlog14.8)$standard.errors
p14.8 <- (1 - pnorm(abs(z14.8), 0, 1)) * 2
p14.8
